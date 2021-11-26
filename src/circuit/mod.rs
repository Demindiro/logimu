mod aabb;
mod circuit_component;
mod direction;
mod ic;
mod point;
mod point_offset;
mod relative_aabb;
mod script;
mod wire;

pub use aabb::*;
pub use circuit_component::*;
pub use direction::*;
pub use ic::*;
pub use point::*;
pub use point_offset::*;
pub use relative_aabb::*;
pub use script::*;
pub use wire::*;

use super::simulator::{
	Component, Graph, GraphIter, GraphNodeHandle, InputType, NexusHandle, OutputType, Port,
	Program, Property, RemoveError, SetProperty,
};
use crate::arena::{Arena, Handle};
use core::{fmt, mem};
use serde::de;
use serde::ser::SerializeStruct;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashSet;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct WireHandle(Handle);

/// A collection of interconnected wires and components.
pub struct Circuit<C>
where
	C: CircuitComponent,
{
	/// All wires in this circuit.
	wires: Arena<(Wire, NexusHandle)>,
	/// A graph connecting all nodes. Used for IR generation.
	graph: Graph<C, (Point, Direction), Vec<WireHandle>>,
	/// The source of the attached script, if any.
	pub script_source: String,
}

impl<C> Circuit<C>
where
	C: CircuitComponent,
{
	/// Add a new wire to this circuit. The wire must not be zero-length.
	pub fn add_wire(&mut self, wire: Wire) {
		// Add the wire piece-wise, merging & splitting other wires as needed.
		let it = wire.segments();
		let (min, max) = wire.into();

		let ins_w = |slf: &mut Self, wire, nexus| {
			let h = WireHandle(slf.wires.insert((wire, nexus)));
			slf.graph.nexus_mut(nexus).unwrap().userdata.push(h);
		};
		let merge = |slf: &mut Self, keep, merge| {
			slf.graph
				.merge_nexuses(keep, merge, |kp, mrg| {
					mrg.iter().for_each(|&i| slf.wires[i.0].1 = keep);
					kp.extend(mrg);
				})
				.unwrap();
		};

		// Split the wires at the endpoints
		let mut nexuses = Vec::new();
		for p in [min, max] {
			// TODO avoid allocation
			for (w, h, nh) in self.wires(Aabb::new(p, p)).collect::<Vec<_>>() {
				(!nexuses.contains(&nh)).then(|| nexuses.push(nh));
				let (a, b) = w.into();
				if p != a && p != b {
					self.wires[h.0].0 = Wire::new(a, p);
					ins_w(self, Wire::new(p, b), nh);
				}
			}
		}

		// Merge nexuses
		let mut nexuses = nexuses.into_iter();
		let nexus = if let Some(nexus) = nexuses.next() {
			nexuses.for_each(|nh| merge(self, nexus, nh));
			nexus
		} else {
			self.graph.new_nexus(Default::default())
		};

		// Insert the wire, splitting it up & merging as necessary
		for wire in it {
			let (a, b) = wire.into();
			let mut merged = None;

			// Combine with nexus at A
			for (i, (w, h, nh)) in self.wire_endpoints(a).enumerate() {
				(i == 0 && merged.is_none()).then(|| merged = wire.merge(w).map(|w| (w, h)));
				(i > 0).then(|| merged = None);
				if nexus != nh {
					merge(self, nexus, nh);
					break;
				}
			}

			// Merge with the wire at A or add a new wire
			if let Some((w, h)) = merged {
				self.wires[h.0].0 = w;
			} else {
				ins_w(self, wire, nexus);
			}

			// Combine with nexus at B
			if let Some((.., nh)) = self.wire_endpoints(b).next() {
				(nexus != nh).then(|| merge(self, nexus, nh));
			}
		}

		// Merge with the last wire if possible
		self.merge_wires_at_point(max, |_, _| false);
	}

	pub fn remove_wires(&mut self, handles: &[WireHandle]) -> Result<(), RemoveWireError> {
		for &handle in handles {
			// Remove the wire
			let (w, nexus) = self
				.wires
				.remove(handle.0)
				.ok_or(RemoveWireError::InvalidHandle)?;
			let l = &mut self.graph.nexus_mut(nexus).unwrap().userdata;
			l.swap_remove(l.iter().position(|&h| h == handle).unwrap());

			// Remove the nexus if no other wires are part of it.
			if l.is_empty() {
				self.graph.remove_nexus(nexus).unwrap();
				continue;
			}

			// Merge any 2 wires at each endpoint _unless_ they are also wires to be removed.
			let (min, max) = w.into();
			let f = |a: &[_], e| a.contains(&e);
			let f = |a, b| !f(&[a, b], handle) && (f(handles, a) || f(handles, b));
			let a = self.merge_wires_at_point(min, f);
			let b = self.merge_wires_at_point(max, f);

			// Check if there is a potential nexus split...
			if !a || !b {
				continue;
			}

			// ... if so, walk one of the endpoints ...
			let mut visited = HashSet::<WireHandle>::default();
			let (mut rd, mut wr) = (Vec::from([min]), Vec::new());
			while !rd.is_empty() {
				for p in rd.drain(..) {
					for (w, h, _) in self.wire_endpoints(p) {
						let (min, max) = w.into();
						if visited.insert(h) {
							wr.push((min == p).then(|| max).unwrap_or(min));
						}
					}
				}
				mem::swap(&mut rd, &mut wr);
			}

			// ... and remove handles that are not found during the walk
			// and form a new nexus with them.
			let l = &mut self.graph.nexus_mut(nexus).unwrap().userdata;
			if l.len() == visited.len() {
				// The nexus is not disjoint
				continue;
			}
			let l = l
				.drain_filter(|h| !visited.contains(&h))
				.collect::<Vec<_>>();
			let h = self.graph.new_nexus(l);
			for w in self.graph.nexus_mut(h).unwrap().userdata.iter() {
				self.wires[w.0].1 = h;
			}
		}

		Ok(())
	}

	pub fn wire(&self, handle: WireHandle) -> Option<(Wire, NexusHandle)> {
		self.wires.get(handle.0).cloned()
	}

	pub fn wires(&self, aabb: Aabb) -> WireIter<C> {
		WireIter {
			iter: self.wires.iter(),
			aabb,
			_marker: std::marker::PhantomData,
		}
	}

	pub fn wire_endpoints(&self, point: Point) -> WireEndpointIter<C> {
		WireEndpointIter {
			iter: self.wires.iter(),
			point,
			_marker: std::marker::PhantomData,
		}
	}

	pub fn add_component(
		&mut self,
		component: C,
		position: Point,
		direction: Direction,
	) -> GraphNodeHandle {
		// Add to graph
		let handle = self.graph.add(component, (position, direction));

		// TODO add to zones. This requires per component AABBs.
		handle
	}

	pub fn remove_component(&mut self, handle: GraphNodeHandle) -> Result<(), RemoveError> {
		self.graph.remove(handle)
	}

	pub fn component(&self, handle: GraphNodeHandle) -> Option<(&C, Point, Direction)> {
		self.graph.get(handle).map(|(c, &(p, d))| (c, p, d))
	}

	pub fn component_mut(&mut self, handle: GraphNodeHandle) -> Option<(&mut C, Point, Direction)> {
		self.graph.get_mut(handle).map(|(c, &mut (p, d))| (c, p, d))
	}

	pub fn move_component(
		&mut self,
		handle: GraphNodeHandle,
		position: Point,
		direction: Direction,
	) -> Result<(), MoveError> {
		let (node, &(p, dir)) = self.graph.get(handle).ok_or(MoveError::InvalidHandle)?;
		let (ip, op) = (node.input_points(), node.output_points());

		for (port, &ip) in ip.iter().enumerate() {
			if let Some(ip) = p + dir * ip {
				for (_, &(w, _)) in self.wires.iter() {
					let (f, t): (Point, _) = w.into();
					if f == ip || t == ip {
						self.graph
							.connect(Port::Input { node: handle, port }, None)
							.unwrap();
					}
				}
			}
		}
		for (port, &op) in op.iter().enumerate() {
			if let Some(op) = p + dir * op {
				for (_, &(w, _)) in self.wires.iter() {
					let (f, t): (Point, _) = w.into();
					if f == op || t == op {
						self.graph
							.connect(Port::Output { node: handle, port }, None)
							.unwrap();
					}
				}
			}
		}

		let (_, (p, dir)) = self.graph.get_mut(handle).unwrap();
		(*p, *dir) = (position, direction);
		Ok(())
	}

	pub fn components(&self, aabb: Aabb) -> ComponentIter<C> {
		ComponentIter {
			iter: self.graph.nodes(),
			_circuit: self,
			_aabb: aabb,
			index: 0,
		}
	}

	// TODO make non-mutable
	pub fn generate_ir(&mut self) -> Program {
		self.connect_wire(None);
		self.graph.generate_ir()
	}

	fn find_ports_at_internal<'a, F, G>(
		&'a self,
		pos: Point,
		mut in_callback: F,
		mut out_callback: G,
	) where
		F: FnMut(GraphNodeHandle, usize),
		G: FnMut(GraphNodeHandle, usize),
	{
		//self.intersect_zone(position).find_ports_at(self, position, in_callback, out_callback);
		for (c, h, &(p, d)) in self.graph.nodes() {
			for (i, &inp) in c.input_points().iter().enumerate() {
				(p + d * inp).map(|inp| (inp == pos).then(|| in_callback(h, i)));
			}
			for (i, &outp) in c.output_points().iter().enumerate() {
				(p + d * outp).map(|outp| (outp == pos).then(|| out_callback(h, i)));
			}
		}
	}

	fn connect_wire(&mut self, _wire: Option<WireHandle>) {
		// TODO iterating all wires is wasteful.
		// Connect components using wire information
		for (_, &(w, nexus)) in self.wires.iter() {
			// TODO handle overlapping ports (i.e. ports without wire)
			let (f, t) = w.into();
			for p in [f, t].iter() {
				let (mut inp, mut outp) = (None, None);
				self.find_ports_at_internal(
					*p,
					|c, i| inp = Some((c, i)),
					|c, i| outp = Some((c, i)),
				);
				if let Some((node, port)) = inp {
					self.graph
						.connect(Port::Input { node, port }, Some(nexus))
						.unwrap();
				}
				if let Some((node, port)) = outp {
					self.graph
						.connect(Port::Output { node, port }, Some(nexus))
						.unwrap();
				}
			}
		}
	}

	/// Merge two wires at a given point if possible.
	///
	/// # Returns
	///
	/// `true` if wires endpoints were found, `false` otherwise.
	fn merge_wires_at_point<F>(&mut self, point: Point, exclude: F) -> bool
	where
		F: FnOnce(WireHandle, WireHandle) -> bool,
	{
		let mut it = self.wire_endpoints(point);
		match (it.next(), it.next(), it.next()) {
			(Some((aw, ah, _)), Some((bw, bh, _)), None) => {
				if exclude(ah, bh) {
					return false;
				}
				if let Some(w) = aw.merge(bw) {
					self.wires[ah.0].0 = w;
					let (_, nh) = self.wires.remove(bh.0).unwrap();
					let l = &mut self.graph.nexus_mut(nh).unwrap().userdata;
					l.swap_remove(l.iter().position(|&h| h == bh).unwrap());
				}
				true
			}
			(None, None, None) => false,
			_ => true,
		}
	}
}

impl<C> Default for Circuit<C>
where
	C: CircuitComponent,
{
	fn default() -> Self {
		Self {
			wires: Default::default(),
			graph: Graph::new(),
			script_source: Default::default(),
		}
	}
}

impl<C> Serialize for Circuit<C>
where
	C: CircuitComponent + Serialize,
{
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		let mut circuit = serializer.serialize_struct(stringify!(Circuit), 2)?;
		// TODO avoid redundant box
		circuit.serialize_field(
			"wires",
			&self.wires.iter().map(|(_, (w, _))| w).collect::<Box<_>>(),
		)?;
		circuit.serialize_field(
			"components",
			&self
				.graph
				.nodes()
				.map(|(c, _, (p, d))| (c, p, d))
				.collect::<Box<_>>(),
		)?;
		if !self.script_source.is_empty() {
			circuit.serialize_field("script", &self.script_source)?;
		}
		circuit.end()
	}
}

impl<'a, C> Deserialize<'a> for Circuit<C>
where
	C: CircuitComponent + Deserialize<'a>,
{
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'a>,
	{
		#[derive(Deserialize)]
		#[serde(field_identifier, rename_all = "lowercase")]
		enum Field {
			Wires,
			Components,
			Script,
		}

		struct CircuitVisitor<C>(core::marker::PhantomData<C>);

		impl<'a, C> de::Visitor<'a> for CircuitVisitor<C>
		where
			C: CircuitComponent + Deserialize<'a>,
		{
			type Value = Circuit<C>;

			fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
				formatter.write_str("struct Circuit")
			}

			fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
			where
				V: de::MapAccess<'a>,
			{
				let mut s = Circuit::default();
				let (mut handled_wires, mut handled_components) = (false, false);
				let mut handled_script = false;

				while let Some(key) = map.next_key()? {
					match key {
						Field::Wires => {
							if handled_wires {
								return Err(de::Error::duplicate_field("wires"));
							}
							handled_wires = true;
							for w in map.next_value::<Vec<Wire>>()? {
								s.add_wire(w);
							}
						}
						Field::Components => {
							if handled_components {
								return Err(de::Error::duplicate_field("wires"));
							}
							handled_components = true;
							for (c, p, d) in map.next_value::<Vec<(C, Point, Direction)>>()? {
								s.add_component(c, p, d);
							}
						}
						Field::Script => {
							if handled_script {
								Err(de::Error::duplicate_field("script"))?;
							}
							handled_script = true;
							s.script_source = map.next_value::<String>()?;
						}
					}
				}

				Ok(s)
			}
		}

		deserializer.deserialize_struct(
			stringify!(Circuit),
			&["wires", "components", "script"],
			CircuitVisitor(core::marker::PhantomData),
		)
	}
}

pub struct WireIter<'a, C>
where
	C: CircuitComponent,
{
	aabb: Aabb,
	iter: crate::arena::Iter<'a, (Wire, NexusHandle)>,
	_marker: std::marker::PhantomData<C>,
}

impl<'a, C> Iterator for WireIter<'a, C>
where
	C: CircuitComponent,
{
	type Item = (Wire, WireHandle, NexusHandle);

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			let (wh, &(w, nh)) = self.iter.next()?;
			let (f, t) = w.into();
			if self.aabb.intersect_line(f, t) {
				return Some((w, WireHandle(wh), nh));
			}
		}
	}
}

pub struct WireEndpointIter<'a, C>
where
	C: CircuitComponent,
{
	point: Point,
	iter: crate::arena::Iter<'a, (Wire, NexusHandle)>,
	_marker: std::marker::PhantomData<C>,
}

impl<'a, C> Iterator for WireEndpointIter<'a, C>
where
	C: CircuitComponent,
{
	type Item = (Wire, WireHandle, NexusHandle);

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			let (wh, &(w, nh)) = self.iter.next()?;
			let (f, t): (Point, _) = w.into();
			if f == self.point || t == self.point {
				return Some((w, WireHandle(wh), nh));
			}
		}
	}
}

pub struct ComponentIter<'a, C>
where
	C: CircuitComponent,
{
	// TODO avoid iter, use zones
	iter: GraphIter<'a, C, (Point, Direction)>,
	_circuit: &'a Circuit<C>,
	_aabb: Aabb,
	index: usize,
}

impl<'a, C> Iterator for ComponentIter<'a, C>
where
	C: CircuitComponent,
{
	type Item = (&'a C, Point, Direction, GraphNodeHandle);

	fn next(&mut self) -> Option<Self::Item> {
		// TODO check AABBs.
		while let Some((c, h, &(p, d))) = self.iter.next() {
			self.index += 1;
			return Some((c, p, d, h));
		}
		None
	}
}

#[derive(Debug)]
pub enum RemoveWireError {
	InvalidHandle,
}

#[derive(Debug)]
pub enum MoveError {
	InvalidHandle,
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::simulator::{
		ir, AndGate as And, In, NonZeroOneU8, NotGate as Not, OrGate as Or, Out, XorGate as Xor,
	};
	use core::num::NonZeroU8;
	use std::sync::Arc;

	/// ```
	/// i0 --+-------v
	///      |      AND --> NOT
	/// i1 --|--+----^       |
	///      |  |            v
	///      +--|----v      AND --> o0
	///      |  |    OR -----^
	///      |  +----^
	///      |  |
	///      +--|----v
	///         |   XOR ----------> o1
	///         +----^
	/// ```
	///
	/// NOT:
	/// ```
	/// I -> *0* -> O
	/// ```
	///
	/// AND/OR/XOR:
	/// ```
	/// I1 -> *--
	///       -0* -> O
	/// I2 -> *--
	/// ```
	#[test]
	fn manual_xor() {
		let mut circuit = Box::<Circuit<Box<dyn CircuitComponent>>>::default();

		let bits = NonZeroU8::new(4).unwrap();
		let inputs = NonZeroOneU8::new(2).unwrap();
		let i0 = Box::new(In::new("I0", bits, 0));
		let i1 = Box::new(In::new("I1", bits, 1));
		let l0 = Box::new(And::new(inputs));
		let l1 = Box::new(Not::new());
		let r0 = Box::new(Or::new(inputs));
		let lr = Box::new(And::new(inputs));
		let o0 = Box::new(Out::new("O0", bits, 0));
		let cp = Box::new(Xor::new(inputs));
		let o1 = Box::new(Out::new("O1", bits, 1));

		// Inputs
		circuit.add_component(i0, Point::new(0, 0), Direction::Right);
		circuit.add_component(i1, Point::new(0, 4), Direction::Right);

		// Connect inputs to AND
		circuit.add_wire(Wire::new(Point::new(0, 0), Point::new(3, 0)));
		circuit.add_wire(Wire::new(Point::new(0, 4), Point::new(3, 2)));
		// Place AND and NOT
		circuit.add_component(l0, Point::new(4, 1), Direction::Right);
		circuit.add_component(l1, Point::new(8, 0), Direction::Right);
		// Connect AND to NOT
		circuit.add_wire(Wire::new(Point::new(5, 1), Point::new(7, 0)));

		// Place OR
		circuit.add_component(r0, Point::new(4, 4), Direction::Right);
		// Connect inputs to OR
		circuit.add_wire(Wire::new(Point::new(0, 0), Point::new(3, 3)));
		circuit.add_wire(Wire::new(Point::new(0, 4), Point::new(3, 5)));

		// Connect AND & OR to AND and connect AND to output
		circuit.add_wire(Wire::new(Point::new(9, 0), Point::new(11, 0)));
		circuit.add_wire(Wire::new(Point::new(5, 4), Point::new(11, 2)));
		circuit.add_wire(Wire::new(Point::new(13, 1), Point::new(16, 0)));
		// Place AND and output
		circuit.add_component(lr, Point::new(12, 1), Direction::Right);
		circuit.add_component(o0, Point::new(16, 0), Direction::Right);

		// Place XOR and output
		circuit.add_component(cp, Point::new(4, 8), Direction::Right);
		circuit.add_component(o1, Point::new(16, 8), Direction::Right);
		// Connect inputs to XOR and XOR to output
		circuit.add_wire(Wire::new(Point::new(0, 0), Point::new(3, 7)));
		circuit.add_wire(Wire::new(Point::new(0, 4), Point::new(3, 9)));
		circuit.add_wire(Wire::new(Point::new(5, 8), Point::new(16, 8)));

		let mut state = Arc::new(circuit.generate_ir()).new_state();
		let (a, b) = (0b1100, 0b0110);
		state.write_inputs(&[ir::Value::Set(a), ir::Value::Set(b)]);
		state.run(1024);
		let mut out = [ir::Value::Floating; 2];
		state.read_outputs(&mut out);
		assert_eq!(out, [ir::Value::Set(a ^ b); 2]);
	}
}
