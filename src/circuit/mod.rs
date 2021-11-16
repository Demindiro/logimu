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
	ir::IrOp, Component, Graph, GraphIter, GraphNodeHandle, InputType, NexusHandle, OutputType,
	Port, Property, RemoveError, SetProperty,
};
use crate::arena::{Arena, Handle};

use core::fmt;

use serde::de;
use serde::ser::SerializeStruct;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
	pub fn add_wire(&mut self, wire: Wire) -> Option<WireHandle> {
		if wire.from == wire.to {
			return None;
		}

		// Add wire to existing nexus if it connects with one.
		// Otherwise create a new nexus and add the wire to it.
		let mut nexus = None;
		// TODO: avoid allocating here (Rust borrowing the entirety of self sucks :( )
		let mut intersecting_wires = Vec::new();
		self.intersect_point(wire.from, |i| intersecting_wires.push(i), |_| todo!());
		self.intersect_point(wire.to, |i| intersecting_wires.push(i), |_| todo!());

		for i in intersecting_wires {
			let n = self.wires[i.0].1;
			if let Some(nexus) = nexus {
				if nexus != n {
					self.graph
						.merge_nexuses(nexus, n, |keep, merge| {
							merge.iter().for_each(|&i| self.wires[i.0].1 = nexus);
							keep.extend(merge);
						})
						.unwrap();
				}
			} else {
				nexus = Some(n);
			}
		}

		let nexus = nexus.unwrap_or_else(|| self.graph.new_nexus(Vec::new()));
		let handle = WireHandle(self.wires.insert((wire, nexus)));
		self.graph.nexus_mut(nexus).unwrap().userdata.push(handle);

		// Check if this wire connects with any components. If so, connect these components
		// to this wire's nexus.
		self.connect_wire(Some(handle));

		Some(handle)
	}

	pub fn remove_wire(&mut self, handle: WireHandle) -> Result<(), &'static str> {
		let (_, nexus) = self.wires.remove(handle.0).ok_or("invalid handle")?;

		// Remove from nexus.
		let list = &mut self.graph.nexus_mut(nexus).unwrap().userdata;
		list.remove(list.iter().position(|e| *e == handle).unwrap());

		// Remove nexus if it no longer has any wires.
		if list.is_empty() {
			self.graph.remove_nexus(nexus).unwrap();
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
				for (_, (w, _)) in self.wires.iter() {
					if w.from == ip || w.to == ip {
						self.graph
							.connect(Port::Input { node: handle, port }, None)
							.unwrap();
					}
				}
			}
		}
		for (port, &op) in op.iter().enumerate() {
			if let Some(op) = p + dir * op {
				for (_, (w, _)) in self.wires.iter() {
					if w.from == op || w.to == op {
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
	pub fn generate_ir(&mut self) -> (Vec<IrOp>, usize) {
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

	fn intersect_point(
		&self,
		position: Point,
		mut wire_callback: impl FnMut(WireHandle),
		_component_callback: impl FnMut(usize),
	) {
		for (h, (w, ..)) in self.wires.iter() {
			w.intersect_point(position)
				.then(|| wire_callback(WireHandle(h)));
		}
	}

	fn connect_wire(&mut self, _wire: Option<WireHandle>) {
		// TODO iterating all wires is wasteful.
		// Connect components using wire information
		for (_, (w, nexus)) in self.wires.iter() {
			// TODO handle overlapping ports (i.e. ports without wire)
			for p in [w.from, w.to].iter() {
				let (mut inp, mut outp) = (None, None);
				self.find_ports_at_internal(
					*p,
					|c, i| inp = Some((c, i)),
					|c, i| outp = Some((c, i)),
				);
				if let Some((node, port)) = inp {
					self.graph
						.connect(Port::Input { node, port }, Some(*nexus))
						.unwrap();
				}
				if let Some((node, port)) = outp {
					self.graph
						.connect(Port::Output { node, port }, Some(*nexus))
						.unwrap();
				}
			}
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
	type Item = (&'a Wire, WireHandle, NexusHandle);

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			let (wh, (w, nh)) = self.iter.next()?;
			if self.aabb.intersect_point(w.from) || self.aabb.intersect_point(w.to) {
				return Some((w, WireHandle(wh), *nh));
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
pub enum MoveError {
	InvalidHandle,
}

#[cfg(test)]
mod test {
	use super::*;
	use crate::simulator::{
		ir::interpreter, AndGate as And, In, NonZeroOneU8, NotGate as Not, OrGate as Or, Out,
		XorGate as Xor,
	};
	use core::num::NonZeroU8;

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

		let (ir, _) = circuit.generate_ir();
		let (a, b) = (0b1100, 0b0110);
		let mut out = [0; 2];
		interpreter::run(&ir, &mut [0; 32], &[a, b], &mut out);
		assert_eq!(out, [a ^ b; 2]);
	}
}
