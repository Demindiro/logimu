mod ic;

pub use ic::*;

use super::simulator;
use super::simulator::{
	ir::IrOp, Component, Graph, GraphIter, GraphNodeHandle, InputType, NexusHandle, OutputType,
	Port, Property, RemoveError, SetProperty,
};
use crate::arena::{Arena, Handle};
use crate::impl_dyn;

use core::cmp::Ordering;
use core::fmt;
use core::mem;
use core::ops::{Add, Mul};
use serde::de;
use serde::ser::{SerializeSeq, SerializeStruct, SerializeTuple};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::error::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, Serialize, Deserialize)]
pub struct Point {
	pub x: u16,
	pub y: u16,
}

impl Point {
	pub const MIN: Self = Self { x: u16::MIN, y: u16::MIN };
	pub const MAX: Self = Self { x: u16::MAX, y: u16::MAX };

	pub const fn new(x: u16, y: u16) -> Self {
		Self { x, y }
	}
}

impl PartialOrd for Point {
	/// The `y` coordinate has precedence over the `x` coordinate.
	fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
		Some(match self.y.cmp(&rhs.y) {
			Ordering::Equal => self.x.cmp(&rhs.x),
			o => o,
		})
	}
}

#[derive(Clone, Copy, Debug)]
pub struct PointOffset {
	pub x: i8,
	pub y: i8,
}

impl PointOffset {
	pub const MIN: Self = Self { x: i8::MIN, y: i8::MIN };
	pub const MAX: Self = Self { x: i8::MAX, y: i8::MAX };

	pub const fn new(x: i8, y: i8) -> Self {
		Self { x, y }
	}
}

impl Add<PointOffset> for Point {
	type Output = Option<Self>;

	fn add(self, rhs: PointOffset) -> Self::Output {
		let x = i32::from(self.x) + i32::from(rhs.x);
		let y = i32::from(self.y) + i32::from(rhs.y);
		x.try_into()
			.and_then(|x| y.try_into().map(|y| Self { x, y }))
			.ok()
	}
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum Direction {
	Right,
	Down,
	Left,
	Up,
}

impl Direction {
	pub fn rotate_clockwise(self) -> Self {
		match self {
			Self::Right => Self::Down,
			Self::Down => Self::Left,
			Self::Left => Self::Up,
			Self::Up => Self::Right,
		}
	}
}

impl Mul<PointOffset> for Direction {
	type Output = PointOffset;

	fn mul(self, rhs: PointOffset) -> Self::Output {
		match self {
			Self::Right => PointOffset::new(rhs.x, rhs.y),
			Self::Down => PointOffset::new(-rhs.y, rhs.x),
			Self::Left => PointOffset::new(-rhs.x, -rhs.y),
			Self::Up => PointOffset::new(rhs.y, -rhs.x),
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub struct Aabb {
	min: Point,
	max: Point,
}

impl Aabb {
	pub const ALL: Self = Self { min: Point::MIN, max: Point::MAX };

	/// Create a new AABB containing two points as tightly as possible.
	pub fn new(p1: Point, p2: Point) -> Self {
		Self {
			min: Point { x: p1.x.min(p2.x), y: p1.y.min(p2.y) },
			max: Point { x: p1.x.max(p2.x), y: p1.y.max(p2.y) },
		}
	}

	/// Check if this AABB contains a point.
	pub fn intersect_point(&self, p: Point) -> bool {
		self.min.x <= p.x && p.x <= self.max.x && self.min.y <= p.y && p.y <= self.max.y
	}
}
#[derive(Clone, Copy, Debug)]
pub struct RelativeAabb {
	pub min: PointOffset,
	pub max: PointOffset,
}

impl RelativeAabb {
	/// Create a new AABB containing two points as tightly as possible.
	pub fn new(p1: PointOffset, p2: PointOffset) -> Self {
		Self {
			min: PointOffset { x: p1.x.min(p2.x), y: p1.y.min(p2.y) },
			max: PointOffset { x: p1.x.max(p2.x), y: p1.y.max(p2.y) },
		}
	}

	/// Create an AABB containing both this AABB and the given point.
	pub fn expand(mut self, p: PointOffset) -> Self {
		self.min.x = self.min.x.min(p.x);
		self.min.y = self.min.y.min(p.y);
		self.max.x = self.max.x.max(p.x);
		self.max.y = self.max.y.max(p.y);
		self
	}
}

impl Mul<RelativeAabb> for Direction {
	type Output = RelativeAabb;

	fn mul(self, rhs: RelativeAabb) -> Self::Output {
		RelativeAabb::new(self * rhs.min, self * rhs.max)
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Wire {
	pub from: Point,
	pub to: Point,
}

impl Wire {
	pub fn new(from: Point, to: Point) -> Self {
		Self { from, to }
	}

	/// Check if this wire intersects with a point.
	pub fn intersect_point(&self, point: Point) -> bool {
		// Check if the point is inside the AABB of the wire.
		if !self.aabb().intersect_point(point) {
			return false;
		}

		let (x1, y1) = (i32::from(self.from.x), i32::from(self.from.y));
		let (x2, y2) = (i32::from(self.to.x), i32::from(self.to.y));
		let (xp, yp) = (i32::from(point.x), i32::from(point.y));
		// Make start of line (x1, y1) the origin so b = 0
		let (dx, dy) = (x2 - x1, y2 - y1);
		let (dxp, dyp) = (xp - x1, yp - y1);
		// y = ax <=> y = dy / dx * x <=> y * dx = x * dx
		dx * dyp == dy * dxp
	}

	/// Return the AABB enclosing this wire.
	pub fn aabb(&self) -> Aabb {
		Aabb::new(self.from, self.to)
	}
}

/// A component with fixed input & output locations
#[typetag::serde]
pub trait CircuitComponent
where
	Self: simulator::Component,
{
	/// All the inputs of this component.
	fn inputs(&self) -> Box<[PointOffset]>;

	/// All the outputs of this component.
	fn outputs(&self) -> Box<[PointOffset]>;

	fn external_input(&self) -> Option<usize> {
		None
	}

	fn external_output(&self) -> Option<usize> {
		None
	}

	fn aabb(&self, direction: Direction) -> RelativeAabb;
}

impl_dyn! {
	Component for Box<dyn CircuitComponent> {
		ref input_count() -> usize;
		ref input_type(input: usize) -> Option<InputType>;
		ref output_count() -> usize;
		ref output_type(output: usize) -> Option<OutputType>;
		ref generate_ir(inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp), ms: usize) -> usize;
		ref properties() -> Box<[Property]>;
		mut set_property(name: &str, value: SetProperty) -> Result<(), Box<dyn Error>>;
	}
}

impl_dyn! {
	CircuitComponent for Box<dyn CircuitComponent> {
		ref inputs() -> Box<[PointOffset]>;
		ref outputs() -> Box<[PointOffset]>;
		ref external_input() -> Option<usize>;
		ref external_output() -> Option<usize>;
		ref aabb(dir: Direction) -> RelativeAabb;
		ref typetag_name() -> &'static str;
		ref typetag_deserialize() -> ();
	}
}

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
}

impl<C> Circuit<C>
where
	C: CircuitComponent,
{
	pub fn new() -> Self {
		Self::default()
	}

	pub fn add_wire(&mut self, wire: Wire) -> WireHandle {
		let Aabb { min, max } = wire.aabb();

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

		handle
	}

	pub fn remove_wire(&mut self, handle: WireHandle) -> Result<(), &'static str> {
		let (wire, nexus) = self.wires.remove(handle.0).ok_or("invalid handle")?;

		// Remove from zones.
		let Aabb { min, max } = wire.aabb();

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

	pub fn components(&self, aabb: Aabb) -> ComponentIter<C> {
		ComponentIter { iter: self.graph.nodes(), circuit: self, aabb, index: 0 }
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
			for (i, &inp) in c.inputs().iter().enumerate() {
				(p + d * inp).map(|inp| (inp == pos).then(|| in_callback(h, i)));
			}
			for (i, &outp) in c.outputs().iter().enumerate() {
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

	fn connect_wire(&mut self, wire: Option<WireHandle>) {
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
						.connect(Port::Input { node, port }, *nexus)
						.unwrap();
				}
				if let Some((node, port)) = outp {
					self.graph
						.connect(Port::Output { node, port }, *nexus)
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
		Self { wires: Default::default(), graph: Graph::new() }
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

				while let Some(key) = map.next_key()? {
					match key {
						Field::Wires => {
							if handled_wires {
								return Err(de::Error::duplicate_field("wires"));
							}
							handled_wires = true;
							for w in map.next_value::<Vec<Wire>>()?.into_iter() {
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
					}
				}

				Ok(s)
			}
		}

		deserializer.deserialize_struct(
			stringify!(Circuit),
			&["wires", "components"],
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
	circuit: &'a Circuit<C>,
	aabb: Aabb,
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

#[cfg(test)]
mod test {
	use super::*;
	use core::num::NonZeroU8;
	use simulator::{
		AndGate as And, In, NonZeroOneU8, NotGate as Not, OrGate as Or, Out, XorGate as Xor,
	};

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
		let l0 = Box::new(And::new(inputs, bits));
		let l1 = Box::new(Not::new(bits));
		let r0 = Box::new(Or::new(inputs, bits));
		let lr = Box::new(And::new(inputs, bits));
		let o0 = Box::new(Out::new("O0", bits, 0));
		let cp = Box::new(Xor::new(inputs, bits));
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
		simulator::ir::interpreter::run(&ir, &mut [0; 32], &[a, b], &mut out);
		assert_eq!(out, [a ^ b; 2]);
	}
}
