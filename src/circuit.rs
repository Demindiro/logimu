use crate::impl_dyn;
use super::simulator;
use super::simulator::{Component, InputType, OutputType, ir::IrOp, Graph, GraphNodeHandle, GraphIter, NexusHandle, Port};

use core::mem;
use core::ops::{Add, Mul};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Point {
	pub x: u16,
	pub y: u16,
}

impl Point {
	pub const fn new(x: u16, y: u16) -> Self {
		Self { x, y }
	}
}

#[derive(Clone, Copy, Debug)]
pub struct PointOffset {
	pub x: i8,
	pub y: i8,
}

impl PointOffset {
	pub const fn new(x: i8, y: i8) -> Self {
		Self { x, y }
	}
}

impl Add<PointOffset> for Point {
	type Output = Option<Self>;

	fn add(self, rhs: PointOffset) -> Self::Output {
		let x = i32::from(self.x) + i32::from(rhs.x);
		let y = i32::from(self.y) + i32::from(rhs.y);
		x.try_into().and_then(|x| y.try_into().map(|y| Self { x, y })).ok()
	}
}

#[derive(Clone, Copy, Debug)]
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
	type Output = Option<PointOffset>;

	fn mul(self, rhs: PointOffset) -> Self::Output {
		let (x, y) = match self {
			Self::Right => (rhs.x, rhs.y),
			Self::Down => (rhs.y.checked_neg()?, rhs.x),
			Self::Left => (rhs.x.checked_neg()?, rhs.y.checked_neg()?),
			Self::Up => (rhs.y, rhs.x.checked_neg()?),
		};
		Some(PointOffset { x, y })
	}
}

#[derive(Clone, Copy, Debug)]
pub struct Aabb {
	min: Point,
	max: Point,
}

impl Aabb {
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

#[derive(Clone, Debug)]
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

		// y = ax + b
		// a = dy / dx = (y2 - y1) / (x2 - x1)
		// b = y1 - ax1
		let (x1, y1) = (i32::from(self.from.x), i32::from(self.from.y));
		let (x2, y2) = (i32::from(self.to.x), i32::from(self.to.y));
		let (xp, yp) = (i32::from(point.x), i32::from(point.y));
		let (dx, dy) = (x2 - x1, y2 - y1);
		// Ensure we can divide by dx
		if dx != 0 {
			let a = dy / dx;
			let b = y1 - a * x1;
			// p in wire iff yp = a*xp + b
			yp == a * xp + b
		} else {
			// wire is straight along y axis (=> x1 == x2), so just check if xp == x1
			xp == x1
		}
	}

	/// Return the AABB enclosing this wire.
	pub fn aabb(&self) -> Aabb {
		Aabb::new(self.from, self.to)
	}
}

/// A component with fixed input & output locations
pub trait CircuitComponent
where
	Self: simulator::Component,
{
	/// All the inputs of this component.
	fn inputs(&self) -> &[PointOffset];

	/// All the outputs of this component.
	fn outputs(&self) -> &[PointOffset];
}

impl_dyn! {
	Component for &dyn CircuitComponent {
		input_count() -> usize;
		input_type(input: usize) -> Option<InputType>;
		output_count() -> usize;
		output_type(output: usize) -> Option<OutputType>;
		generate_ir(inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp)) -> ();
	}
}

impl_dyn! {
	CircuitComponent for &dyn CircuitComponent {
		inputs() -> &[PointOffset];
		outputs() -> &[PointOffset];
	}
}

/// A collection of interconnected wires and components.
pub struct Circuit<C>
where
	C: CircuitComponent,
{
	/// A grid is used to speed up intersection lookups.
	///
	/// To save on memory, the grid is split into zones.
	///
	/// - Root zone: 1024x1024 subzones -> 24 MiB memory on 64-bit.
	/// - Subzone: 64x64 points. Memory usage depends on amount of nodes (components & wires) in zone.
	zones: [[Zone; 1024]; 1024],
	/// All wires in this circuit.
	wires: Vec<(Wire, NexusHandle)>,
	/// A graph connecting all nodes. Used for IR generation.
	graph: Graph<C, (Point, Direction), Vec<usize>>,
}

/// A single zone in a circuit.
pub struct Zone {
	/// A mapping from point to component or wire.
	///
	/// - If MSB is 0, maps to wire.
	/// - If MSB is 1, maps to component.
	nodes: Vec<usize>,
}

impl<C> Circuit<C>
where
	C: CircuitComponent,
{
	pub fn new() -> Self {
		Self::default()
	}

	pub fn add_wire(&mut self, wire: Wire) {
		let Aabb { min, max } = wire.aabb();
		let index = self.wires.len();

		let mut nexus = None;
		self.intersect_point(wire.from, |i| nexus = Some(self.wires[i].1), |_| todo!());
		self.intersect_point(wire.to, |i| {
			nexus.is_some().then(|| todo!("handle connecting two separate wires with new wire"));
			nexus = Some(self.wires[i].1);
		}, |_| todo!());

		let nexus = nexus.unwrap_or_else(|| self.graph.new_nexus(Vec::new()));
		self.graph.nexus_mut(nexus).unwrap().userdata.push(index);

		self.wires.push((wire, nexus));

		let (min_x, min_y) = (min.x / 64, min.y / 64);
		// Round down, then count up to max including max so zero-width/height
		// wires are visible.
		let (max_x, max_y) = (max.x / 64, max.y / 64);
		for y in min_y..=max_y {
			for x in min_x..=max_x {
				self.zones[usize::from(y)][usize::from(x)].add_wire(index);
			}
		}
	}

	pub fn wires(&self, aabb: Aabb) -> WireIter<C> {
		WireIter {
			circuit: self,
			aabb,
			index: 0,
		}
	}

	pub fn add_component(&mut self, component: C, position: Point, direction: Direction) -> usize {
		// Add to graph
		let handle = self.graph.add(component, (position, direction));
		assert_eq!(handle.into_raw() & (1 << mem::size_of_val(&handle.into_raw())), 0);

		// TODO add to zones. This requires per component AABBs.
		handle.into_raw()
	}

	pub fn components(&self, aabb: Aabb) -> ComponentIter<C> {
		ComponentIter {
			iter: self.graph.nodes(),
			circuit: self,
			aabb,
			index: 0,
		}
	}

	pub fn generate_ir(&mut self) -> (Vec<IrOp>, usize) {
		// Connect components using wire information
		for (w, nexus) in self.wires.iter() {
			// TODO handle overlapping ports (i.e. ports without wire)
			for p in [w.from, w.to].iter() {
				let (mut inp, mut outp) = (None, None);
				self.find_ports_at_internal(*p, |c, i| inp = Some((c, i)), |c, i| outp = Some((c, i)));
				if let Some((node, port)) = inp {
					self.graph.connect(Port::Input { node, port }, *nexus).unwrap();
				}
				if let Some((node, port)) = outp {
					self.graph.connect(Port::Output { node, port }, *nexus).unwrap();
				}
			}
		}

		let (ir, mem_size) = self.graph.generate_ir();

		(ir, mem_size)
	}

	fn find_ports_at_internal<'a, F, G>(&'a self, pos: Point, mut in_callback: F, mut out_callback: G)
	where
		F: FnMut(GraphNodeHandle, usize),
		G: FnMut(GraphNodeHandle, usize),
	{
		//self.intersect_zone(position).find_ports_at(self, position, in_callback, out_callback);
		for (c, h, &(p, d)) in self.graph.nodes() {
			for (i, &inp) in c.inputs().iter().enumerate() {
				(d * inp)
					.and_then(|inp| p + inp)
					.map(|inp| (inp == pos).then(|| in_callback(h, i)));
			}
			for (i, &outp) in c.outputs().iter().enumerate() {
				(d * outp)
					.and_then(|outp| p + outp)
					.map(|outp| (outp == pos).then(|| out_callback(h, i)));
			}
		}
	}

	fn intersect_zone<'a>(&'a self, position: Point) -> &'a Zone {
		let (x, y) = (usize::from(position.x) / 64, usize::from(position.y) / 64);
		&self.zones[y][x]
	}

	fn intersect_point(&self, position: Point, wire_callback: impl FnMut(usize), component_callback: impl FnMut(usize)) {
		self.intersect_zone(position).intersect_point(self, position, wire_callback, component_callback);
	}
}

impl<C> Default for Circuit<C>
where
	C: CircuitComponent,
{
	fn default() -> Self {
		const ZONE: Zone = Zone { nodes: Vec::new() };
		const ARRAY: [Zone; 1024] = [ZONE; 1024];
		const MATRIX: [[Zone; 1024]; 1024] = [ARRAY; 1024];
		Self {
			zones: MATRIX,
			wires: Vec::new(),
			graph: Graph::new(),
		}
	}
}

impl Zone {
	const COMPONENT_FLAG: usize = 1 << (mem::size_of::<usize>() - 1);

	/// Get all wires and components at a given point.
	fn intersect_point<C>(
		&self,
		circuit: &Circuit<C>,
		position: Point,
		mut wire_callback: impl FnMut(usize),
		mut component_callback: impl FnMut(usize),
	)
	where
		C: CircuitComponent,
	{
		for &n in self.nodes.iter() {
			if n & Self::COMPONENT_FLAG == 0 {
				// Wire
				circuit.wires[n].0.intersect_point(position).then(|| wire_callback(n));
			} else {
				// Component
				let n = n ^ Self::COMPONENT_FLAG;
				todo!();
			}
		}
	}

	fn add_wire(&mut self, index: usize) {
		assert_eq!(index & Self::COMPONENT_FLAG, 0);
		self.nodes.push(index);
	}

	fn find_ports_at<'a, F, C>(&self, circuit: &'a Circuit<C>, position: Point, mut in_callback: F, mut out_callback: F)
	where
		F: FnMut(&'a C, usize),
		C: CircuitComponent,
	{
		todo!()
	}
}

pub struct WireIter<'a, C>
where
	C: CircuitComponent,
{
	circuit: &'a Circuit<C>,
	aabb: Aabb,
	index: usize,
}

impl<'a, C> Iterator for WireIter<'a, C>
where
	C: CircuitComponent,
{
	type Item = &'a Wire;

	fn next(&mut self) -> Option<Self::Item> {
		while let Some((w, _)) = self.circuit.wires.get(self.index) {
			self.index += 1;
			if self.aabb.intersect_point(w.from) || self.aabb.intersect_point(w.to) {
				return Some(w);
			}
		}
		None
	}
}

pub struct ComponentIter<'a, C>
where
	C: CircuitComponent,
{
	// TODO avoid iter, use zones
	iter: GraphIter<'a, C, (Point, Direction), Vec<usize>>,
	circuit: &'a Circuit<C>,
	aabb: Aabb,
	index: usize,
}

impl<'a, C> Iterator for ComponentIter<'a, C>
where
	C: CircuitComponent,
{
	type Item = (&'a C, Point, Direction);

	fn next(&mut self) -> Option<Self::Item> {
		// TODO check AABBs.
		while let Some((c, _, &(p, d))) = self.iter.next() {
			self.index += 1;
			return Some((c, p, d));
		}
		None
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use core::num::NonZeroU8;
	use simulator::{In, Out, AndGate as And, OrGate as Or, NotGate as Not, XorGate as Xor, NonZeroOneU8};

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

		let mut circuit = Box::<Circuit<&dyn CircuitComponent>>::default();

		let bits = NonZeroU8::new(1).unwrap();
		let inputs = NonZeroOneU8::new(2).unwrap();
		let i0 = In::new(bits, 0);
		let i1 = In::new(bits, 1);
		let l0 = And::new(inputs, bits);
		let l1 = Not::new(bits);
		let r0 = Or::new(inputs, bits);
		let lr = And::new(inputs, bits);
		let o0 = Out::new(bits, 0);
		let cp = Xor::new(inputs, bits);
		let o1 = Out::new(bits, 1);

		// Inputs
		circuit.add_component(&i0, Point::new(0, 0), Direction::Right);
		circuit.add_component(&i1, Point::new(0, 4), Direction::Right);

		// Connect inputs to AND
		circuit.add_wire(Wire::new(Point::new(0, 0), Point::new(3, 0)));
		circuit.add_wire(Wire::new(Point::new(0, 4), Point::new(3, 2)));
		// Place AND and NOT
		circuit.add_component(&l0, Point::new(4, 1), Direction::Right);
		circuit.add_component(&l1, Point::new(8, 0), Direction::Right);
		// Connect AND to NOT
		circuit.add_wire(Wire::new(Point::new(5, 1), Point::new(7, 0)));

		// Place OR
		circuit.add_component(&r0, Point::new(4, 4), Direction::Right);
		// Connect inputs to OR
		circuit.add_wire(Wire::new(Point::new(0, 0), Point::new(3, 3)));
		circuit.add_wire(Wire::new(Point::new(0, 4), Point::new(3, 5)));
		
		// Connect AND & OR to AND and connect AND to output
		circuit.add_wire(Wire::new(Point::new(9, 0), Point::new(11, 0)));
		circuit.add_wire(Wire::new(Point::new(5, 4), Point::new(11, 2)));
		circuit.add_wire(Wire::new(Point::new(13, 1), Point::new(16, 0)));
		// Place AND and output
		circuit.add_component(&lr, Point::new(12, 1), Direction::Right);
		circuit.add_component(&o0, Point::new(16, 0), Direction::Right);

		// Place XOR and output
		circuit.add_component(&cp, Point::new(4, 8), Direction::Right);
		circuit.add_component(&o1, Point::new(16, 8), Direction::Right);
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
