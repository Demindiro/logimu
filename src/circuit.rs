use super::simulator::Component;

use core::mem;
use core::ops::{Add, Mul};
use std::rc::Rc;

#[derive(Clone, Copy, Debug)]
pub struct Point {
	pub x: u16,
	pub y: u16,
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
pub trait CircuitComponent {
	/// All the inputs of this component.
	fn inputs(&self) -> &[PointOffset];

	/// All the outputs of this component.
	fn outputs(&self) -> &[PointOffset];
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
	wires: Vec<Wire>,
	/// All components in this circuit.
	components: Vec<(C, Point, Direction)>,
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

	pub fn add_wire(&mut self, wire: Wire) -> usize {
		let Aabb { min, max } = wire.aabb();

		let index = self.wires.len();
		self.wires.push(wire);

		let (min_x, min_y) = (min.x / 64, min.y / 64);
		// Round down, then count up to max including max so zero-width/height
		// wires are visible.
		let (max_x, max_y) = (max.x / 64, max.y / 64);
		for y in min_y..=max_y {
			for x in min_x..=max_x {
				self.zones[usize::from(y)][usize::from(x)].add_wire(index);
			}
		}

		index
	}

	pub fn wires(&self, aabb: Aabb) -> WireIter<C> {
		WireIter {
			circuit: self,
			aabb,
			index: 0,
		}
	}

	pub fn add_component(&mut self, component: C, position: Point, direction: Direction) -> usize {
		let index = self.components.len();
		self.components.push((component, position, direction));
		// TODO add to zones. This requires per component AABBs.
		index
	}

	pub fn components(&self, aabb: Aabb) -> ComponentIter<C> {
		ComponentIter {
			circuit: self,
			aabb,
			index: 0,
		}
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
			components: Vec::new(),
		}
	}
}

impl Zone {
	const COMPONENT_FLAG: usize = 1 << (mem::size_of::<usize>() - 1);

	/// Get all wires and components at a given point.
	pub fn intersect_point<C>(
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
			if n & !Self::COMPONENT_FLAG == 0 {
				// Wire
				circuit.wires[n].intersect_point(position).then(|| wire_callback(n));
			} else {
				// Component
				let n = n ^ Self::COMPONENT_FLAG;
				todo!();
			}
		}
	}

	pub fn add_wire(&mut self, index: usize) {
		assert_eq!(index & Self::COMPONENT_FLAG, 0);
		self.nodes.push(index);
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
		while let Some(w) = self.circuit.wires.get(self.index) {
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
		while let Some(c) = self.circuit.components.get(self.index) {
			self.index += 1;
			return Some((&c.0, c.1, c.2));
		}
		None
	}
}

#[cfg(test)]
mod test {
	use super::*;


}
