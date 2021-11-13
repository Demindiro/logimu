use super::*;

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
			min: Point::new(p1.x.min(p2.x), p1.y.min(p2.y)),
			max: Point::new(p1.x.max(p2.x), p1.y.max(p2.y)),
		}
	}

	/// Check if this AABB contains a point.
	pub fn intersect_point(&self, p: Point) -> bool {
		self.min.x <= p.x && p.x <= self.max.x && self.min.y <= p.y && p.y <= self.max.y
	}

	#[allow(dead_code)]
	pub fn min(&self) -> Point {
		self.min
	}

	#[allow(dead_code)]
	pub fn max(&self) -> Point {
		self.max
	}
}
