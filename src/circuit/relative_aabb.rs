use super::{Direction, PointOffset};
use core::ops::Mul;

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
