use super::{Aabb, Point};
use serde::{Deserialize, Serialize};

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
