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

	/// Check if a line goes through this AABB.
	pub fn intersect_line(&self, a: Point, b: Point) -> bool {
		let Point { x: lx, y: ly } = self.min;
		let Point { x: ux, y: uy } = self.max;

		// Check if a and b are on the same side of the rectangle.
		// If so, the line cannot intersect the rectangle.
		let f = |k, l, c, d| (k < c && l < c) || (k > d && l > d);
		if f(a.x, b.x, lx, ux) || f(a.y, b.y, ly, uy) {
			return false;
		}

		// If all corners of the rectangle are on one side of the line,
		// there is no intersection.
		let (ax, ay) = (i32::from(a.x), i32::from(a.y));
		let (bx, by) = (i32::from(b.x), i32::from(b.y));
		let (dx, dy) = (bx - ax, by - ay);
		// y ? (dy / dx) * x + c <=> (y - s) * dx ? (x - r) * dy
		let f = |x, y| ((i32::from(y) - ay) * dx - (i32::from(x) - ax) * dy).signum();
		(f(lx, ly) + f(lx, uy) + f(ux, uy) + f(ux, ly)).abs() != 4
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
