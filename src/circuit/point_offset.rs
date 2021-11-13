use super::{Direction, Point};
use core::ops::{Add, Mul};

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
