use super::{Direction, Point};
use core::ops::{Add, Mul, Sub};

#[derive(Clone, Copy, Debug)]
pub struct PointOffset {
	pub x: i8,
	pub y: i8,
}

impl PointOffset {
	pub const MIN: Self = Self { x: i8::MIN, y: i8::MIN };
	pub const MAX: Self = Self { x: i8::MAX, y: i8::MAX };
	pub const ZERO: Self = Self { x: 0, y: 0 };

	pub const fn new(x: i8, y: i8) -> Self {
		Self { x, y }
	}
}

impl Point {
	pub fn saturating_add(self, offset: PointOffset) -> Self {
		let f = |b, d| {
			(i32::from(b) + i32::from(d))
				.clamp(0, u16::MAX.into())
				.try_into()
				.unwrap()
		};
		Self::new(f(self.x, offset.x), f(self.y, offset.y))
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

impl Sub<Point> for Point {
	type Output = Option<PointOffset>;

	fn sub(self, rhs: Self) -> Self::Output {
		let x = i32::from(self.x) - i32::from(rhs.x);
		let y = i32::from(self.y) - i32::from(rhs.y);
		x.try_into()
			.and_then(|x| y.try_into().map(|y| PointOffset { x, y }))
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
