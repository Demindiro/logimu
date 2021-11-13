use core::cmp::Ordering;
use serde::{Deserialize, Serialize};

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
