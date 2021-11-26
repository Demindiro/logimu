use super::{Aabb, Point};
use core::fmt;
use gcd::Gcd;
use serde::{de, ser, Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// The `min` point will always come before `max`.
pub struct Wire {
	min: Point,
	max: Point,
}

impl Wire {
	/// Create a new wire.
	pub fn new(a: Point, b: Point) -> Self {
		Self { min: a.min(b), max: a.max(b) }
	}

	/// Check if this wire intersects with a point.
	pub fn intersect_point(&self, point: Point) -> bool {
		// Check if the point is inside the AABB of the wire.
		if !self.aabb().intersect_point(point) {
			return false;
		}

		let (x1, y1) = (i64::from(self.min.x), i64::from(self.min.y));
		let (xp, yp) = (i64::from(point.x), i64::from(point.y));
		// Make start of line (x1, y1) the origin so b = 0
		let (dx, dy) = self.deltas();
		let (dxp, dyp) = (xp - x1, yp - y1);
		// y = ax <=> y = dy / dx * x <=> y * dx = x * dx
		i64::from(dx) * dyp == i64::from(dy) * dxp
	}

	/// Return the AABB enclosing this wire.
	pub fn aabb(&self) -> Aabb {
		Aabb::new(self.min, self.max)
	}

	/// Return the squared length of this wire.
	#[allow(dead_code)]
	pub fn length_squared(&self) -> u32 {
		let (dx, dy) = self.lengths();
		u32::from(dx) * u32::from(dx) + u32::from(dy) * u32::from(dy)
	}

	/// Iterate over all intersecting points on this wire.
	pub fn intersecting_points(&self) -> Iter {
		let steps = match self.lengths() {
			(0, 0) => return Iter { point: self.min, dx: 0, dy: 0, steps: 1 },
			(dx, 0) => dx,
			(0, dy) => dy,
			(dx, dy) => dx.gcd(dy),
		};
		let (dx, dy) = self.deltas();
		Iter {
			point: self.min,
			dx: dx / i32::from(steps),
			dy: dy / i32::from(steps),
			steps: steps + 1,
		}
	}

	/// Iterate over all the smallest segments this wire is built of.
	pub fn segments(&self) -> IterSegments {
		let mut iter = self.intersecting_points();
		let min = iter.next().unwrap();
		IterSegments { min, iter }
	}

	/// Check whether this wire is visually with another wire, i.e. dy_a / dx_a == dy_b / dx_b.
	pub fn contiguous_with(self, rhs: Self) -> bool {
		if ![self.min, self.max].contains(&rhs.min) && ![self.min, self.max].contains(&rhs.max) {
			return false;
		}
		let (dx_a, dy_a) = self.deltas();
		let (dx_b, dy_b) = rhs.deltas();
		//     dy_a / dx_a == dy_b / dx_b
		// <=> dy_a * dx_b == dy_b * dx_a
		if dx_a == 0 || dx_b == 0 {
			return dx_a == dx_b;
		}
		i64::from(dy_a) * i64::from(dx_b) == i64::from(dy_b) * i64::from(dx_a)
	}

	/// Try max merge two wires.
	pub fn merge(self, rhs: Self) -> Option<Self> {
		self.contiguous_with(rhs).then(|| {
			if self.max == rhs.min {
				Self { min: self.min, max: rhs.max }
			} else {
				Self { min: rhs.min, max: self.max }
			}
		})
	}

	/// Return the lengths along each axis of this wire.
	fn lengths(&self) -> (u16, u16) {
		let Self { min, max } = self;
		let dx = min.x.max(max.x) - min.x.min(max.x);
		let dy = min.y.max(max.y) - min.y.min(max.y);
		(dx, dy)
	}

	/// Return the deltas along each axis of this wire.
	fn deltas(&self) -> (i32, i32) {
		let (x1, y1) = (i32::from(self.min.x), i32::from(self.min.y));
		let (x2, y2) = (i32::from(self.max.x), i32::from(self.max.y));
		(x2 - x1, y2 - y1)
	}
}

impl Serialize for Wire {
	fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
	where
		S: ser::Serializer,
	{
		use ser::SerializeTuple;
		let mut s = s.serialize_tuple(2)?;
		s.serialize_element(&self.min)?;
		s.serialize_element(&self.max)?;
		s.end()
	}
}

impl<'a> Deserialize<'a> for Wire {
	fn deserialize<D>(d: D) -> Result<Self, D::Error>
	where
		D: de::Deserializer<'a>,
	{
		#[derive(Deserialize)]
		#[serde(field_identifier, rename_all = "lowercase")]
		enum F {
			From,
			To,
		}

		struct T;

		impl<'a> de::Visitor<'a> for T {
			type Value = Wire;

			fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
				f.write_str("a struct or a tuple with two integers")
			}

			fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
			where
				V: de::SeqAccess<'a>,
			{
				let a = seq
					.next_element()?
					.ok_or_else(|| de::Error::invalid_length(0, &"2"))?;
				let b = seq
					.next_element()?
					.ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
				Ok(Wire::new(a, b))
			}

			fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
			where
				V: de::MapAccess<'a>,
			{
				let (mut from, mut to) = (None, None);
				while let Some(keto) = map.next_key()? {
					match keto {
						F::From => {
							if from.is_some() {
								Err(de::Error::duplicate_field("from"))?;
							}
							from = Some(map.next_value()?);
						}
						F::To => {
							if to.is_some() {
								Err(de::Error::duplicate_field("to"))?;
							}
							to = Some(map.next_value()?);
						}
					}
				}
				let a = from.ok_or_else(|| de::Error::missing_field("from"))?;
				let b = to.ok_or_else(|| de::Error::missing_field("to"))?;
				Ok(Wire::new(a, b))
			}
		}

		d.deserialize_any(T)
	}
}

impl From<((u16, u16), (u16, u16))> for Wire {
	fn from(t: ((u16, u16), (u16, u16))) -> Self {
		Self::new(t.0.into(), t.1.into())
	}
}

impl From<(Point, Point)> for Wire {
	fn from(t: (Point, Point)) -> Self {
		Self::new(t.0, t.1)
	}
}

impl From<Wire> for ((u16, u16), (u16, u16)) {
	fn from(t: Wire) -> Self {
		(t.min.into(), t.max.into())
	}
}

impl From<Wire> for (Point, Point) {
	fn from(t: Wire) -> Self {
		(t.min, t.max)
	}
}

pub struct Iter {
	point: Point,
	dx: i32,
	dy: i32,
	steps: u16,
}

impl Iterator for Iter {
	type Item = Point;

	fn next(&mut self) -> Option<Self::Item> {
		self.steps.checked_sub(1).map(|s| {
			self.steps = s;
			let p = self.point;
			// Cast to work around overflow when steps == 0 without adding a branch.
			self.point.x = (i32::from(self.point.x) + self.dx) as u16;
			self.point.y = (i32::from(self.point.y) + self.dy) as u16;
			p
		})
	}
}

impl ExactSizeIterator for Iter {
	fn len(&self) -> usize {
		self.steps.into()
	}
}

pub struct IterSegments {
	min: Point,
	iter: Iter,
}

impl Iterator for IterSegments {
	type Item = Wire;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next().map(|max| {
			let w = Wire { min: self.min, max };
			self.min = max;
			w
		})
	}
}

impl ExactSizeIterator for IterSegments {
	fn len(&self) -> usize {
		self.iter.len()
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn merge_horizontal() {
		let a = Wire::from(((0, 1), (0, 3)));
		let b = Wire::from(((0, 3), (0, 6)));
		assert_eq!(Some(Wire::from(((0, 1), (0, 6)))), a.merge(b));
	}

	#[test]
	fn merge_vertical() {
		let a = Wire::from(((1, 0), (3, 0)));
		let b = Wire::from(((3, 0), (6, 0)));
		assert_eq!(Some(Wire::from(((1, 0), (6, 0)))), a.merge(b));
	}

	#[test]
	fn merge_diagonal() {
		// step: (2, 3)
		// (1, 3) + 1 * (2, 3) = (3, 6)
		// (3, 6) + 2 * (2, 3) = (7, 12)
		let a = Wire::from(((7, 12), (3, 6)));
		let b = Wire::from(((1, 3), (3, 6)));
		assert_eq!(Some(Wire::from(((1, 3), (7, 12)))), a.merge(b));
	}

	#[test]
	fn points() {
		// step: (2, 3)
		// (1, 3) + 3 * (2, 3) = (7, 12)
		let mut it = Wire::from(((1, 3), (7, 12))).intersecting_points();
		assert_eq!(it.next(), Some((1, 3).into()));
		assert_eq!(it.next(), Some((3, 6).into()));
		assert_eq!(it.next(), Some((5, 9).into()));
		assert_eq!(it.next(), Some((7, 12).into()));
		assert_eq!(it.next(), None);
	}

	#[test]
	fn segments() {
		// step: (2, 3)
		// (1, 3) + 3 * (2, 3) = (7, 12)
		let mut it = Wire::from(((1, 3), (7, 12))).segments();
		assert_eq!(it.next(), Some(Wire::from(((1, 3), (3, 6)))));
		assert_eq!(it.next(), Some(Wire::from(((3, 6), (5, 9)))));
		assert_eq!(it.next(), Some(Wire::from(((5, 9), (7, 12)))));
		assert_eq!(it.next(), None);
	}
}
