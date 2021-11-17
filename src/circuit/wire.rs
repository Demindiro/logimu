use super::{Aabb, Point};
use core::fmt;
use gcd::Gcd;
use serde::{de, ser, Deserialize, Serialize};

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

		let (x1, y1) = (i64::from(self.from.x), i64::from(self.from.y));
		let (xp, yp) = (i64::from(point.x), i64::from(point.y));
		// Make start of line (x1, y1) the origin so b = 0
		let (dx, dy) = self.deltas();
		let (dxp, dyp) = (xp - x1, yp - y1);
		// y = ax <=> y = dy / dx * x <=> y * dx = x * dx
		i64::from(dx) * dyp == i64::from(dy) * dxp
	}

	/// Return the AABB enclosing this wire.
	pub fn aabb(&self) -> Aabb {
		Aabb::new(self.from, self.to)
	}

	/// Return the squared length of this wire.
	#[allow(dead_code)]
	pub fn length_squared(&self) -> u32 {
		let (dx, dy) = self.lengths();
		u32::from(dx) * u32::from(dx) + u32::from(dy) * u32::from(dy)
	}

	/// Iterator over all intersecting points on this wire.
	pub fn intersecting_points(&self) -> Iter {
		let (dx, dy) = self.lengths();
		let steps = dx.gcd(dy);
		let (dx, dy) = self.deltas();
		Iter {
			point: self.from,
			dx: dx / i32::from(steps),
			dy: dy / i32::from(steps),
			steps,
		}
	}

	/// Check whether this wire is visually with another wire, i.e. dy_a / dx_a == dy_b / dx_b.
	pub fn contiguous_with(&self, rhs: &Self) -> bool {
		if ![self.from, self.to].contains(&rhs.from) && ![self.from, self.to].contains(&rhs.to) {
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

	/// Return the lengths along each axis of this wire.
	fn lengths(&self) -> (u16, u16) {
		let Self { from, to } = self;
		let dx = from.x.max(to.x) - from.x.min(to.x);
		let dy = from.y.max(to.y) - from.y.min(to.y);
		(dx, dy)
	}

	/// Return the deltas along each axis of this wire.
	fn deltas(&self) -> (i32, i32) {
		let (x1, y1) = (i32::from(self.from.x), i32::from(self.from.y));
		let (x2, y2) = (i32::from(self.to.x), i32::from(self.to.y));
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
		s.serialize_element(&self.from)?;
		s.serialize_element(&self.to)?;
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
				let from = seq
					.next_element()?
					.ok_or_else(|| de::Error::invalid_length(0, &"2"))?;
				let to = seq
					.next_element()?
					.ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
				Ok(Wire { from, to })
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
				let from = from.ok_or_else(|| de::Error::missing_field("from"))?;
				let to = to.ok_or_else(|| de::Error::missing_field("to"))?;
				Ok(Wire { from, to })
			}
		}

		d.deserialize_any(T)
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
			self.point.x = (i32::from(self.point.x) + self.dx).try_into().unwrap();
			self.point.y = (i32::from(self.point.y) + self.dy).try_into().unwrap();
			p
		})
	}
}
