use super::{Aabb, Point};
use core::fmt;
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
