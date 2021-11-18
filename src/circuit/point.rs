use core::cmp::Ordering;
use core::fmt;
use serde::{de, ser, Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, Hash)]
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

impl Serialize for Point {
	fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
	where
		S: ser::Serializer,
	{
		use ser::SerializeTuple;
		let mut s = s.serialize_tuple(2)?;
		s.serialize_element(&self.x)?;
		s.serialize_element(&self.y)?;
		s.end()
	}
}

impl<'a> Deserialize<'a> for Point {
	fn deserialize<D>(d: D) -> Result<Self, D::Error>
	where
		D: de::Deserializer<'a>,
	{
		#[derive(Deserialize)]
		#[serde(field_identifier, rename_all = "lowercase")]
		enum F {
			X,
			Y,
		}

		struct T;

		impl<'a> de::Visitor<'a> for T {
			type Value = Point;

			fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
				f.write_str("a struct or a tuple with two integers")
			}

			fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
			where
				V: de::SeqAccess<'a>,
			{
				let x = seq
					.next_element()?
					.ok_or_else(|| de::Error::invalid_length(0, &"2"))?;
				let y = seq
					.next_element()?
					.ok_or_else(|| de::Error::invalid_length(1, &"2"))?;
				Ok(Point { x, y })
			}

			fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
			where
				V: de::MapAccess<'a>,
			{
				let (mut x, mut y) = (None, None);
				while let Some(key) = map.next_key()? {
					match key {
						F::X => {
							if x.is_some() {
								Err(de::Error::duplicate_field("x"))?;
							}
							x = Some(map.next_value()?);
						}
						F::Y => {
							if y.is_some() {
								Err(de::Error::duplicate_field("y"))?;
							}
							y = Some(map.next_value()?);
						}
					}
				}
				let x = x.ok_or_else(|| de::Error::missing_field("x"))?;
				let y = y.ok_or_else(|| de::Error::missing_field("y"))?;
				Ok(Point { x, y })
			}
		}

		d.deserialize_any(T)
	}
}

impl From<(u16, u16)> for Point {
	fn from(t: (u16, u16)) -> Self {
		Self::new(t.0, t.1)
	}
}

impl From<Point> for (u16, u16) {
	fn from(t: Point) -> Self {
		(t.x, t.y)
	}
}
