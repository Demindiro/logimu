use super::*;
use crate::circuit::{Direction, PointOffset};
use crate::simulator::{Component, ir};
use std::fmt;
use std::fs::File;
use eframe::egui::{Painter, Pos2, Vec2, Color32, Shape, Rect, Stroke, paint::RectShape};
use serde::ser::{Serialize, Serializer};
use serde::de::{Deserialize, Deserializer, Visitor, Error};

#[typetag::serde]
impl ComponentPlacer for Ic {
	fn name(&self) -> &str {
		&self.path
	}

	fn draw(&self, painter: &Painter, position: Pos2, direction: Direction, inputs: &[usize], outputs: &[usize]) {
		let stroke = Stroke::new(3.0, Color32::BLACK);
		let rect = Rect::from_two_pos(position, position + Vec2::new(32.0, 80.0));
		let rect = RectShape { rect, corner_radius: 1.0, fill: Color32::WHITE, stroke };
		painter.add(Shape::Rect(rect));
	}
}

impl Serialize for Ic
{
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		serializer.serialize_str(&self.path)
	}
}

impl<'a> Deserialize<'a> for Ic
{
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'a>,
	{
		struct V;
		impl<'a> Visitor<'a> for V
		{
			type Value = Ic;

			fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
				f.write_str("a path to a valid logimu file")
			}

			fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
			where
				E: Error,
			{
				let file = File::open(s).map_err(|e| Error::custom(e))?;
				let circuit: Circuit<Box<dyn ComponentPlacer>> = ron::de::from_reader(file)
					.map_err(|e| Error::custom(e))?;
				Ok(Ic::from_circuit(circuit, s))
			}
		}
		deserializer.deserialize_str(V)
	}
}
