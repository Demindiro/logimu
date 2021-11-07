use super::*;
use crate::circuit::{Direction, PointOffset};
use crate::simulator::{ir, Component};
use eframe::egui::{paint::RectShape, Color32, Painter, Pos2, Rect, Shape, Stroke, Vec2};
use serde::de::{Deserialize, Deserializer, Error, Visitor};
use serde::ser::{Serialize, Serializer};
use std::fmt;
use std::fs::File;

#[typetag::serde]
impl ComponentPlacer for Ic {
    fn name(&self) -> &str {
        &self.path
    }

    fn draw(
        &self,
        painter: &Painter,
        position: Pos2,
        direction: Direction,
        inputs: &[usize],
        outputs: &[usize],
    ) {
        let aabb = direction * self.aabb();
        let min = position + Vec2::new(aabb.min.x.into(), aabb.min.y.into()) * 16.0;
        let max = position + Vec2::new(aabb.max.x.into(), aabb.max.y.into()) * 16.0;
        let stroke = Stroke::new(3.0, Color32::BLACK);
        let rect = Rect { min, max };
        let rect = RectShape {
            rect,
            corner_radius: 1.0,
            fill: Color32::WHITE,
            stroke,
        };
        painter.add(Shape::Rect(rect));
    }
}

impl Serialize for Ic {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.path)
    }
}

impl<'a> Deserialize<'a> for Ic {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        struct V;
        impl<'a> Visitor<'a> for V {
            type Value = Ic;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a path to a valid logimu file")
            }

            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                let file = File::open(s).map_err(|e| Error::custom(e))?;
                let circuit: Circuit<Box<dyn ComponentPlacer>> =
                    ron::de::from_reader(file).map_err(|e| Error::custom(e))?;
                Ok(Ic::from_circuit(circuit, s))
            }
        }
        deserializer.deserialize_str(V)
    }
}
