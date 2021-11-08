use super::*;
use crate::circuit::{Direction, PointOffset};
use crate::simulator::{ir, Component};
use eframe::egui::{paint::RectShape, Color32, Painter, Pos2, Rect, Shape, Stroke, Vec2};
use serde::de::{Deserialize, Deserializer, Error, Visitor};
use serde::ser::{Serialize, Serializer};
use std::fmt;
use std::fs::File;
use std::path::Path;

#[typetag::serde]
impl ComponentPlacer for Ic {
	fn name(&self) -> Box<str> {
		self.path().to_string_lossy().into()
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
		let rect = RectShape { rect, corner_radius: 1.0, fill: Color32::WHITE, stroke };
		painter.add(Shape::Rect(rect));
	}
}
