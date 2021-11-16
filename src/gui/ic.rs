use super::*;
use crate::circuit::Direction;

use eframe::egui::{paint::RectShape, Color32, Painter, Pos2, Rect, Shape, Stroke, Vec2};

#[typetag::serde]
impl ComponentPlacer for Ic {
	fn name(&self) -> Box<str> {
		self.path().to_string_lossy().into()
	}

	fn draw(
		&self,
		painter: &Painter,
		alpha: f32,
		position: Pos2,
		direction: Direction,
		_inputs: &[usize],
		_outputs: &[usize],
	) {
		let aabb = self.aabb(direction);
		let min = position + Vec2::new(aabb.min.x.into(), aabb.min.y.into()) * 16.0;
		let max = position + Vec2::new(aabb.max.x.into(), aabb.max.y.into()) * 16.0;
		let stroke = super::gates::stroke(alpha);
		let rect = Rect { min, max };
		let fill = super::gates::fill(alpha);
		let rect = RectShape { rect, corner_radius: 1.0, fill, stroke };
		painter.add(Shape::Rect(rect));
	}
}
