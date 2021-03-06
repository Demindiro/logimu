use super::*;

use eframe::egui::{paint::RectShape, Rect, Shape, Vec2};

#[typetag::serde]
impl ComponentPlacer for Ic {
	fn name(&self) -> Box<str> {
		self.path().to_string_lossy().into()
	}

	fn draw(&self, draw: Draw) {
		let Draw { painter, alpha, position, direction, .. } = draw;
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
