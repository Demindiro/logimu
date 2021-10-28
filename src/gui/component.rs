use crate::simulator::Component;
use crate::circuit;
use eframe::egui::{Painter, Vec2, Pos2};

impl circuit::Direction {
	pub fn rotate_vec2(self, v: Vec2) -> Vec2 {
		match self {
			Self::Right => Vec2::new( v.x,  v.y),
			Self::Down  => Vec2::new(-v.y,  v.x),
			Self::Left  => Vec2::new(-v.x, -v.y),
			Self::Up    => Vec2::new( v.y, -v.x),
		}
	}
}

pub trait ComponentPlacer{
	fn name(&self) -> &str;

	fn draw(&self, painter: &Painter, position: Pos2, direction: circuit::Direction);

	/// Create a new instance of this component.
	fn instance(&self) -> Box<dyn Component>;
}
