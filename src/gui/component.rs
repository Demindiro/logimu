use eframe::egui::{Painter, Vec2, Pos2};

#[derive(Clone, Copy, Debug)]
pub enum Direction {
	Right,
	Down,
	Left,
	Up,
}

impl Direction {
	pub fn rotate_vec2(self, v: Vec2) -> Vec2 {
		match self {
			Self::Right => Vec2::new( v.x,  v.y),
			Self::Down  => Vec2::new(-v.y,  v.x),
			Self::Left  => Vec2::new(-v.x, -v.y),
			Self::Up    => Vec2::new( v.y, -v.x),
		}
	}

	pub fn rotate_clockwise(self) -> Self {
		match self {
			Self::Right => Self::Down,
			Self::Down => Self::Left,
			Self::Left => Self::Up,
			Self::Up => Self::Right,
		}
	}
}

pub trait ComponentPlacer{
	fn name(&self) -> &str;

	fn draw(&self, painter: &Painter, position: Pos2, direction: Direction);
}
