use crate::circuit::{CircuitComponent, Direction, PointOffset, RelativeAabb};
use crate::impl_dyn;
use crate::simulator::{ir::IrOp, Component, InputType, OutputType, Property, SetProperty};
use core::any::Any;
use core::ops::Mul;
use eframe::egui::{Painter, Pos2, Vec2};
use std::error::Error;

impl Mul<Vec2> for Direction {
	type Output = Vec2;

	fn mul(self, rhs: Vec2) -> Self::Output {
		self.rotate_vec2(rhs)
	}
}

impl Direction {
	pub fn rotate_vec2(self, v: Vec2) -> Vec2 {
		match self {
			Self::Right => Vec2::new(v.x, v.y),
			Self::Down => Vec2::new(-v.y, v.x),
			Self::Left => Vec2::new(-v.x, -v.y),
			Self::Up => Vec2::new(v.y, -v.x),
		}
	}
}

#[typetag::serde]
pub trait ComponentPlacer
where
	Self: CircuitComponent + Any,
{
	fn name(&self) -> Box<str>;

	fn draw(
		&self,
		painter: &Painter,
		position: Pos2,
		direction: Direction,
		inputs: &[usize],
		outputs: &[usize],
	);
}

impl_dyn! {
	Component for Box<dyn ComponentPlacer> {
		ref input_count() -> usize;
		ref input_type(input: usize) -> Option<InputType>;
		ref output_count() -> usize;
		ref output_type(output: usize) -> Option<OutputType>;
		ref generate_ir(inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp), ms: usize) -> usize;
		ref properties() -> Box<[Property]>;
		mut set_property(name: &str, property: SetProperty) -> Result<(), Box<dyn Error>>;
	}
}

impl_dyn! {
	#[typetag::serde]
	CircuitComponent for Box<dyn ComponentPlacer> {
		ref inputs() -> Box<[PointOffset]>;
		ref outputs() -> Box<[PointOffset]>;
		ref external_input() -> Option<usize>;
		ref external_output() -> Option<usize>;
		ref aabb(dir: Direction) -> RelativeAabb;
	}
}
