use super::{Direction, InputType, OutputType, PointOffset, RelativeAabb};
use crate::impl_dyn;
use crate::simulator::{ir::IrOp, Component, Property, SetProperty};
use std::error::Error;

/// A component with fixed input & output locations
#[typetag::serde]
pub trait CircuitComponent
where
	Self: Component,
{
	/// The location of each input on this component.
	fn input_points(&self) -> Box<[PointOffset]>;

	/// The location of each output on this component.
	fn output_points(&self) -> Box<[PointOffset]>;

	/// Get the name of an input.
	///
	/// # Panics
	///
	/// The index is out of range.
	fn input_name(&self, index: usize) -> Box<str>;

	/// Get the name of an output.
	///
	/// # Panics
	///
	/// The index is out of range.
	fn output_name(&self, index: usize) -> Box<str>;

	fn external_input(&self) -> Option<usize> {
		None
	}

	fn external_output(&self) -> Option<usize> {
		None
	}

	fn aabb(&self, direction: Direction) -> RelativeAabb;
}

impl_dyn! {
	Component for Box<dyn CircuitComponent> {
		ref label() -> Option<&str>;
		ref inputs() -> Box<[InputType]>;
		ref outputs() -> Box<[OutputType]>;
		ref generate_ir(inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp), ms: usize) -> usize;
		ref properties() -> Box<[Property]>;
		mut set_property(name: &str, value: SetProperty) -> Result<(), Box<dyn Error>>;
	}
}

impl_dyn! {
	CircuitComponent for Box<dyn CircuitComponent> {
		ref input_points() -> Box<[PointOffset]>;
		ref output_points() -> Box<[PointOffset]>;
		ref input_name(index: usize) -> Box<str>;
		ref output_name(index: usize) -> Box<str>;
		ref external_input() -> Option<usize>;
		ref external_output() -> Option<usize>;
		ref aabb(dir: Direction) -> RelativeAabb;
		ref typetag_name() -> &'static str;
		ref typetag_deserialize() -> ();
	}
}
