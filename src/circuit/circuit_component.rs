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
	/// All the inputs of this component.
	fn inputs(&self) -> Box<[PointOffset]>;

	/// All the outputs of this component.
	fn outputs(&self) -> Box<[PointOffset]>;

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
		ref input_count() -> usize;
		ref input_type(input: usize) -> Option<InputType>;
		ref output_count() -> usize;
		ref output_type(output: usize) -> Option<OutputType>;
		ref generate_ir(inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp), ms: usize) -> usize;
		ref properties() -> Box<[Property]>;
		mut set_property(name: &str, value: SetProperty) -> Result<(), Box<dyn Error>>;
	}
}

impl_dyn! {
	CircuitComponent for Box<dyn CircuitComponent> {
		ref inputs() -> Box<[PointOffset]>;
		ref outputs() -> Box<[PointOffset]>;
		ref external_input() -> Option<usize>;
		ref external_output() -> Option<usize>;
		ref aabb(dir: Direction) -> RelativeAabb;
		ref typetag_name() -> &'static str;
		ref typetag_deserialize() -> ();
	}
}
