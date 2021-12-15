use super::*;
use serde::{Deserialize, Serialize};

/// A controlled buffer selectively blocks or lets through a signal, i.e.
///
/// - If the select input is 0, the output will float.
/// - If the select input is 1, the output takes the value of the data input.
#[derive(Serialize, Deserialize)]
pub struct ControlledBuffer {}

impl ControlledBuffer {
	pub const fn new() -> Self {
		Self {}
	}
}

impl Component for ControlledBuffer {
	fn inputs(&self) -> Box<[InputType]> {
		[
			InputType { bits: NonZeroU8::new(32).unwrap() },
			InputType { bits: NonZeroU8::new(1).unwrap() },
		]
		.into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[OutputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn generate_ir(&self, gen: GenerateIr) -> usize {
		if gen.inputs.iter().all(|e| *e != usize::MAX) && gen.outputs[0] != usize::MAX {
			let ir = [
				IrOp::Copy { a: gen.inputs[1] },
				IrOp::Andi { i: 1 },
				IrOp::BreakZero,
				IrOp::Copy { a: gen.inputs[0] },
				IrOp::Save { out: gen.outputs[0] },
			]
			.into();
			(gen.out)(ir);
		}
		0
	}

	fn properties(&self) -> Box<[Property]> {
		Box::default()
	}

	fn set_property(&mut self, _name: &str, _value: SetProperty) -> Result<(), Box<dyn Error>> {
		Err("no properties".into())
	}
}
