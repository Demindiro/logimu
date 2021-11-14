use super::{Component, InputType, IrOp, OutputType, Property, PropertyValue, SetProperty};
use core::num::NonZeroU8;
use serde::{Deserialize, Serialize};
use std::error::Error;

#[derive(Debug, Serialize, Deserialize)]
pub struct Constant {
	pub value: usize,
	pub bits: NonZeroU8,
}

impl Constant {
	pub fn new(bits: NonZeroU8, value: usize) -> Self {
		Self { bits, value }
	}
}

impl Component for Constant {
	fn inputs(&self) -> Box<[InputType]> {
		[].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[OutputType { bits: self.bits }].into()
	}

	fn generate_ir(
		&self,
		_: &[usize],
		outputs: &[usize],
		out: &mut dyn FnMut(IrOp),
		_: usize,
	) -> usize {
		if outputs[0] != usize::MAX {
			out(IrOp::Load { value: self.value, out: outputs[0] });
		}
		0
	}

	fn properties(&self) -> Box<[Property]> {
		let bits = self.bits.get();
		let range = (-(1 << bits) >> 1)..=((1 << bits) - 1);
		let value = PropertyValue::Int { value: self.value as i64, range };
		let bits = PropertyValue::Int { value: bits.into(), range: 1..=32 };
		[Property::new("bits", bits), Property::new("value", value)].into()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		match name {
			"bits" => {
				let v = value.as_int().ok_or("expected integer")?;
				(1..=32)
					.contains(&v)
					.then(|| self.bits = NonZeroU8::new(v.try_into().unwrap()).unwrap())
					.ok_or("integer out of range")?;
			}
			"value" => self.value = value.as_int().ok_or("expected integer")? as usize,
			_ => Err("invalid property")?,
		}
		Ok(())
	}
}
