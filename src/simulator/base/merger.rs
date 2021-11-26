use super::{
	Component, GenerateIr, InputType, IrOp, OutputType, Property, PropertyValue, SetProperty,
};
use core::num::{NonZeroU8, NonZeroUsize};
use serde::{Deserialize, Serialize};
use std::error::Error;

#[derive(Serialize, Deserialize)]
pub struct Merger {
	pub bits: NonZeroU8,
	/// Mask of input bits to output bits.
	pub inputs: Vec<NonZeroUsize>,
}

impl Merger {
	pub fn new(bits: NonZeroU8) -> Self {
		Self {
			bits,
			inputs: (0..bits.get())
				.map(|i| NonZeroUsize::new(1 << i).unwrap())
				.collect(),
		}
	}
}

impl Component for Merger {
	fn inputs(&self) -> Box<[InputType]> {
		self.inputs
			.iter()
			.map(|e| InputType {
				bits: NonZeroU8::new(e.get().count_ones().try_into().unwrap()).unwrap(),
			})
			.collect()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[OutputType { bits: self.bits }].into()
	}

	fn generate_ir(&self, gen: GenerateIr) -> usize {
		assert_eq!(gen.outputs.len(), 1, "expected only one output");
		let output = gen.outputs[0];
		if output == usize::MAX {
			return 0;
		}
		let mut ir = Vec::new();
		for (&w, &r) in gen.inputs.iter().zip(self.inputs.iter()) {
			if w == usize::MAX {
				continue;
			}
			let r = r.get();
			if r.count_ones() == r.trailing_ones() {
				let mask = (1 << r.trailing_ones()) - 1;
				ir.push(IrOp::Copy { a: w });
				ir.push(IrOp::Andi { i: mask });
				ir.push(IrOp::OrB);
			} else if r.count_ones() == (r >> r.trailing_zeros()).trailing_ones() {
				let mask = (1 << r.count_ones()) - 1;
				ir.push(IrOp::Copy { a: w });
				ir.push(IrOp::Andi { i: mask });
				ir.push(IrOp::Slli { i: r.trailing_zeros().try_into().unwrap() });
				ir.push(IrOp::OrB);
			} else {
				todo!("handle spread output bits: {:032b}", r);
			}
		}
		ir.push(IrOp::SaveB { out: output });
		(gen.out)(ir);
		0
	}

	fn properties(&self) -> Box<[Property]> {
		let bits = PropertyValue::Int { value: self.bits.get().into(), range: 1..=32 };
		let inputs =
			PropertyValue::Int { value: self.inputs.len().try_into().unwrap(), range: 1..=32 };
		let mut v = Vec::from([Property::new("bits", bits), Property::new("inputs", inputs)]);
		for (i, o) in self.inputs.iter().enumerate() {
			let mask = PropertyValue::Mask { value: o.get().into() };
			v.push(Property::new(format!("input {}", i), mask));
		}
		v.into()
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
			"inputs" => {
				let v = value.as_int().ok_or("expected integer")?;
				let mut i = self.inputs.len();
				self.inputs.resize_with(
					v.try_into().map_err(|_| "expected positive integer")?,
					|| {
						let n = NonZeroUsize::new(1 << i).unwrap();
						i += 1;
						n
					},
				);
			}
			name if name.starts_with("input ") => {
				let (_, n) = name.split_once(' ').ok_or("invalid property")?;
				let n = n.parse::<usize>().map_err(|_| "invalid property")?;
				let m = self.inputs.get_mut(n).ok_or("invalid property")?;
				let v = value.as_mask().ok_or("expected mask")?;
				*m = v.try_into().map_err(|_| "mask cannot be empty")?;
			}
			_ => Err("invalid property")?,
		}
		Ok(())
	}
}
