use super::{
	Component, GenerateIr, InputType, IrOp, OutputType, Property, PropertyValue, SetProperty,
};
use core::num::{NonZeroU8, NonZeroUsize};
use serde::{Deserialize, Serialize};
use std::error::Error;

#[derive(Serialize, Deserialize)]
pub struct Splitter {
	/// Mask of bits to output from input.
	pub outputs: Vec<NonZeroUsize>,
}

impl Splitter {
	pub fn new() -> Self {
		Self { outputs: [NonZeroUsize::new(1).unwrap()].into() }
	}
}

impl Component for Splitter {
	fn inputs(&self) -> Box<[InputType]> {
		[InputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		self.outputs
			.iter()
			.map(|e| OutputType {
				bits: NonZeroU8::new(e.get().count_ones().try_into().unwrap()).unwrap(),
			})
			.collect()
	}

	fn generate_ir(&self, gen: GenerateIr) -> usize {
		assert_eq!(gen.inputs.len(), 1, "expected only one input");
		//assert_eq!(outputs.len(), self.outputs.len(), "outputs do not match");
		let input = gen.inputs[0];
		if input == usize::MAX {
			return 0;
		}
		let mut ir = Vec::new();
		for (&w, &r) in gen
			.outputs
			.iter()
			.zip(self.outputs.iter())
			.filter(|(&w, _)| w != usize::MAX)
		{
			let r = r.get();
			if r.count_ones() == r.trailing_ones() {
				// 000..111
				ir.push(IrOp::Copy { a: input });
				ir.push(IrOp::Save { out: w });
			} else if r.count_ones() == (r >> r.trailing_zeros()).trailing_ones() {
				let shift = r.trailing_zeros().try_into().unwrap();
				ir.push(IrOp::Copy { a: input });
				ir.push(IrOp::Srli { i: shift });
				ir.push(IrOp::Save { out: w });
			} else {
				todo!("handle spread output bits: {:032b}", r);
			}
		}
		(gen.out)(ir);
		0
	}

	fn properties(&self) -> Box<[Property]> {
		let outputs =
			PropertyValue::Int { value: self.outputs.len().try_into().unwrap(), range: 1..=32 };
		let mut v = Vec::from([Property::new("outputs", outputs)]);
		for (i, o) in self.outputs.iter().enumerate() {
			let mask = PropertyValue::Mask { value: o.get().into() };
			v.push(Property::new(format!("output {}", i), mask));
		}
		v.into()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		match name {
			"outputs" => {
				let v = value.as_int().ok_or("expected integer")?;
				let mut i = self.outputs.len();
				self.outputs.resize_with(
					v.try_into().map_err(|_| "expected positive integer")?,
					|| {
						let n = NonZeroUsize::new(1 << i).unwrap();
						i += 1;
						n
					},
				);
			}
			name if name.starts_with("output ") => {
				let (_, n) = name.split_once(' ').ok_or("invalid property")?;
				let n = n.parse::<usize>().map_err(|_| "invalid property")?;
				let m = self.outputs.get_mut(n).ok_or("invalid property")?;
				let v = value.as_mask().ok_or("expected mask")?;
				*m = v.try_into().map_err(|_| "mask cannot be empty")?;
			}
			_ => Err("invalid property")?,
		}
		Ok(())
	}
}
