use super::{Component, InputType, IrOp, OutputType, Property, PropertyValue, SetProperty};
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

	fn generate_ir(
		&self,
		inputs: &[usize],
		outputs: &[usize],
		out: &mut dyn FnMut(IrOp),
		mem: usize,
	) -> usize {
		assert_eq!(outputs.len(), 1, "expected only one output");
		let output = outputs[0];
		let temp = mem;
		// Clear destination
		out(IrOp::Andi { a: output, i: 0, out: output });
		for (&w, &r) in inputs.iter().zip(self.inputs.iter()) {
			let r = r.get();
			if r.count_ones() == r.trailing_ones() {
				let mask = (1 << r.trailing_ones()) - 1;
				out(IrOp::Andi { a: w, i: mask, out: temp });
				out(IrOp::Or { a: output, b: temp, out: output });
			} else if r.count_ones() == (r >> r.trailing_zeros()).trailing_ones() {
				let mask = (1 << r.count_ones()) - 1;
				out(IrOp::Andi { a: w, i: mask, out: temp });
				out(IrOp::Slli {
					a: temp,
					i: r.trailing_zeros().try_into().unwrap(),
					out: temp,
				});
				out(IrOp::Or { a: output, b: temp, out: output });
			} else {
				todo!("handle spread output bits: {:032b}", r);
			}
		}
		1
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
