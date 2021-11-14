mod constant;
mod merger;
mod rom;
mod splitter;

pub use constant::*;
pub use merger::*;
pub use rom::*;
pub use splitter::*;

use super::{Component, InputType, IrOp, OutputType, Property, PropertyValue, SetProperty};
use core::fmt;
use core::num::NonZeroU8;
use serde::de;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::error::Error;

/// A u8 that is larger than 2.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct NonZeroOneU8(NonZeroU8);

impl NonZeroOneU8 {
	pub fn new(n: u8) -> Option<Self> {
		(n >= 2).then(|| Self(NonZeroU8::new(n).unwrap()))
	}

	pub fn get(&self) -> u8 {
		self.0.get()
	}
}

impl Serialize for NonZeroOneU8 {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		serializer.serialize_u8(self.get())
	}
}

impl<'a> Deserialize<'a> for NonZeroOneU8 {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'a>,
	{
		struct V;

		impl<'b> de::Visitor<'b> for V {
			type Value = NonZeroOneU8;

			fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
				formatter.write_str("out of range")
			}

			fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
			where
				E: de::Error,
			{
				NonZeroOneU8::new(value).ok_or(E::invalid_value(
					de::Unexpected::Unsigned(value.into()),
					&self,
				))
			}
		}

		deserializer.deserialize_u8(V)
	}
}

macro_rules! gate {
	($name:ident, $op:ident) => {
		#[derive(Serialize, Deserialize)]
		pub struct $name {
			/// The amount of inputs this gate has. Must be at least 2.
			pub inputs: NonZeroOneU8,
		}

		impl $name {
			pub fn new(inputs: NonZeroOneU8) -> Self {
				Self { inputs }
			}
		}

		impl Component for $name {
			fn inputs(&self) -> Box<[InputType]> {
				let bits = NonZeroU8::new(32).unwrap();
				(0..self.inputs.get()).map(|_| InputType { bits }).collect()
			}

			fn outputs(&self) -> Box<[OutputType]> {
				[OutputType { bits: NonZeroU8::new(32).unwrap() }].into()
			}

			fn generate_ir(
				&self,
				inputs: &[usize],
				outputs: &[usize],
				out: &mut dyn FnMut(IrOp),
				_: usize,
			) -> usize {
				assert!(outputs.len() < 2);
				let mut it = inputs.iter().filter(|a| **a != usize::MAX);
				if let (Some(&output), Some(mut a)) = (outputs.first(), it.next().copied()) {
					let mut cpy = true;
					while let Some(&b) = it.next() {
						out(IrOp::$op { a, b, out: output });
						a = output;
						cpy = false;
					}
					cpy.then(|| out(IrOp::Copy { a, out: output }));
				}
				0
			}

			fn properties(&self) -> Box<[Property]> {
				let inputs = PropertyValue::Int { value: self.inputs.get().into(), range: 2..=31 };
				[Property { name: "inputs".into(), read_only: false, value: inputs }].into()
			}

			fn set_property(
				&mut self,
				name: &str,
				value: SetProperty,
			) -> Result<(), Box<dyn Error>> {
				match name {
					"inputs" => {
						let v = value.as_int().ok_or("expected integer")?;
						let v = v.try_into().map_err(|_| "integer out of range")?;
						self.inputs = NonZeroOneU8::new(v).ok_or("integer out of range")?;
					}
					_ => Err("invalid property")?,
				}
				Ok(())
			}
		}
	};
}

gate!(AndGate, And);
gate!(OrGate, Or);
gate!(XorGate, Xor);

#[derive(Serialize, Deserialize)]
pub struct NotGate {}

impl NotGate {
	pub fn new() -> Self {
		Self {}
	}
}

impl Component for NotGate {
	fn inputs(&self) -> Box<[InputType]> {
		[InputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[OutputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn generate_ir(
		&self,
		inputs: &[usize],
		outputs: &[usize],
		out: &mut dyn FnMut(IrOp),
		_: usize,
	) -> usize {
		out(IrOp::Not { a: inputs[0], out: outputs[0] });
		0
	}

	fn properties(&self) -> Box<[Property]> {
		Box::default()
	}

	fn set_property(&mut self, _name: &str, _value: SetProperty) -> Result<(), Box<dyn Error>> {
		Err("no properties".into())
	}
}

#[derive(Serialize, Deserialize)]
pub struct In {
	#[serde(default)]
	pub name: Box<str>,
	pub bits: NonZeroU8,
	pub index: usize,
}

impl In {
	pub fn new(name: impl Into<Box<str>>, bits: NonZeroU8, index: usize) -> Self {
		Self { name: name.into(), bits, index }
	}
}

impl Component for In {
	fn label(&self) -> Option<&str> {
		(!self.name.is_empty()).then(|| &*self.name)
	}

	fn inputs(&self) -> Box<[InputType]> {
		[].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[OutputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn generate_ir(
		&self,
		_: &[usize],
		outputs: &[usize],
		out: &mut dyn FnMut(IrOp),
		_: usize,
	) -> usize {
		out(IrOp::In { out: outputs[0], index: self.index });
		let mask = (1 << self.bits.get()) - 1;
		out(IrOp::Andi { a: outputs[0], i: mask, out: outputs[0] });
		0
	}

	fn properties(&self) -> Box<[Property]> {
		let name = PropertyValue::Str { value: self.name.clone() };
		let bits = PropertyValue::Int { value: self.bits.get().into(), range: 1..=32 };
		[Property::new("name", name), Property::new("bits", bits)].into()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		match name {
			"name" => self.name = value.into_str().ok_or("expected string")?,
			"bits" => {
				let v = value.as_int().ok_or("expected integer")?;
				(1..=32)
					.contains(&v)
					.then(|| self.bits = NonZeroU8::new(v.try_into().unwrap()).unwrap())
					.ok_or("integer out of range")?;
			}
			_ => Err("invalid property")?,
		}
		Ok(())
	}
}

#[derive(Serialize, Deserialize)]
pub struct Out {
	#[serde(default)]
	pub name: Box<str>,
	pub bits: NonZeroU8,
	pub index: usize,
}

impl Out {
	pub fn new(name: impl Into<Box<str>>, bits: NonZeroU8, index: usize) -> Self {
		Self { name: name.into(), bits, index }
	}
}

impl Component for Out {
	fn label(&self) -> Option<&str> {
		(!self.name.is_empty()).then(|| &*self.name)
	}

	fn inputs(&self) -> Box<[InputType]> {
		[InputType { bits: NonZeroU8::new(32).unwrap() }].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[].into()
	}

	fn generate_ir(
		&self,
		inputs: &[usize],
		_: &[usize],
		out: &mut dyn FnMut(IrOp),
		_: usize,
	) -> usize {
		out(IrOp::Out { a: inputs[0], index: self.index });
		0
	}

	fn properties(&self) -> Box<[Property]> {
		let name = PropertyValue::Str { value: self.name.clone() };
		let bits = PropertyValue::Int { value: self.bits.get().into(), range: 1..=32 };
		[Property::new("name", name), Property::new("bits", bits)].into()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		match name {
			"name" => self.name = value.into_str().ok_or("expected string")?,
			"bits" => {
				let v = value.as_int().ok_or("expected integer")?;
				(1..=32)
					.contains(&v)
					.then(|| self.bits = NonZeroU8::new(v.try_into().unwrap()).unwrap())
					.ok_or("integer out of range")?;
			}
			_ => Err("invalid property")?,
		}
		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::super::ir::interpreter;
	use super::*;

	/// ```
	/// i0 --+-------v
	///      |      AND --> NOT
	/// i1 --|--+----^       |
	///      |  |            v
	///      +--|----v      AND --> o0
	///      |  |    OR -----^
	///      |  +----^
	///      |  |
	///      +--|----v
	///         |   XOR ----------> o1
	///         +----^
	/// ```
	#[test]
	fn manual_xor() {
		let mut ir = Vec::new();

		let inputs = NonZeroOneU8::new(2).unwrap();
		AndGate::new(inputs).generate_ir(&[0, 1], &[2], &mut |op| ir.push(op), 0);
		OrGate::new(inputs).generate_ir(&[0, 1], &[3], &mut |op| ir.push(op), 0);
		NotGate::new().generate_ir(&[2], &[4], &mut |op| ir.push(op), 0);
		AndGate::new(inputs).generate_ir(&[3, 4], &[5], &mut |op| ir.push(op), 0);

		XorGate::new(inputs).generate_ir(&[0, 1], &[6], &mut |op| ir.push(op), 0);

		let (a, b) = (0b1100, 0b0110);
		let mut mem = [a, b, 0, 0, 0, 0, 0];

		let mut out = [0; 2];
		interpreter::run(&ir, &mut mem, &mut [a, b], &mut out);

		assert_eq!(mem, [a, b, a & b, a | b, !(a & b), a ^ b, a ^ b])
	}
}
