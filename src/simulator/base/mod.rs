mod constant;
mod controlled_buffer;
mod merger;
mod probe;
mod rom;
mod splitter;

pub use constant::*;
pub use controlled_buffer::*;
pub use merger::*;
pub use probe::*;
pub use rom::*;
pub use splitter::*;

use super::{
	Component, GenerateIr, InputType, IrOp, OutputType, Property, PropertyValue, SetProperty,
};
use crate::integer_set::IntegerSetU8;
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
	($name:ident, $op:ident, $not_op:ident) => {
		#[derive(Serialize, Deserialize)]
		pub struct $name {
			/// The amount of inputs this gate has. Must be at least 2.
			pub inputs: NonZeroOneU8,
			/// Which inputs are inverted. Number 255 represents the output.
			#[serde(default)]
			inverted: IntegerSetU8,
		}

		impl $name {
			pub fn new(inputs: NonZeroOneU8) -> Self {
				Self { inputs, inverted: Default::default() }
			}

			pub fn is_output_inverted(&self) -> bool {
				self.inverted.contains(255)
			}

			pub fn is_input_inverted(&self, input: u8) -> bool {
				self.inverted.contains(input)
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

			fn generate_ir(&self, gen: GenerateIr) -> usize {
				assert_eq!(gen.outputs.len(), 1);
				if gen.outputs[0] == usize::MAX || gen.inputs.iter().all(|&i| i == usize::MAX) {
					return 0;
				}
				let mut it = gen.inputs[..]
					.iter()
					.enumerate()
					.map(|(i, a)| (i.try_into().unwrap(), a))
					.filter(|(_, a)| **a != usize::MAX);
				let it = it
					.next()
					.map(|(i, &a)| {
						if self.inverted.contains(i) {
							IrOp::CopyNot { a }
						} else {
							IrOp::Copy { a }
						}
					})
					.into_iter()
					.chain(it.map(|(i, &a)| {
						if self.inverted.contains(i) {
							IrOp::$not_op { a }
						} else {
							IrOp::$op { a }
						}
					}))
					.chain(self.inverted.contains(255).then(|| IrOp::Not))
					.chain(Some(IrOp::Save { out: gen.outputs[0] }))
					.collect();
				(gen.out)(it);
				0
			}

			fn properties(&self) -> Box<[Property]> {
				let inputs = PropertyValue::Int { value: self.inputs.get().into(), range: 2..=31 };
				let inv_o = PropertyValue::Bool { value: self.inverted.contains(255) };
				let mut v = Vec::with_capacity(2 + usize::from(self.inputs.get()));
				v.extend([
					Property { name: "inputs".into(), read_only: false, value: inputs },
					Property { name: "invert output".into(), read_only: false, value: inv_o },
				]);
				v.extend((0..self.inputs.get()).map(|i| Property {
					name: format!("invert input {}", i).into(),
					read_only: false,
					value: PropertyValue::Bool { value: self.inverted.contains(i) },
				}));
				v.into()
			}

			fn set_property(
				&mut self,
				name: &str,
				value: SetProperty,
			) -> Result<(), Box<dyn Error>> {
				let mut toggle_inv = |i, v: SetProperty| {
					v.as_bool().ok_or("expected boolean").map(|b| {
						if b {
							self.inverted.insert(i)
						} else {
							self.inverted.remove(i)
						}
					})
				};
				match name {
					"inputs" => {
						let v = value.as_int().ok_or("expected integer")?;
						let v = v.try_into().map_err(|_| "integer out of range")?;
						self.inputs = NonZeroOneU8::new(v).ok_or("integer out of range")?;
					}
					"invert output" => {
						toggle_inv(255, value)?;
					}
					n if n.starts_with("invert input ") => {
						let (_, n) = n.split_at("invert input ".len());
						let n = n
							.parse::<u8>()
							.map_err(|_| "expected integer in range 0..254")?;
						if n >= self.inputs.get() {
							Err("input does not exist")?;
						}
						toggle_inv(n, value)?;
					}
					_ => Err("invalid property")?,
				}
				Ok(())
			}
		}
	};
}

gate!(AndGate, And, NotAnd);
gate!(OrGate, Or, NotOr);
gate!(XorGate, Xor, NotXor);

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

	fn generate_ir(&self, gen: GenerateIr) -> usize {
		if gen.inputs[0] != usize::MAX && gen.outputs[0] != usize::MAX {
			let ir = [
				IrOp::Copy { a: gen.inputs[0] },
				IrOp::Xori { i: usize::MAX },
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
		[OutputType { bits: self.bits }].into()
	}

	fn generate_ir(&self, _: GenerateIr) -> usize {
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

	fn external_type(&self) -> Option<super::ExternalType> {
		let mask = (1 << self.bits.get()) - 1;
		Some(super::ExternalType::In(self.index, mask))
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
		[InputType { bits: self.bits }].into()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		[].into()
	}

	fn generate_ir(&self, _: GenerateIr) -> usize {
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

	fn external_type(&self) -> Option<super::ExternalType> {
		let mask = (1 << self.bits.get()) - 1;
		Some(super::ExternalType::Out(self.index, mask))
	}
}
