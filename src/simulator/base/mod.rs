mod rom;

pub use rom::*;

use super::ir::IrOp;
use crate::impl_dyn;
use core::fmt;
use core::num::{NonZeroU8, NonZeroUsize};
use core::ops::RangeInclusive;
use serde::de;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::error::Error;

/// A single component with one or more inputs and outputs.
pub trait Component {
	/// The label attached to this component.
	fn label(&self) -> Option<&str> {
		None
	}

	/// The amount of inputs.
	fn input_count(&self) -> usize;

	/// The type of a given input.
	fn input_type(&self, input: usize) -> Option<InputType>;

	/// The amount of outputs.
	fn output_count(&self) -> usize;

	/// The type of a given output.
	fn output_type(&self, output: usize) -> Option<OutputType>;

	/// Generate IR for this component.
	fn generate_ir(
		&self,
		inputs: &[usize],
		outputs: &[usize],
		out: &mut dyn FnMut(IrOp),
		memory_size: usize,
	) -> usize;

	/// Return all properties of this component.
	fn properties(&self) -> Box<[Property]>;

	/// Set a property of this component.
	fn set_property(&mut self, name: &str, property: SetProperty) -> Result<(), Box<dyn Error>>;
}

#[derive(Clone, Debug)]
pub struct Property {
	pub name: Box<str>,
	pub read_only: bool,
	pub value: PropertyValue,
}

impl Property {
	pub fn new(name: impl Into<Box<str>>, value: impl Into<PropertyValue>) -> Self {
		Self { name: name.into(), value: value.into(), read_only: false }
	}
}

#[derive(Clone, Debug)]
pub enum PropertyValue {
	Int { value: i64, range: RangeInclusive<i64> },
	Str { value: Box<str> },
	Mask { value: usize },
}

#[derive(Clone, Debug)]
pub enum SetProperty {
	Int(i64),
	Str(Box<str>),
	Mask(usize),
}

impl SetProperty {
	fn as_int(&self) -> Option<i64> {
		match self {
			Self::Int(i) => Some(*i),
			_ => None,
		}
	}

	fn into_str(self) -> Option<Box<str>> {
		match self {
			Self::Str(s) => Some(s),
			_ => None,
		}
	}

	fn as_mask(&self) -> Option<usize> {
		match self {
			Self::Mask(i) => Some(*i),
			_ => None,
		}
	}
}

impl_dyn! {
	Component for Box<dyn Component> {
		ref input_count() -> usize;
		ref input_type(input: usize) -> Option<InputType>;
		ref output_count() -> usize;
		ref output_type(output: usize) -> Option<OutputType>;
		ref generate_ir(inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp), ms: usize) -> usize;
		ref properties() -> Box<[Property]>;
		mut set_property(name: &str, value: SetProperty) -> Result<(), Box<dyn Error>>;
	}
}

/// The type of an input.
pub struct InputType {
	/// How many bits this input has.
	pub bits: NonZeroU8,
}

/// The type of an output.
pub struct OutputType {
	/// How many bits this output has.
	pub bits: NonZeroU8,
}

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
			fn input_count(&self) -> usize {
				self.inputs.get().into()
			}

			fn input_type(&self, input: usize) -> Option<InputType> {
				(input < self.input_count())
					.then(|| InputType { bits: NonZeroU8::new(32).unwrap() })
			}

			fn output_count(&self) -> usize {
				1
			}

			fn output_type(&self, output: usize) -> Option<OutputType> {
				(output < self.output_count())
					.then(|| OutputType { bits: NonZeroU8::new(32).unwrap() })
			}

			fn generate_ir(
				&self,
				inputs: &[usize],
				outputs: &[usize],
				out: &mut dyn FnMut(IrOp),
				_: usize,
			) -> usize {
				assert!(outputs.len() < 2);
				if let Some(&output) = outputs.first() {
					let mut it = inputs.iter().filter(|a| **a != usize::MAX);
					it.next()
						.map(|&a| out(IrOp::Andi { a, i: usize::MAX, out: output }));
					it.for_each(|&a| out(IrOp::$op { a, b: output, out: output }));
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
	fn input_count(&self) -> usize {
		1
	}

	fn input_type(&self, input: usize) -> Option<InputType> {
		(input < self.input_count()).then(|| InputType { bits: NonZeroU8::new(32).unwrap() })
	}

	fn output_count(&self) -> usize {
		1
	}

	fn output_type(&self, output: usize) -> Option<OutputType> {
		(output < self.output_count()).then(|| OutputType { bits: NonZeroU8::new(32).unwrap() })
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

	fn input_count(&self) -> usize {
		0
	}

	fn input_type(&self, _: usize) -> Option<InputType> {
		None
	}

	fn output_count(&self) -> usize {
		1
	}

	fn output_type(&self, output: usize) -> Option<OutputType> {
		(output == 0).then(|| OutputType { bits: self.bits })
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

	fn input_count(&self) -> usize {
		1
	}

	fn input_type(&self, input: usize) -> Option<InputType> {
		(input == 0).then(|| InputType { bits: self.bits })
	}

	fn output_count(&self) -> usize {
		0
	}

	fn output_type(&self, _: usize) -> Option<OutputType> {
		None
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

#[derive(Serialize, Deserialize)]
pub struct Splitter {
	/// Mask of bits to output from input.
	outputs: Vec<NonZeroUsize>,
}

impl Splitter {
	pub fn new() -> Self {
		Self { outputs: [NonZeroUsize::new(1).unwrap()].into() }
	}
}

impl Component for Splitter {
	fn input_count(&self) -> usize {
		1
	}

	fn input_type(&self, index: usize) -> Option<InputType> {
		(index < 1).then(|| InputType { bits: NonZeroU8::new(32).unwrap() })
	}

	fn output_count(&self) -> usize {
		self.outputs.len()
	}

	fn output_type(&self, index: usize) -> Option<OutputType> {
		self.outputs.get(index).map(|e| OutputType {
			bits: NonZeroU8::new(e.get().count_ones().try_into().unwrap()).unwrap(),
		})
	}

	fn generate_ir(
		&self,
		inputs: &[usize],
		outputs: &[usize],
		out: &mut dyn FnMut(IrOp),
		_: usize,
	) -> usize {
		assert_eq!(inputs.len(), 1, "expected only one input");
		//assert_eq!(outputs.len(), self.outputs.len(), "outputs do not match");
		let input = inputs[0];
		if input == usize::MAX {
			return 0;
		}
		for (&w, &r) in outputs
			.iter()
			.zip(self.outputs.iter())
			.filter(|(&w, _)| w != usize::MAX)
		{
			let r = r.get();
			if r.count_ones() == r.trailing_ones() {
				out(IrOp::Andi { a: input, i: usize::MAX, out: w });
			} else if r.count_ones() == (r >> r.trailing_zeros()).trailing_ones() {
				let shift = r.trailing_zeros().try_into().unwrap();
				out(IrOp::Srli { a: input, i: shift, out: w });
			} else {
				todo!("handle spread output bits: {:032b}", r);
			}
		}
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

#[derive(Serialize, Deserialize)]
pub struct Merger {
	pub bits: NonZeroU8,
	/// Mask of input bits to output bits.
	inputs: Vec<NonZeroUsize>,
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
	fn input_count(&self) -> usize {
		self.inputs.len()
	}

	fn input_type(&self, index: usize) -> Option<InputType> {
		self.inputs.get(index).map(|e| InputType {
			bits: NonZeroU8::new(e.get().count_ones().try_into().unwrap()).unwrap(),
		})
	}

	fn output_count(&self) -> usize {
		1
	}

	fn output_type(&self, index: usize) -> Option<OutputType> {
		(index < 1).then(|| OutputType { bits: self.bits })
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
	fn input_count(&self) -> usize {
		0
	}

	fn input_type(&self, _: usize) -> Option<InputType> {
		None
	}

	fn output_count(&self) -> usize {
		1
	}

	fn output_type(&self, output: usize) -> Option<OutputType> {
		(output < 1).then(|| OutputType { bits: self.bits })
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
