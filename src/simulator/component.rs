use super::ir::IrOp;
use crate::impl_dyn;
use core::num::NonZeroU8;
use core::ops::RangeInclusive;
use std::error::Error;

/// A single component with one or more inputs and outputs.
pub trait Component {
	/// The label attached to this component.
	fn label(&self) -> Option<&str> {
		None
	}

	/// The types of inputs.
	fn inputs(&self) -> Box<[InputType]>;

	/// The types of outputs.
	fn outputs(&self) -> Box<[OutputType]>;

	/// Generate IR for this component.
	fn generate_ir(&self, generate: GenerateIr) -> usize;

	/// Return all properties of this component.
	fn properties(&self) -> Box<[Property]>;

	/// Set a property of this component.
	fn set_property(&mut self, name: &str, property: SetProperty) -> Result<(), Box<dyn Error>>;

	/// Whether this component is an In, Out or regular component.
	fn external_type(&self) -> Option<ExternalType> {
		None
	}
}

pub enum ExternalType {
	In(usize, usize),
	Out(usize, usize),
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
	Bool { value: bool },
}

#[derive(Clone, Debug)]
pub enum SetProperty {
	Int(i64),
	Str(Box<str>),
	Mask(usize),
	Bool(bool),
}

impl SetProperty {
	pub fn as_int(&self) -> Option<i64> {
		match self {
			Self::Int(i) => Some(*i),
			_ => None,
		}
	}

	pub fn into_str(self) -> Option<Box<str>> {
		match self {
			Self::Str(s) => Some(s),
			_ => None,
		}
	}

	pub fn as_mask(&self) -> Option<usize> {
		match self {
			Self::Mask(i) => Some(*i),
			_ => None,
		}
	}

	pub fn as_bool(&self) -> Option<bool> {
		match self {
			Self::Bool(b) => Some(*b),
			_ => None,
		}
	}
}

impl_dyn! {
	Component for Box<dyn Component> {
		ref inputs() -> Box<[InputType]>;
		ref outputs() -> Box<[OutputType]>;
		ref generate_ir(gen: GenerateIr) -> usize;
		ref properties() -> Box<[Property]>;
		ref external_type() -> Option<ExternalType>;
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

pub struct GenerateIr<'a> {
	pub inputs: &'a [usize],
	pub outputs: &'a [usize],
	pub out: &'a mut dyn FnMut(Vec<IrOp>),
	pub memory_size: usize,
	pub nodes: usize,
}
