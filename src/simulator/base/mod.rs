use super::ir::IrOp;
use core::num::NonZeroU8;

/// A single component with one or more inputs and outputs.
pub trait Component {
	/// The amount of inputs.
	fn input_count(&self) -> usize;

	/// The type of a given input.
	fn input_type(&self, input: usize) -> Option<InputType>;

	/// The amount of outputs.
	fn output_count(&self) -> usize;

	/// The type of a given output.
	fn output_type(&self, output: usize) -> Option<OutputType>;

	/// Generate IR for this component.
	fn generate_ir(&self, inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp));
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

macro_rules! gate {
	($name:ident, $op:ident) => {
		pub struct $name {
			/// The amount of inputs this gate has. Must be at least 2.
			inputs: NonZeroOneU8,
			/// The size of each input and the output.
			bits: NonZeroU8,
		}

		impl $name {
			pub fn new(inputs: NonZeroOneU8, bits: NonZeroU8) -> Self {
				Self { inputs, bits }
			}
		}

		impl Component for $name {
			fn input_count(&self) -> usize {
				self.inputs.get().into()
			}

			fn input_type(&self, input: usize) -> Option<InputType> {
				(input < self.input_count()).then(|| InputType { bits: self.bits })
			}

			fn output_count(&self) -> usize {
				1
			}

			fn output_type(&self, output: usize) -> Option<OutputType> {
				(output < self.output_count()).then(|| OutputType { bits: self.bits })
			}

			fn generate_ir(&self, inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp)) {
				for i in inputs.iter().skip(1) {
					out(IrOp::$op { a: inputs[0], b: *i, out: outputs[0] })
				}
			}
		}
	};
}

gate!(AndGate, And);
gate!(OrGate, Or);
gate!(XorGate, Xor);

pub struct NotGate {
	/// The size of each input and the output.
	bits: NonZeroU8,
}

impl NotGate {
	pub fn new(bits: NonZeroU8) -> Self {
		Self { bits }
	}
}

impl Component for NotGate {
	fn input_count(&self) -> usize {
		1
	}

	fn input_type(&self, input: usize) -> Option<InputType> {
		(input < self.input_count()).then(|| InputType { bits: self.bits })
	}

	fn output_count(&self) -> usize {
		1
	}

	fn output_type(&self, output: usize) -> Option<OutputType> {
		(output < self.output_count()).then(|| OutputType { bits: self.bits })
	}

	fn generate_ir(&self, inputs: &[usize], outputs: &[usize], out: &mut dyn FnMut(IrOp)) {
		out(IrOp::Not { a: inputs[0], out: outputs[0] })
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use super::super::ir::interpreter;

	#[test]
	fn manual_xor() {
		let mut ir = Vec::new();

		AndGate::new(
			NonZeroOneU8::new(2).unwrap(),
			NonZeroU8::new(1).unwrap(),
		).generate_ir(&[0, 1], &[2], &mut |op| ir.push(op));
		OrGate::new(
			NonZeroOneU8::new(2).unwrap(),
			NonZeroU8::new(1).unwrap(),
		).generate_ir(&[0, 1], &[3], &mut |op| ir.push(op));
		NotGate::new(
			NonZeroU8::new(1).unwrap(),
		).generate_ir(&[2], &[4], &mut |op| ir.push(op));
		AndGate::new(
			NonZeroOneU8::new(2).unwrap(),
			NonZeroU8::new(1).unwrap(),
		).generate_ir(&[3, 4], &[5], &mut |op| ir.push(op));

		XorGate::new(
			NonZeroOneU8::new(2).unwrap(),
			NonZeroU8::new(1).unwrap(),
		).generate_ir(&[0, 1], &[6], &mut |op| ir.push(op));
		
		let (a, b) = (0b1100, 0b0110);
		let mut mem = [a, b, 0, 0, 0, 0, 0];
		interpreter::run(&ir, &mut mem);

		assert_eq!(mem, [a, b, a & b, a | b, !(a & b), a ^ b, a ^ b])
	}
}
