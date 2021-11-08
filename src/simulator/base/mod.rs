use super::ir::IrOp;
use crate::impl_dyn;
use core::fmt;
use core::num::NonZeroU8;
use core::ops::RangeInclusive;
use std::error::Error;
use serde::de;
use serde::ser::SerializeStruct;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

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
	fn set_property(&mut self, name: &'static str, property: SetProperty) -> Result<(), Box<dyn Error>>;
}

#[derive(Clone, Debug)]
pub struct Property {
	pub name: &'static str,
	pub read_only: bool,
	pub value: PropertyValue,
}

impl Property {
	pub const fn new(name: &'static str, value: PropertyValue) -> Self {
		Self { name, value, read_only: false }
	}
}

#[derive(Clone, Debug)]
pub enum PropertyValue {
	Int { value: i64, range: RangeInclusive<i64> },
	Str { value: Box<str> },
}

#[derive(Clone, Debug)]
pub enum SetProperty {
	Int(i64),
	Str(Box<str>),
}

impl SetProperty {
	fn as_int(&self) -> Option<i64> {
		match self {
			Self::Int(i) => Some(*i),
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
		mut set_property(name: &'static str, value: SetProperty) -> Result<(), Box<dyn Error>>;
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

            fn generate_ir(
                &self,
                inputs: &[usize],
                outputs: &[usize],
                out: &mut dyn FnMut(IrOp),
                _: usize,
            ) -> usize {
                for i in inputs.iter().skip(1) {
                    out(IrOp::$op {
                        a: inputs[0],
                        b: *i,
                        out: outputs[0],
                    })
                }
                0
            }

			fn properties(&self) -> Box<[Property]> {
				Box::default()
			}

			fn set_property(&mut self, name: &'static str, value: SetProperty) -> Result<(), Box<dyn Error>> {
				Err("no properties".into())
			}
        }
    };
}

gate!(AndGate, And);
gate!(OrGate, Or);
gate!(XorGate, Xor);

#[derive(Serialize, Deserialize)]
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

    fn generate_ir(
        &self,
        inputs: &[usize],
        outputs: &[usize],
        out: &mut dyn FnMut(IrOp),
        _: usize,
    ) -> usize {
        out(IrOp::Not {
            a: inputs[0],
            out: outputs[0],
        });
        0
    }

	fn properties(&self) -> Box<[Property]> {
		Box::default()
	}

	fn set_property(&mut self, name: &'static str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		Err("no properties".into())
	}
}

#[derive(Serialize, Deserialize)]
pub struct In {
    pub bits: NonZeroU8,
    pub index: usize,
}

impl In {
    pub fn new(bits: NonZeroU8, index: usize) -> Self {
        Self { bits, index }
    }
}

impl Component for In {
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
        out(IrOp::In {
            out: outputs[0],
            index: self.index,
        });
        0
    }

	fn properties(&self) -> Box<[Property]> {
		let bits = PropertyValue::Int { value: self.bits.get().into(), range: 1..=32 };
		Box::new([Property::new("bits", bits)])
	}

	fn set_property(&mut self, name: &'static str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		match name {
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
    pub bits: NonZeroU8,
    pub index: usize,
}

impl Out {
    pub fn new(bits: NonZeroU8, index: usize) -> Self {
        Self { bits, index }
    }
}

impl Component for Out {
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
        out(IrOp::Out {
            a: inputs[0],
            index: self.index,
        });
        0
    }

	fn properties(&self) -> Box<[Property]> {
		let bits = PropertyValue::Int { value: self.bits.get().into(), range: 1..=32 };
		Box::new([Property::new("bits", bits)])
	}

	fn set_property(&mut self, name: &'static str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		match name {
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

        AndGate::new(NonZeroOneU8::new(2).unwrap(), NonZeroU8::new(1).unwrap()).generate_ir(
            &[0, 1],
            &[2],
            &mut |op| ir.push(op),
            0,
        );
        OrGate::new(NonZeroOneU8::new(2).unwrap(), NonZeroU8::new(1).unwrap()).generate_ir(
            &[0, 1],
            &[3],
            &mut |op| ir.push(op),
            0,
        );
        NotGate::new(NonZeroU8::new(1).unwrap()).generate_ir(&[2], &[4], &mut |op| ir.push(op), 0);
        AndGate::new(NonZeroOneU8::new(2).unwrap(), NonZeroU8::new(1).unwrap()).generate_ir(
            &[3, 4],
            &[5],
            &mut |op| ir.push(op),
            0,
        );

        XorGate::new(NonZeroOneU8::new(2).unwrap(), NonZeroU8::new(1).unwrap()).generate_ir(
            &[0, 1],
            &[6],
            &mut |op| ir.push(op),
            0,
        );

        let (a, b) = (0b1100, 0b0110);
        let mut mem = [a, b, 0, 0, 0, 0, 0];

        let mut out = [0; 2];
        interpreter::run(&ir, &mut mem, &mut [a, b], &mut out);

        assert_eq!(mem, [a, b, a & b, a | b, !(a & b), a ^ b, a ^ b])
    }
}
