use super::*;
use crate::simulator::ir;
use serde::de::{Deserializer, Visitor};
use serde::{Deserialize, Serialize};
use std::collections::hash_map::{Entry, HashMap};
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::path::Path;
use std::sync::{Arc, Mutex};

lazy_static::lazy_static! {
	/// A collection of ICs that have been loaded, mapped to paths.
	///
	/// Used when deserializing a circuit containing ICs.
	///
	/// The key should be a full path!
	static ref ICS: Mutex<HashMap<Arc<Path>, Ic>> = Default::default();
}

#[derive(Clone)]
struct Inner {
	ir: Arc<[ir::IrOp]>,
	memory_size: usize,
	inputs: Box<[PointOffset]>,
	outputs: Box<[PointOffset]>,
	path: Arc<Path>,
}

impl Inner {
	fn from_circuit<C>(mut circuit: Circuit<C>, path: impl Into<Arc<Path>>) -> Self
	where
		C: CircuitComponent,
	{
		let (mut inputs, mut outputs) = (Vec::new(), Vec::new());

		for (c, ..) in circuit.components(Aabb::ALL) {
			if let Some(i) = c.external_input() {
				inputs.push(PointOffset::new(-(i as i8 + 1), 0));
			}
			if let Some(o) = c.external_output() {
				outputs.push(PointOffset::new(-(o as i8 + 1), 2));
			}
		}

		let (ir, memory_size) = circuit.generate_ir();

		Self {
			ir: ir.into(),
			memory_size,
			inputs: inputs.into(),
			outputs: outputs.into(),
			path: path.into(),
		}
	}
}

#[derive(Debug)]
pub enum LoadError {
	Io(std::io::Error),
	Serde(ron::de::Error),
}

impl fmt::Display for LoadError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::Io(e) => e.fmt(f),
			Self::Serde(e) => e.fmt(f),
		}
	}
}

#[derive(Clone)]
#[repr(transparent)]
pub struct Ic(Arc<Inner>);

impl Ic {
	/// Get an IC from cache or from disk.
	pub fn get_ic(path: impl Into<Arc<Path>>) -> Result<Self, LoadError> {
		let mut ics = ICS.lock().unwrap();
		let path = path.into();
		if let Some(ic) = ics.get(&path) {
			Ok(ic.clone())
		} else {
			let file = File::open(&path).map_err(LoadError::Io)?;
			drop(ics);
			let circuit: Circuit<Box<dyn CircuitComponent>> =
				ron::de::from_reader(file).map_err(LoadError::Serde)?;
			ics = ICS.lock().unwrap();
			let ic = Self(Arc::new(Inner::from_circuit(circuit, path.clone())));
			Ok(ics
				.try_insert(path, ic)
				.unwrap_or_else(|_| unreachable!())
				.clone())
		}
	}

	pub fn path(&self) -> &Path {
		&self.0.path
	}
}

impl Serialize for Ic {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		self.path().serialize(serializer)
	}
}

impl<'a> Deserialize<'a> for Ic {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'a>,
	{
		use serde::de::Error;
		let path = Box::<Path>::deserialize(deserializer)?;
		Self::get_ic(path).map_err(Error::custom)
	}
}

impl Component for Ic {
	fn input_count(&self) -> usize {
		self.0.inputs.len()
	}

	fn input_type(&self, input: usize) -> Option<InputType> {
		// TODO don't hardcode bits.
		self.0
			.inputs
			.get(input)
			.map(|_| InputType { bits: core::num::NonZeroU8::new(1).unwrap() })
	}

	fn output_count(&self) -> usize {
		self.0.outputs.len()
	}

	fn output_type(&self, output: usize) -> Option<OutputType> {
		// TODO don't hardcode bits.
		self.0
			.outputs
			.get(output)
			.map(|_| OutputType { bits: core::num::NonZeroU8::new(1).unwrap() })
	}

	fn generate_ir(
		&self,
		inputs: &[usize],
		outputs: &[usize],
		out: &mut dyn FnMut(ir::IrOp),
		memory_size: usize,
	) -> usize {
		out(ir::IrOp::RunIc {
			ic: self.0.ir.clone().into(),
			offset: memory_size,
			inputs: inputs.into(),
			outputs: outputs.into(),
		});
		self.0.memory_size
	}

	fn properties(&self) -> Box<[Property]> {
		Box::default()
	}

	fn set_property(&mut self, name: &str, value: SetProperty) -> Result<(), Box<dyn Error>> {
		Err("no properties".into())
	}
}

#[typetag::serde]
impl CircuitComponent for Ic {
	fn inputs(&self) -> Box<[PointOffset]> {
		self.0.inputs.clone()
	}

	fn outputs(&self) -> Box<[PointOffset]> {
		self.0.outputs.clone()
	}

	fn aabb(&self) -> RelativeAabb {
		let (inp, outp) = (self.inputs(), self.outputs());
		let mut iter = inp.iter().chain(outp.iter());
		let s = *iter.next().unwrap();
		let mut aabb = RelativeAabb::new(s, s);
		iter.for_each(|p| aabb = aabb.expand(*p));
		aabb.min.x -= 1;
		aabb.max.x += 1;
		aabb
	}
}
