use super::*;
use crate::simulator::{GenerateIr, IrOp, Program};
use core::cmp::Ordering;
use serde::de::Deserializer;
use serde::{Deserialize, Serialize};
use std::collections::{BinaryHeap, HashMap};
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::path::Path;
use std::sync::{Arc, Mutex};
use thin_dst::ThinArc;

lazy_static::lazy_static! {
	/// A collection of ICs that have been loaded, mapped to paths.
	///
	/// Used when deserializing a circuit containing ICs.
	///
	/// The key should be a full path!
	static ref ICS: Mutex<HashMap<Arc<Path>, Ic>> = Default::default();
}

struct Inner {
	program: Program,
	inputs: Box<[PointOffset]>,
	outputs: Box<[PointOffset]>,
	input_names: Box<[Box<str>]>,
	output_names: Box<[Box<str>]>,
	input_map: Box<[usize]>,
	output_map: Box<[usize]>,
	path: Arc<Path>,
}

impl Inner {
	fn from_circuit<C>(mut circuit: Circuit<C>, path: impl Into<Arc<Path>>) -> Self
	where
		C: CircuitComponent,
	{
		let (mut inp, mut outp) = (BinaryHeap::default(), BinaryHeap::default());

		#[derive(Eq, Ord)]
		struct E(Point, usize, Box<str>);

		impl PartialEq for E {
			fn eq(&self, rhs: &Self) -> bool {
				self.0 == rhs.0
			}
		}

		impl PartialOrd for E {
			fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
				Some(self.0.cmp(&rhs.0).reverse())
			}
		}

		for (c, p, ..) in circuit.components(Aabb::ALL) {
			c.external_input()
				.map(|i| inp.push(E(p, i, c.label().unwrap_or("").into())));
			c.external_output()
				.map(|o| outp.push(E(p, o, c.label().unwrap_or("").into())));
		}

		let mut inputs = Vec::new();
		let mut outputs = Vec::new();
		let mut input_map = Vec::new();
		let mut output_map = Vec::new();
		let mut input_names = Vec::new();
		let mut output_names = Vec::new();
		for (x, E(_p, i, n)) in inp.into_iter_sorted().enumerate() {
			inputs.push(PointOffset::new(-(x as i8 + 1), 0));
			input_map.push(i);
			input_names.push(n);
		}
		for (x, E(_, o, n)) in outp.into_iter_sorted().enumerate() {
			outputs.push(PointOffset::new(-(x as i8 + 1), 2));
			output_map.push(o);
			output_names.push(n);
		}

		let program = circuit.generate_ir();

		Self {
			program,
			inputs: inputs.into(),
			outputs: outputs.into(),
			input_map: input_map.into(),
			output_map: output_map.into(),
			input_names: input_names.into(),
			output_names: output_names.into(),
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
	fn inputs(&self) -> Box<[InputType]> {
		// TODO don't hardcode bits.
		(0..self.0.inputs.len())
			.map(|_| InputType { bits: core::num::NonZeroU8::new(1).unwrap() })
			.collect()
	}

	fn outputs(&self) -> Box<[OutputType]> {
		// TODO don't hardcode bits.
		(0..self.0.outputs.len())
			.map(|_| OutputType { bits: core::num::NonZeroU8::new(1).unwrap() })
			.collect()
	}

	fn generate_ir(&self, mut gen: GenerateIr) -> usize {
		let (mut inp, mut outp) = (Vec::new(), Vec::new());
		for (from, &to) in self.0.input_map.iter().enumerate() {
			inp.resize(inp.len().max(to + 1), usize::MAX);
			inp[to] = *gen.inputs.get(from).unwrap_or(&usize::MAX);
		}
		for (from, &to) in self.0.output_map.iter().enumerate() {
			outp.resize(outp.len().max(to + 1), usize::MAX);
			outp[to] = *gen.outputs.get(from).unwrap_or(&usize::MAX);
		}
		let mut ms = 0;
		for n in self.0.program.nodes.iter() {
			let ir =
				n.ir.iter()
					.filter_map(|op| translate_mem_op(&self.0.program, op, &mut gen, &mut ms))
					.collect();
			(gen.out)(ir);
		}
		ms
	}

	fn properties(&self) -> Box<[Property]> {
		Box::default()
	}

	fn set_property(&mut self, _name: &str, _value: SetProperty) -> Result<(), Box<dyn Error>> {
		Err("no properties".into())
	}
}

/// Translate a memory address of an IC to another address for use in a larger circuit.
fn translate_mem_op(
	program: &Program,
	op: &IrOp,
	gen: &mut GenerateIr,
	max_mem: &mut usize,
) -> Option<IrOp> {
	let ad = match op {
		IrOp::CheckDirty { a, .. }
		| IrOp::Save { out: a }
		| IrOp::And { a }
		| IrOp::Or { a }
		| IrOp::Xor { a }
		| IrOp::Copy { a } => {
			let f = |a: &[_], &k| a.iter().position(|&(e, _)| e == k);
			if let Some(i) = f(&program.input_map, a) {
				gen.inputs[i]
			} else if let Some(i) = f(&program.output_map, a) {
				gen.outputs[i]
			} else {
				*max_mem = (*max_mem).max(*a + 1);
				gen.memory_size + *a
			}
		}
		IrOp::Andi { .. }
		| IrOp::Xori { .. }
		| IrOp::Slli { .. }
		| IrOp::Srli { .. }
		| IrOp::Load { .. }
		| IrOp::Read { .. } => return Some(op.clone()),
	};
	if ad == usize::MAX {
		return None;
	}
	let mut op = op.clone();
	match &mut op {
		IrOp::CheckDirty { a, node } => (*a, *node) = (ad, gen.nodes + *node),
		IrOp::Save { out: a }
		| IrOp::And { a }
		| IrOp::Or { a }
		| IrOp::Xor { a }
		| IrOp::Copy { a } => *a = ad,
		_ => unreachable!(),
	}
	Some(op)
}

#[typetag::serde]
impl CircuitComponent for Ic {
	fn input_points(&self) -> Box<[PointOffset]> {
		self.0.inputs.clone()
	}

	fn output_points(&self) -> Box<[PointOffset]> {
		self.0.outputs.clone()
	}

	fn input_name(&self, index: usize) -> Box<str> {
		self.0.input_names[index].clone()
	}

	fn output_name(&self, index: usize) -> Box<str> {
		self.0.output_names[index].clone()
	}

	fn aabb(&self, dir: Direction) -> RelativeAabb {
		let (inp, outp) = (self.input_points(), self.output_points());
		let mut iter = inp.iter().chain(outp.iter());
		let s = *iter.next().unwrap();
		let mut aabb = RelativeAabb::new(s, s);
		iter.for_each(|p| aabb = aabb.expand(*p));
		aabb.min.x -= 1;
		aabb.max.x += 1;
		dir * aabb
	}
}
