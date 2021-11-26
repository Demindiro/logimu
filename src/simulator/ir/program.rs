use super::super::NexusHandle;
use crate::integer_set::IntegerSet;
use core::{fmt, mem};
use std::sync::Arc;
use thin_dst::ThinArc;

#[derive(Debug)]
pub(crate) struct Node {
	/// IR to simulate this node.
	pub(crate) ir: Box<[IrOp]>,
}

#[derive(Debug, Default)]
pub struct Program {
	/// All nodes that can affect the circuit.
	pub(crate) nodes: Box<[Node]>,
	/// The amount of memory needed to run this program.
	pub(crate) memory_size: usize,
	/// Input & mask to nexus map.
	pub(crate) input_map: Box<[(usize, usize)]>,
	/// Output & mask to nexus map.
	pub(crate) output_map: Box<[(usize, usize)]>,
	/// Input to node map.
	pub(crate) input_nodes_map: Box<[Box<[usize]>]>,
}

#[derive(Debug, Default)]
pub struct State {
	/// The program associated with this state.
	program: Arc<Program>,
	/// All nodes that need an update in the next step.
	update_dirty: IntegerSet,
	/// A hashset that will be populated with nodes to be updated on the next step.
	///
	/// This set is swapped with the dirty set at the end of each step.
	mark_dirty: IntegerSet,
	/// Memory to write to in the next step.
	pub(super) write: Box<[usize]>,
	/// Memory to read from in the next step.
	pub(super) read: Box<[usize]>,
}

impl State {
	/// Write the given inputs to memory.
	///
	/// # Panics
	///
	/// The inputs slice doesn't match the actual amount of inputs.
	pub fn write_inputs(&mut self, inputs: &[Value]) {
		for (i, o) in inputs.iter().enumerate() {
			// TODO
			if i >= self.program.input_map.len() {
				continue;
			}
			let (k, mask) = self.program.input_map[i];
			if k == usize::MAX {
				// The input doesn't map to a memory location
				continue;
			}
			let dirty;
			match *o {
				Value::Set(o) => {
					let v = o & mask;
					dirty = self.read[k] & mask != v;
					self.read[k] = v;
					self.write[k] = v;
				}
				_ => todo!(),
			}
			if dirty {
				for &i in self.program.input_nodes_map[i].iter() {
					self.update_dirty.insert(i);
				}
			}
		}
	}

	/// Read the outputs from memory.
	///
	/// # Panics
	///
	/// The outputs slice doesn't match the actual amount of outputs.
	pub fn read_outputs(&self, outputs: &mut [Value]) {
		for (i, o) in outputs.iter_mut().enumerate() {
			// TODO
			if i >= self.program.output_map.len() {
				continue;
			}
			let (i, mask) = self.program.output_map[i];
			*o = if i == usize::MAX {
				Value::Floating
			} else {
				Value::Set(self.read[i] & mask)
			};
		}
	}

	/// Get the value of the given nexus.
	///
	/// # Panics
	///
	/// The nexus is invalid.
	pub fn read_nexus(&self, nexus: NexusHandle) -> Value {
		Value::Set(self.read[nexus.index()])
	}

	/// Modify this state to be compatible with a new program whilst losing as little information
	/// as possible.
	pub fn adapt(self, program: impl Into<Arc<Program>> + AsRef<Program>) -> Self {
		let program = program.into();
		if Arc::as_ptr(&program) == Arc::as_ptr(&self.program) {
			return self;
		}
		let mut s = program.new_state();
		s.update_dirty |= self.update_dirty;
		for (r, w) in self.read.iter().zip(s.read.iter_mut()) {
			*w = *r;
		}
		s.write.copy_from_slice(&s.read);
		s
	}

	/// Step the circuit once.
	pub fn step(&mut self) -> usize {
		debug_assert!(self.mark_dirty.is_empty());
		for n in self.update_dirty.drain() {
			run(
				&self.program.nodes[n].ir,
				&self.read,
				&mut self.write,
				&mut self.mark_dirty,
			);
		}
		self.read.copy_from_slice(&self.write);
		mem::swap(&mut self.write, &mut self.read);
		mem::swap(&mut self.update_dirty, &mut self.mark_dirty);
		self.update_dirty.len()
	}

	/// Step the circuit up to n times or until no more nodes need an update.
	pub fn run(&mut self, max_iterations: usize) -> usize {
		for _ in 0..max_iterations {
			if self.step() == 0 {
				break;
			}
		}
		self.update_dirty.len()
	}
}

/// The state of an input or output.
#[derive(Clone, Copy, Debug)]
pub enum Value {
	Set(usize),
	Floating,
	Short,
}

impl Program {
	pub fn new_state(self: Arc<Self>) -> State {
		State {
			program: self.clone(),
			update_dirty: (0..self.nodes.len()).collect(),
			mark_dirty: Default::default(),
			write: (0..self.memory_size).map(|_| 0).collect(),
			read: (0..self.memory_size).map(|_| 0).collect(),
		}
	}
}

/// Run a sequence of instructions.
fn run(ops: &[IrOp], rd: &[usize], wr: &mut [usize], dirty: &mut IntegerSet) {
	let mut acc = 0;
	let mut b = 0;
	for op in ops {
		match op {
			&IrOp::CheckDirty { a, node } => {
				if wr[a] & 1 != rd[a] & 1 {
					dirty.insert(node);
				}
			}
			&IrOp::Save { out } => wr[out] = acc,
			&IrOp::And { a } => acc &= rd[a],
			&IrOp::Or { a } => acc |= rd[a],
			&IrOp::Xor { a } => acc ^= rd[a],
			&IrOp::Andi { i } => acc &= i,
			&IrOp::Xori { i } => acc ^= i,
			&IrOp::Slli { i } => acc <<= i,
			&IrOp::Srli { i } => acc >>= i,
			&IrOp::Copy { a } => acc = rd[a],
			&IrOp::Load { value } => acc = value,
			IrOp::Read { memory } => acc = *memory.slice.get(acc).unwrap_or(&0),
			&IrOp::SaveB { out } => wr[out] = b,
			&IrOp::OrB => b |= acc,
		}
	}
}

#[derive(Clone)]
pub enum IrOp {
	CheckDirty { a: usize, node: usize },
	Save { out: usize },
	And { a: usize },
	Or { a: usize },
	Xor { a: usize },
	Andi { i: usize },
	Xori { i: usize },
	Slli { i: u8 },
	Srli { i: u8 },
	Load { value: usize },
	Copy { a: usize },
	Read { memory: ThinArc<(), usize> },
	SaveB { out: usize },
	OrB,
}

impl IrOp {}

impl fmt::Debug for IrOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let fmt0 = |f: &mut fmt::Formatter, op| write!(f, "({})", op);
		let fmt1 = |f: &mut fmt::Formatter, op, a| write!(f, "({:<5} {:>3})", op, a);
		let fmt2 = |f: &mut fmt::Formatter, op, a, b| write!(f, "({:<5} {:>3} {:>3})", op, a, b);
		match self {
			IrOp::CheckDirty { a, node } => fmt2(f, "check-dirty", a, node),
			IrOp::Save { out } => fmt1(f, "save", out),
			IrOp::And { a } => fmt1(f, "and", a),
			IrOp::Or { a } => fmt1(f, "or", a),
			IrOp::Xor { a } => fmt1(f, "xor", a),
			IrOp::Andi { i } => fmt1(f, "andi", i),
			IrOp::Xori { i } => fmt1(f, "xori", i),
			IrOp::Slli { i } => fmt1(f, "slli", &(*i).into()),
			IrOp::Srli { i } => fmt1(f, "srli", &(*i).into()),
			IrOp::Copy { a } => fmt1(f, "copy", a),
			IrOp::Load { value } => fmt1(f, "load", value),
			IrOp::Read { .. } => fmt0(f, "(read [_])"),
			IrOp::SaveB { out } => fmt1(f, "save-b", out),
			IrOp::OrB => fmt0(f, "or-b"),
		}
	}
}
