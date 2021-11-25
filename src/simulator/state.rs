use core::mem;

/// State of a circuit
pub struct State {
	/// Memory to write to in the next step.
	pub(super) write: Box<[usize]>,
	/// Memory to read from in the next step.
	pub(super) read: Box<[usize]>,
	/// Nexus to memory map.
	pub(super) nexus_map: Box<[usize]>,
	/// Input & mask to nexus map.
	pub(super) input_map: Box<[(usize, usize)]>,
	/// Output & mask to nexus map.
	pub(super) output_map: Box<[(usize, usize)]>,
}

impl State {
	/// Swap the read & write buffers
	pub fn swap(&mut self) {
		mem::swap(&mut self.write, &mut self.read);
	}

	/// Write the given inputs to memory.
	///
	/// # Panics
	///
	/// The inputs slice doesn't match the actual amount of inputs.
	pub fn write_inputs(&mut self, inputs: &[Value]) {
		for (i, o) in inputs.iter().enumerate() {
			let (i, mask) = self.input_map[i];
			match *o {
				Value::Set(o) => self.read[i] = o & mask,
				_ => todo!(),
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
			let (i, mask) = self.output_map[i];
			*o = Value::Set(self.read[i] & i);
		}
	}

	/// Get the value of the given nexus.
	///
	/// # Panics
	///
	/// The nexus is invalid.
	pub fn read_nexus(&self, nexus: usize) -> Value {
		Value::Set(self.read[self.nexus_map[nexus]])
	}
}

/// The state of an input or output.
#[derive(Clone, Copy, Debug)]
pub enum Value {
	Set(usize),
	Floating,
	Short,
}
