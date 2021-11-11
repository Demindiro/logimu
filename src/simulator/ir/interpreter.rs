//! A basic interpreter meant for debugging or platforms for which no JIT has been implemented yet.

use super::*;

/// Run a sequence of instructions until the end.
pub fn run(ops: &[IrOp], memory: &mut [usize], input: &[usize], output: &mut [usize]) {
	let mut pc = 0;
	while let Some(op) = ops.get(pc) {
		pc += 1;
		match op {
			&IrOp::In { index, out } => memory[out] = input[index],
			&IrOp::Out { a, index } => output[index] = memory[a],
			&IrOp::And { a, b, out } => memory[out] = memory[a] & memory[b],
			&IrOp::Or { a, b, out } => memory[out] = memory[a] | memory[b],
			&IrOp::Xor { a, b, out } => memory[out] = memory[a] ^ memory[b],
			&IrOp::Not { a, out } => memory[out] = !memory[a],
			&IrOp::Andi { a, i, out } => memory[out] = memory[a] & i,
			&IrOp::Slli { a, i, out } => memory[out] = memory[a] << i,
			&IrOp::Srli { a, i, out } => memory[out] = memory[a] >> i,
			&IrOp::Load { value, out } => memory[out] = value,
			IrOp::Read { memory: mem, address, out } => {
				memory[*out] = *mem.get(memory[*address]).unwrap_or(&0)
			}
			IrOp::RunIc { ic, offset, inputs, outputs } => {
				let mut inp = [0; 256];
				let mut outp = [0; 256];
				for (i, v) in inputs.iter().enumerate().filter(|(_, v)| **v != usize::MAX) {
					inp[i] = memory[*v];
				}
				run(&ic, &mut memory[*offset..], &inp, &mut outp);
				for (i, v) in outputs
					.iter()
					.enumerate()
					.filter(|(_, v)| **v != usize::MAX)
				{
					memory[*v] = outp[i];
				}
			}
		}
	}
}
