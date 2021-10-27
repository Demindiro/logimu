//! A basic interpreter meant for debugging or platforms for which no JIT has been implemented yet.

use super::*;

/// Run a sequence of instructions until the end.
pub fn run(ops: &[IrOp], memory: &mut [usize]) {
	let mut pc = 0;
	while let Some(op) = ops.get(pc) {
		pc += 1;
		match *op {
			IrOp::And { a, b, out } => memory[out] = memory[a] & memory[b],
			IrOp::Or { a, b, out } => memory[out] = memory[a] | memory[b],
			IrOp::Xor { a, b, out } => memory[out] = memory[a] ^ memory[b],
			IrOp::Not { a, out } => memory[out] = !memory[a],
		}
	}
}
