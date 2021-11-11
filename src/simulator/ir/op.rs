use core::fmt;
use std::sync::Arc;

#[derive(Clone)]
pub enum IrOp {
	In {
		index: usize,
		out: usize,
	},
	Out {
		a: usize,
		index: usize,
	},
	And {
		a: usize,
		b: usize,
		out: usize,
	},
	Or {
		a: usize,
		b: usize,
		out: usize,
	},
	Xor {
		a: usize,
		b: usize,
		out: usize,
	},
	Not {
		a: usize,
		out: usize,
	},
	Andi {
		a: usize,
		i: usize,
		out: usize,
	},
	Slli {
		a: usize,
		i: u8,
		out: usize,
	},
	Srli {
		a: usize,
		i: u8,
		out: usize,
	},
	Load {
		value: usize,
		out: usize,
	},
	RunIc {
		ic: Arc<[Self]>,
		offset: usize,
		inputs: Box<[usize]>,
		outputs: Box<[usize]>,
	},
}

impl fmt::Debug for IrOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let fmt1 = |f: &mut fmt::Formatter, op, a, b| write!(f, "{:>3} = ({:<5} {:>3})", b, op, a);
		let fmt2 = |f: &mut fmt::Formatter, op, a, b, c| {
			write!(f, "{:>3} = ({:<5} {:>3} {:>3})", c, op, a, b)
		};
		match self {
			IrOp::In { index, out } => write!(f, "{:>3} < (in    {:>3})", out, index),
			IrOp::Out { a, index } => write!(f, "{:>3} > (out   {:>3})", a, index),
			IrOp::And { a, b, out } => fmt2(f, "and", a, b, out),
			IrOp::Or { a, b, out } => fmt2(f, "or", a, b, out),
			IrOp::Xor { a, b, out } => fmt2(f, "xor", a, b, out),
			IrOp::Not { a, out } => fmt1(f, "not", a, out),
			IrOp::Andi { a, i, out } => fmt2(f, "andi", a, i, out),
			IrOp::Slli { a, i, out } => fmt2(f, "slli", a, &(*i).into(), out),
			IrOp::Srli { a, i, out } => fmt2(f, "srli", a, &(*i).into(), out),
			IrOp::Load { value, out } => fmt1(f, "load", value, out),
			IrOp::RunIc { ic, offset, inputs, outputs } => write!(
				f,
				"{:>3?} (runic {:>3} {:>3p} {:>3?})",
				&inputs,
				offset,
				Arc::as_ptr(ic),
				&outputs
			),
		}
	}
}
