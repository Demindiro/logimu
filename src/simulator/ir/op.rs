use core::fmt;

#[derive(Copy, Clone)]
pub enum IrOp {
	And { a: usize, b: usize, out: usize },
	Or { a: usize, b: usize, out: usize },
	Xor { a: usize, b: usize, out: usize },
	Not { a: usize, out: usize },
}

impl fmt::Debug for IrOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let fmt1 = |f: &mut fmt::Formatter, op, a, b| {
			write!(f, "{:>3} = ({:<4} {:>3})", b, op, a)
		};
		let fmt2 = |f: &mut fmt::Formatter, op, a, b, c| {
			write!(f, "{:>3} = ({:<4} {:>3} {:>3})", c, op, a, b)
		};
		match self {
			IrOp::And { a, b, out } => fmt2(f, "and", a, b, out),
			IrOp::Or { a, b, out } => fmt2(f, "or", a, b, out),
			IrOp::Xor { a, b, out } => fmt2(f, "xor", a, b, out),
			IrOp::Not { a, out } => fmt1(f, "not", a, out),
		}
	}
}