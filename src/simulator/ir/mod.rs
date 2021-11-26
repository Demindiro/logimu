//! # Intermediate representation of circuits
//!
//! Converting circuits to an IR makes it much easier to convert it later on
//! to machine code, allowing very fast simulation of large circuits.
//!
//! An interpreter is also supplied to support architectures for which no JIT
//! has been implemented yet.

pub mod program;

pub(super) use program::Node;
pub use program::{IrOp, Program, State, Value};
