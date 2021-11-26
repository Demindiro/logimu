use super::*;
use crate::script::*;
use crate::simulator::{self, ir};
use core::cell::{Cell, RefCell};
use core::fmt;
use std::collections::HashMap;
use std::error::Error;

impl<C> Circuit<C>
where
	C: CircuitComponent,
{
	pub fn tests(&mut self) -> Result<Vec<Test<C>>, ParseError> {
		let mut src = self.script_source.trim_start();
		let mut tests = Vec::new();
		while let Some((script, s)) = SExpr::parse(src)? {
			if script.get(0).and_then(Arg::as_symbol) == Some("test") {
				// Ensure name exists and is valid.
				script
					.get(1)
					.and_then(Arg::to_value)
					.and_then(Value::into_string)
					.map(|_| tests.push(Test { circuit: self, script }));
			}
			src = s.trim_start();
		}
		Ok(tests)
	}
}

pub struct Test<'a, C>
where
	C: CircuitComponent,
{
	circuit: &'a Circuit<C>,
	script: SExpr,
}

impl<C> Test<'_, C>
where
	C: CircuitComponent,
{
	pub fn name(&self) -> &str {
		match self.script.get(1) {
			Some(Arg::Str(s)) => &**s,
			_ => unreachable!(),
		}
	}

	pub fn run(
		&self,
		state: &mut simulator::State,
		inputs: &mut [ir::Value],
		outputs: &mut [ir::Value],
		log: impl core::fmt::Write,
	) -> Result<(), TestError> {
		let (state, inputs, outputs) = (RefCell::new(state), Cell::new(inputs), Cell::new(outputs));
		let log = Cell::new(Some(log));
		let r = Runner::new(
			|r, s, f, e| {
				let get_value = |i| -> Result<_, Box<dyn Error>> {
					Ok(match e.get(i).ok_or(RunError::ExpectedArgument)? {
						Arg::SExpr(s) => r.handle(s)?,
						Arg::Symbol(n) => s.get(n).ok_or(RunError::SymbolNotDefined)?,
						v => v.to_value().unwrap(),
					})
				};
				match f {
					"in" => {
						let label = get_value(1)?.into_string().ok_or(RunError::ExpectedStr)?;
						let value = get_value(2)?.as_int().ok_or(RunError::ExpectedInt)?;
						for (c, ..) in self.circuit.components(Aabb::ALL) {
							if let Some(i) = c.external_input() {
								if c.label() == Some(&label) {
									let inp = inputs.take();
									inp[i] = ir::Value::Set(value as usize);
									inputs.set(inp);
									return Ok(Value::None);
								}
							}
						}
						Err(format!("input '{}' not found", label).into())
					}
					"out" => {
						let label = get_value(1)?.into_string().ok_or(RunError::ExpectedStr)?;
						for (c, ..) in self.circuit.components(Aabb::ALL) {
							if let Some(i) = c.external_output() {
								if c.label() == Some(&label) {
									let outp = outputs.take();
									let value = match outp[i] {
										ir::Value::Set(o) => o,
										_ => todo!(),
									} as i64;
									outputs.set(outp);
									return Ok(Value::Int(value));
								}
							}
						}
						Err(format!("output '{}' not found", label).into())
					}
					"run" => {
						let (inp, outp) = (inputs.take(), outputs.take());
						let mut s = state.borrow_mut();
						s.write_inputs(inp);
						s.run(1024);
						s.read_outputs(outp);
						(inputs.set(inp), outputs.set(outp));
						Ok(Value::None)
					}
					"print" => {
						let mut l = log.take().unwrap();
						match print_args(&mut l, r, s as &dyn Storage<_>, &e[1..]) {
							Err(PrintError::RunError(e)) => Err(e)?,
							Err(PrintError::FmtError(_)) => todo!("handle formatting errors"),
							Ok(()) => {
								l.write_char('\n').unwrap();
								log.set(Some(l));
								Ok(Value::None)
							}
						}
					}
					f => todo!("{}", f),
				}
			},
			Cell::new(HashMap::<Box<str>, _>::default()),
		);
		for e in &self.script[2..] {
			match e {
				Arg::SExpr(s) => {
					r.handle(s)
						.map_err(|e| TestError::RunError(e.to_string().into()))?;
				}
				Arg::Symbol(n) => {
					r.storage
						.get(n)
						.ok_or_else(|| TestError::RunError(Box::new(RunError::SymbolNotDefined)))?;
				}
				_ => (),
			}
		}
		Ok(())
	}
}

#[derive(Debug)]
pub enum TestError {
	RunError(Box<dyn Error>),
}

impl fmt::Display for TestError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::RunError(e) => e.fmt(f),
		}
	}
}
