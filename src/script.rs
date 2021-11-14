use core::borrow::Borrow;
use core::cell::Cell;
use core::cmp::Ordering;
use core::fmt;
use core::marker::PhantomData;
use core::ops::Deref;
use core::str::Chars;
use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;

#[derive(Debug)]
pub struct SExpr(Box<[Arg]>);

#[derive(Debug)]
pub enum Arg {
	None,
	SExpr(SExpr),
	Bool(bool),
	Int(i64),
	Str(Box<str>),
	Symbol(Box<str>),
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Value {
	None,
	Bool(bool),
	Int(i64),
	Str(String),
}

impl Arg {
	pub fn to_value(&self) -> Option<Value> {
		match self {
			Self::None => Some(Value::None),
			Self::Bool(b) => Some(Value::Bool(*b)),
			Self::Int(n) => Some(Value::Int(*n)),
			Self::Str(s) => Some(Value::Str(s.to_string())),
			Self::SExpr(_) | Self::Symbol(_) => None,
		}
	}

	pub fn as_sexpr(&self) -> Option<&SExpr> {
		match self {
			Self::SExpr(s) => Some(s),
			_ => None,
		}
	}

	pub fn as_symbol(&self) -> Option<&str> {
		match self {
			Self::Symbol(s) => Some(&**s),
			_ => None,
		}
	}
}

impl fmt::Display for Arg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::None => "none".fmt(f),
			Self::SExpr(s) => s.fmt(f),
			Self::Bool(b) => b.fmt(f),
			Self::Int(n) => n.fmt(f),
			Self::Str(s) => fmt::Debug::fmt(s, f),
			Self::Symbol(s) => s.fmt(f),
		}
	}
}

macro_rules! op {
	{
		$op:ident($l:ident, $r:ident)
		$(($tl:ident, $tr:ident) => $res:ident $code:block)*
	} => {
		pub fn $op<'a>(&self, rhs: &'a Self) -> Option<Self> {
			match (self, rhs) {
				$((Self::$tl($l), Self::$tr($r)) => Some(Self::$res({ $code })),)*
				_ => None,
			}
		}
	};
}

impl Value {
	#[allow(dead_code)]
	fn type_name(&self) -> &'static str {
		match self {
			Self::None => "none",
			Self::Bool(_) => "bool",
			Self::Int(_) => "integer",
			Self::Str(_) => "string",
		}
	}

	pub fn as_bool(&self) -> Option<bool> {
		match self {
			&Self::Bool(b) => Some(b),
			_ => None,
		}
	}

	pub fn as_int(&self) -> Option<i64> {
		match self {
			&Self::Int(n) => Some(n),
			_ => None,
		}
	}

	#[allow(dead_code)]
	pub fn as_str(&self) -> Option<&str> {
		match self {
			Self::Str(s) => Some(&**s),
			_ => None,
		}
	}

	pub fn into_string(self) -> Option<String> {
		match self {
			Self::Str(s) => Some(s),
			_ => None,
		}
	}

	op! {
		checked_and(a, b)
		(Bool, Bool) => Bool { a & b }
		(Int, Int) => Int { a & b }
	}

	op! {
		checked_or(a, b)
		(Bool, Bool) => Bool { a | b }
		(Int, Int) => Int { a | b }
	}

	op! {
		checked_xor(a, b)
		(Bool, Bool) => Bool { a ^ b }
		(Int, Int) => Int { a ^ b }
	}

	pub fn checked_not(&self) -> Option<Self> {
		match self {
			&Self::Bool(a) => Some(Self::Bool(!a)),
			&Self::Int(a) => Some(Self::Int(!a)),
			_ => None,
		}
	}

	op! {
		checked_add(a, b)
		(Int, Int) => Int { a.checked_add(*b)? }
		(Str, Str) => Str { a.clone() + &*b }
	}

	op! {
		checked_sub(a, b)
		(Int, Int) => Int { a.checked_sub(*b)? }
	}

	op! {
		checked_mul(a, b)
		(Int, Int) => Int { a.checked_mul(*b)? }
	}

	op! {
		checked_div(a, b)
		(Int, Int) => Int { a.checked_div(*b)? }
	}

	op! {
		checked_rem(a, b)
		(Int, Int) => Int { a.checked_rem(*b)? }
	}
}

impl PartialOrd for Value {
	fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
		match (self, rhs) {
			(Self::None, Self::None) => Some(Ordering::Equal),
			(Self::Bool(a), Self::Bool(b)) => a.partial_cmp(b),
			(Self::Int(a), Self::Int(b)) => a.partial_cmp(b),
			(Self::Str(a), Self::Str(b)) => a.partial_cmp(b),
			_ => None,
		}
	}
}

impl From<()> for Value {
	fn from(_: ()) -> Self {
		Self::None
	}
}

impl SExpr {
	/// Parse a LISP script. Returns `Ok(None)` if no expression was found (i.e.
	/// the source is empty or consists only of comments).
	pub fn parse(mut source: &str) -> Result<Option<(Self, &str)>, ParseError> {
		loop {
			source = source.trim_start();
			if source.starts_with(";") {
				let mut it = source.chars();
				while it.next().map_or(false, |c| c != '\n') {}
				source = it.as_str();
			} else {
				break;
			}
		}
		if source.is_empty() {
			return Ok(None);
		}
		if source.chars().next() != Some('(') {
			Err(ParseError::ExpectedOpenBrace)?;
		}
		Ok(Some(Self::parse_postbrace(
			&mut source[1..].trim_start().chars(),
		)?))
	}

	fn parse_postbrace<'a>(source: &mut Chars<'a>) -> Result<(Self, &'a str), ParseError> {
		let mut args = Vec::new();
		let mut neg_number = false;
		loop {
			let c = source.next().ok_or(ParseError::ExpectedCloseBrace)?;
			let after = source.as_str().chars().next();
			match c {
				' ' | '\t' | '\n' => (),
				')' => break Ok((Self(args.into()), source.as_str())),
				'(' => {
					let (expr, _) = Self::parse_postbrace(source)?;
					args.push(Arg::SExpr(expr));
				}
				'"' => {
					let mut s = String::new();
					loop {
						match source.next().ok_or(ParseError::ExpectedEndQuote)? {
							'"' => break,
							'\\' => {
								let mut next = || source.next().ok_or(ParseError::ExpectedChar);
								let c = match next()? {
									'n' => '\n',
									'r' => '\r',
									't' => '\t',
									'"' => '"',
									'0' => '\0',
									'\\' => '\\',
									'x' => {
										let (h, l) = (next()?, next()?);
										let h = match h {
											'0'..='7' => h as u8 - b'0',
											c => Err(ParseError::InvalidEscapedChar(c))?,
										};
										let l = match l.to_ascii_lowercase() {
											'0'..='7' => l as u8 - b'0',
											'a'..='f' => l as u8 - b'a' + 10,
											c => Err(ParseError::InvalidEscapedChar(c))?,
										};
										((h << 4) | l) as char
									}
									c => Err(ParseError::InvalidEscapedChar(c))?,
								};
								s.push(c);
							}
							c => s.push(c),
						}
					}
					args.push(Arg::Str(s.into()));
				}
				'-' if after.map_or(false, |c| c.is_digit(10)) => neg_number = true,
				'0' if after.map_or(false, |c| "box".contains(c)) => {
					// Skip '0'
					source.next().unwrap();
					// Skip 'box'
					let c = source.next().unwrap();
					let (a, f) = match after.unwrap() {
						'b' => Self::parse_num(c, source, 2)?,
						'o' => Self::parse_num(c, source, 8)?,
						'x' => Self::parse_num(c, source, 16)?,
						_ => unreachable!(),
					};
					args.push(Arg::Int(neg_number.then(|| -a).unwrap_or(a)));
					neg_number = false;
					if f {
						break Ok((Self(args.into()), source.as_str()));
					}
				}
				'0'..='9' => {
					let (a, f) = Self::parse_num(c, source, 10)?;
					args.push(Arg::Int(neg_number.then(|| -a).unwrap_or(a)));
					neg_number = false;
					if f {
						break Ok((Self(args.into()), source.as_str()));
					}
				}
				';' => while source.next() != Some('\n') {},
				c => {
					let mut s = String::new();
					s.push(c);
					loop {
						let c = source.as_str().chars().next();
						if " \t\n()\";".contains(c.ok_or(ParseError::ExpectedCloseBrace)?) {
							break;
						}
						s.push(source.next().unwrap());
					}
					args.push(match &*s {
						"true" => Arg::Bool(true),
						"false" => Arg::Bool(false),
						"none" => Arg::None,
						_ => Arg::Symbol(s.into()),
					});
				}
			}
		}
	}

	/// Parse a number of the given base.
	///
	/// '_' are ignored.
	///
	/// # Returns
	///
	/// The argument and whether an ending brace (')') was encountered.
	fn parse_num(n: char, source: &mut Chars, radix: u8) -> Result<(i64, bool), ParseError> {
		let c2n = |c: char| {
			match c.to_ascii_lowercase() {
				'0'..='9' => Some(c as u8 - b'0'),
				'a'..='z' => Some(c as u8 - b'a' + 10),
				_ => None,
			}
			.and_then(|n| (n < radix).then(|| n))
			.ok_or(ParseError::InvalidDigit(c))
		};
		let mut n = c2n(n).expect("bad start digit").into();
		loop {
			let c = source.next().ok_or(ParseError::ExpectedCloseBrace)?;
			match c.to_ascii_lowercase() {
				' ' | '\t' | '\n' => break Ok((n, false)),
				')' => break Ok((n, true)),
				'_' => continue,
				'0'..='9' | 'a'..='z' => n = n * i64::from(radix) + i64::from(c2n(c)?),
				_ => Err(ParseError::InvalidDigit(c))?,
			}
		}
	}
}

#[derive(Debug)]
pub enum ParseError {
	ExpectedOpenBrace,
	ExpectedCloseBrace,
	ExpectedEndQuote,
	ExpectedChar,
	InvalidDigit(char),
	InvalidEscapedChar(char),
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::ExpectedOpenBrace => "expected opening brace '('".fmt(f),
			Self::ExpectedCloseBrace => "expected closing brace ')'".fmt(f),
			Self::ExpectedEndQuote => "expected closing quote '\"'".fmt(f),
			Self::ExpectedChar => "expected character".fmt(f),
			Self::InvalidDigit(c) => write!(f, "invalid digit '{}'", c),
			Self::InvalidEscapedChar(c) => write!(f, "invalid escape character '{}'", c),
		}
	}
}

impl AsRef<[Arg]> for SExpr {
	fn as_ref(&self) -> &[Arg] {
		self.0.as_ref()
	}
}

impl Deref for SExpr {
	type Target = [Arg];

	fn deref(&self) -> &Self::Target {
		self.as_ref()
	}
}

impl fmt::Display for SExpr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.write_str("(")?;
		for (i, a) in self.0.iter().enumerate() {
			if i > 0 {
				f.write_str(" ")?;
			}
			a.fmt(f)?;
		}
		f.write_str(")")
	}
}

impl From<bool> for Value {
	fn from(b: bool) -> Self {
		Self::Bool(b)
	}
}

impl fmt::Display for Value {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::None => f.write_str("none"),
			Self::Bool(b) => b.fmt(f),
			Self::Int(n) => n.fmt(f),
			Self::Str(s) => s.fmt(f),
		}
	}
}

pub trait Dispatcher {
	fn handle<'a>(&self, sexpr: &'a SExpr) -> Result<Value, Box<dyn Error + 'a>>;
}

impl Dispatcher for &dyn Dispatcher {
	fn handle<'a>(&self, sexpr: &'a SExpr) -> Result<Value, Box<dyn Error + 'a>> {
		(&**self).handle(sexpr)
	}
}

pub trait Storage<K>
where
	K: Borrow<str> + Eq + Hash,
{
	fn get(&self, name: &str) -> Option<Value>;

	fn set(&self, name: K, value: Value) -> bool;
}

impl<K> Storage<K> for Cell<HashMap<K, Value>>
where
	K: Borrow<str> + Eq + Hash,
{
	fn get(&self, name: &str) -> Option<Value> {
		let map = self.take();
		let v = map.get(name.borrow()).cloned();
		self.set(map);
		v
	}

	fn set(&self, name: K, value: Value) -> bool {
		let mut map = self.take();
		map.insert(name, value);
		self.set(map);
		true
	}
}

impl<K> Storage<K> for &dyn Storage<K>
where
	K: Borrow<str> + Eq + Hash,
{
	fn get(&self, name: &str) -> Option<Value> {
		(&**self).get(name)
	}
	fn set(&self, name: K, value: Value) -> bool {
		(&**self).set(name, value)
	}
}

/// A standard runner for scripts. It includes handlers for basic keywords
/// such as `if`, `:`, `=` ... while also allowing custom functions to
/// be defined.
#[derive(Default)]
pub struct Runner<H, S, K>
where
	// dyn is necessary to avoid cyclic closure reference.
	H: for<'a> Fn(&dyn Dispatcher, &S, &'a str, &'a SExpr) -> Result<Value, Box<dyn Error + 'a>>,
	S: Storage<K>,
	K: Borrow<str> + Eq + Hash,
{
	pub handler: H,
	pub storage: S,
	_marker: PhantomData<K>,
}

impl<H, S, K> Runner<H, S, K>
where
	H: for<'a> Fn(&dyn Dispatcher, &S, &'a str, &'a SExpr) -> Result<Value, Box<dyn Error + 'a>>,
	S: Storage<K>,
	K: Borrow<str> + Eq + Hash,
{
	pub fn new(handler: H, storage: S) -> Self {
		Self { handler, storage, _marker: PhantomData }
	}
}

impl<H, S, K> Dispatcher for Runner<H, S, K>
where
	H: for<'a> Fn(&dyn Dispatcher, &S, &'a str, &'a SExpr) -> Result<Value, Box<dyn Error + 'a>>,
	S: Storage<K>,
	K: Borrow<str> + Eq + Hash + From<Box<str>> + Clone,
{
	fn handle<'a>(&self, sexpr: &'a SExpr) -> Result<Value, Box<dyn Error + 'a>> {
		let f = match sexpr.get(0) {
			Some(Arg::Symbol(f)) => f,
			_ => Err(RunError::ExpectedFunction)?,
		};
		let get_value = |v: &'a _| match v {
			Arg::SExpr(s) => self.handle(s),
			Arg::Symbol(s) => self
				.storage
				.get(s)
				.ok_or_else(|| RunError::SymbolNotDefined.into()),
			v => Ok(v.to_value().unwrap()),
		};
		let cmp = |ord| {
			sexpr.get(1).map_or_else(
				|| Err(RunError::ExpectedArgument.into()),
				|base| {
					let mut prev = get_value(base)?;
					for a in &sexpr[2..] {
						let next = get_value(a)?;
						if prev.partial_cmp(&next) != Some(ord) {
							return Ok(false.into());
						}
						prev = next;
					}
					Ok(true.into())
				},
			)
		};
		let op = |op: fn(&_, &_) -> Option<_>| {
			sexpr.get(1).map_or_else(
				|| Err(RunError::ExpectedArgument.into()),
				|base| {
					let mut v = get_value(base)?;
					for a in &sexpr[2..] {
						v = (op)(&v, &get_value(a)?).ok_or(RunError::ArithemicError)?;
					}
					Ok(v)
				},
			)
		};
		let sexpr_any = |i| sexpr.get(i).ok_or(RunError::ExpectedArgument);
		let sexpr_sym = |i| {
			sexpr
				.get(i)
				.and_then(Arg::as_symbol)
				.ok_or(RunError::ExpectedSymbol)
		};

		match &**f {
			"<" => cmp(Ordering::Less),
			"=" => cmp(Ordering::Equal),
			">" => cmp(Ordering::Greater),
			"&" => op(Value::checked_and),
			"|" => op(Value::checked_or),
			"^" => op(Value::checked_xor),
			"+" => op(Value::checked_add),
			"-" => op(Value::checked_sub),
			"*" => op(Value::checked_mul),
			"/" => op(Value::checked_div),
			"%" => op(Value::checked_rem),
			"!" => {
				let v = sexpr_any(1)?;
				if sexpr.len() > 2 {
					Err(RunError::UnexpectedArgument)?;
				}
				Ok(get_value(v)?
					.checked_not()
					.ok_or(RunError::ArithemicError)?)
			}
			"cond" => {
				let mut v = Value::None;
				for a in &sexpr[1..] {
					let a = a.as_sexpr().ok_or(RunError::ExpectedSExpr)?;
					let c = a.get(0).ok_or(RunError::ExpectedArgument)?;
					if get_value(c)?.as_bool().ok_or(RunError::ExpectedBool)? {
						for a in &a[1..] {
							v = get_value(a)?;
						}
						break;
					}
				}
				Ok(v)
			}
			"for" => {
				let mut v = Value::None;
				let sym = sexpr_sym(1)?;
				let kw = sexpr_sym(2)?;
				match kw {
					"in" => todo!("for {} in", sym),
					"from" => {
						let (from, kw, to) = (sexpr_any(3)?, sexpr_sym(4)?, sexpr_any(5)?);
						if kw != "to" {
							Err(RunError::ExpectedKeyword(&["to"]))?;
						}
						let from = get_value(from)?.as_int().ok_or(RunError::ExpectedInt)?;
						let to = get_value(to)?.as_int().ok_or(RunError::ExpectedInt)?;
						let sym = K::from(sym.into());
						for i in (from..to).map(Value::Int) {
							self.storage.set(sym.clone(), i);
							for a in &sexpr[6..] {
								if let Some(a) = a.as_sexpr() {
									v = self.handle(a)?;
								}
							}
						}
					}
					_ => Err(RunError::ExpectedKeyword(&["in", "from"]))?,
				}
				Ok(v)
			}
			"assert" => {
				let a = sexpr_any(1)?;
				match get_value(a)?.as_bool() {
					Some(true) => Ok(Value::None),
					Some(false) => Err(RunError::AssertionFailed(a).into()),
					None => Err(RunError::ExpectedBool.into()),
				}
			}
			f => (self.handler)(self, &self.storage, f, sexpr),
		}
	}
}

#[derive(Debug)]
pub enum RunError<'a> {
	ExpectedFunction,
	ExpectedSExpr,
	ExpectedArgument,
	ExpectedBool,
	ExpectedSymbol,
	ExpectedInt,
	ExpectedStr,
	ExpectedKeyword(&'static [&'static str]),
	UnexpectedArgument,
	ArithemicError,
	SymbolNotDefined,
	AssertionFailed(&'a Arg),
}

impl fmt::Display for RunError<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use RunError::*;
		match self {
			ExpectedFunction => "expected function".fmt(f),
			ExpectedSExpr => "expected s-expr".fmt(f),
			ExpectedArgument => "expected argument".fmt(f),
			ExpectedBool => "expected boolean".fmt(f),
			ExpectedSymbol => "expected symbol".fmt(f),
			ExpectedInt => "expected integer".fmt(f),
			ExpectedStr => "expected string".fmt(f),
			ExpectedKeyword(kw) => {
				"expected \"".fmt(f)?;
				for (i, kw) in kw.iter().enumerate() {
					if i == kw.len() - 1 {
						"\" or \"".fmt(f)?;
					} else if i > 0 {
						"\", \"".fmt(f)?;
					}
					kw.fmt(f)?;
				}
				"\"".fmt(f)
			}
			UnexpectedArgument => "unexpected argument".fmt(f),
			ArithemicError => "arithemic error".fmt(f),
			SymbolNotDefined => "symbol not defined".fmt(f),
			AssertionFailed(a) => write!(f, "assertion failed: `{}`", a),
		}
	}
}

// TODO report issue: eliding lifetime here leads to extremely confusing error with 'static
// in Runner::handle
impl Error for RunError<'_> {}

/// Printer for s-expressions. Useful for implementing `print`.
///
/// It currently only works with the default `Runner`.
pub fn print_args<'a, 'r, W, D, S, K>(
	mut out: W,
	dispatcher: D,
	storage: S,
	args: &'a [Arg],
) -> Result<(), PrintError<'a>>
where
	W: core::fmt::Write,
	D: Dispatcher,
	S: Storage<K>,
	K: Borrow<str> + Eq + Hash + From<Box<str>> + Clone,
{
	for a in args.iter() {
		let v = if let Arg::SExpr(s) = a {
			dispatcher.handle(s).map_err(PrintError::RunError)?
		} else if let Arg::Symbol(s) = a {
			storage
				.get(s)
				.ok_or_else(|| PrintError::RunError(RunError::SymbolNotDefined.into()))?
		} else {
			a.to_value().unwrap()
		};
		out.write_fmt(format_args!("{}", v))
			.map_err(PrintError::FmtError)?;
	}
	Ok(())
}

#[derive(Debug)]
pub enum PrintError<'a> {
	RunError(Box<dyn Error + 'a>),
	FmtError(fmt::Error),
}

#[cfg(test)]
mod test {
	use super::*;

	fn run(sexpr: &SExpr) -> String {
		let out = core::cell::Cell::new(String::new());
		Runner::<_, _, Box<str>>::new(
			|r, s, f, e| match f {
				"print" => {
					let mut o = out.take();
					print_args(&mut o, r, s as &dyn Storage<_>, &e[1..]).unwrap();
					out.set(o);
					Ok(Value::None)
				}
				f => Err(format!("unknown function '{}'", f))?,
			},
			Cell::new(HashMap::default()),
		)
		.handle(sexpr)
		.unwrap();
		out.take()
	}

	#[test]
	fn print() {
		let source = "(print \"Hello, world! 123 == 345 is \" (= 123 345))";
		let fmt = source;
		let (e, s) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(format!("{}", e), fmt);
		assert_eq!(s, "");
		assert_eq!(run(&e), "Hello, world! 123 == 345 is false");
	}

	#[test]
	fn scuffed_print_parse() {
		let source = "(print(= 123 3_4_5))teehee";
		let fmt = "(print (= 123 345))";
		let (e, s) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(format!("{}", e), fmt);
		assert_eq!(s, "teehee");
		assert_eq!(run(&e), "false");
	}

	#[test]
	fn print_cond_parse() {
		let source = "
			(print
				(cond
					((< 3 1) \"Less\")
					((= 3 1) \"Equal\")
					((> 3 1) \"Greater\")))
		";
		let fmt = "(print (cond ((< 3 1) \"Less\") ((= 3 1) \"Equal\") ((> 3 1) \"Greater\")))";
		let (e, s) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(format!("{}", e), fmt);
		assert!(s.chars().all(char::is_whitespace));
		assert_eq!(run(&e), "Greater");
	}

	#[test]
	fn fizzbuzz_parse() {
		let source = "
			(for i from 1 to 100
				(print
					(cond
						((= (% i 15) 0) \"FizzBuzz\")
						((= (% i 3) 0) \"Fizz\")
						((= (% i 5) 0) \"Buzz\")
						(true i))
					\"\n\"))
		";
		let fmt = "(for i from 1 to 100 (print (cond ((= (% i 15) 0) \"FizzBuzz\") ((= (% i 3) 0) \"Fizz\") ((= (% i 5) 0) \"Buzz\") (true i)) \"\\n\"))";
		let (e, s) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(format!("{:#}", e), fmt);
		assert!(s.chars().all(char::is_whitespace));

		let expected = {
			let mut s = String::new();
			for i in 1..100 {
				match i {
					i if i % 15 == 0 => s += "FizzBuzz",
					i if i % 3 == 0 => s += "Fizz",
					i if i % 5 == 0 => s += "Buzz",
					i => s += &i.to_string(),
				}
				s.push('\n');
			}
			s
		};
		assert_eq!(run(&e), expected);
	}

	#[test]
	fn parse_neg() {
		let source = "(print -42)";
		let (e, _) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(run(&e), "-42");
	}

	#[test]
	fn parse_bin() {
		let source = "(print 0b110_10_0)";
		let (e, _) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(run(&e), "52");
	}

	#[test]
	fn parse_comment() {
		let source = "
			(print \"Comments are not \" ; printed
			)
		";
		let (e, _) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(run(&e), "Comments are not ");
		let source = "
			(print;er \"Nothing\"
			)
		";
		let (e, _) = SExpr::parse(source).unwrap().unwrap();
		assert_eq!(run(&e), "");
	}
}
