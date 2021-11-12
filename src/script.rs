use core::fmt;
use core::num::ParseIntError;
use core::ops::Deref;
use core::str::Chars;

#[derive(Debug)]
pub struct SExpr(Box<[Arg]>);

#[derive(Debug)]
pub enum Arg {
	SExpr(SExpr),
	Int(i64),
	Str(Box<str>),
	Symbol(Box<str>),
}

impl SExpr {
	const DECIMAL_CHARS: &'static str = "0123456789";

	pub fn parse(source: &str) -> Result<(Self, &str), ParseError> {
		let source = source.trim_start();
		if source.chars().next() != Some('(') {
			Err(ParseError::ExpectedOpenBrace)?;
		}
		Self::parse_postbrace(&mut source[1..].trim_start().chars())
	}

	fn parse_postbrace<'a>(source: &mut Chars<'a>) -> Result<(Self, &'a str), ParseError> {
		let mut args = Vec::new();
		loop {
			let c = source.next().ok_or(ParseError::ExpectedCloseBrace)?;
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
							'\\' => todo!("escaped char"),
							c => s.push(c),
						}
					}
					args.push(Arg::Str(s.into()));
				}
				//'0' => todo!("dec/bin/oct/hex num"),
				'0'..='9' => {
					let (a, f) = Self::parse_decimal(c, source)?;
					args.push(a);
					if f {
						break Ok((Self(args.into()), source.as_str()));
					}
				}
				c => {
					let mut s = String::new();
					s.push(c);
					loop {
						let c = source.as_str().chars().next();
						if " \t\n()\"".contains(c.ok_or(ParseError::ExpectedCloseBrace)?) {
							break;
						}
						s.push(source.next().unwrap());
					}
					args.push(Arg::Symbol(s.into()));
				}
			}
		}
	}

	/// Parse a decimal number.
	///
	/// # Returns
	///
	/// The argument and whether an ending brace (')') was encountered.
	fn parse_decimal(n: char, source: &mut Chars) -> Result<(Arg, bool), ParseError> {
		assert!('0' <= n && n <= '9', "digit is not decimal: '{}'", n);
		let mut n = (n as u8 - b'0').into();
		loop {
			let c = source.next().ok_or(ParseError::ExpectedCloseBrace)?;
			match c {
				' ' | '\t' | '\n' => break Ok((Arg::Int(n), false)),
				')' => break Ok((Arg::Int(n), true)),
				'0'..='9' => n = n * 10 + i64::from(c as u8 - b'0'),
				'_' => continue,
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
	InvalidDigit(char),
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
			match a {
				Arg::SExpr(s) => s.fmt(f),
				Arg::Int(n) => n.fmt(f),
				Arg::Str(s) => fmt::Debug::fmt(s, f),
				Arg::Symbol(s) => s.fmt(f),
			}?
		}
		f.write_str(")")
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn print_parse() {
		let source = "(print \"Hello, world! 123 == 345 is\" (= 123 345))";
		let fmt = source;
		let (e, s) = SExpr::parse(source).unwrap();
		assert_eq!(format!("{}", e), fmt);
		assert_eq!(s, "");
	}

	#[test]
	fn scuffed_print_parse() {
		let source = "(print(= 123 3_4_5))teehee";
		let fmt = "(print (= 123 345))";
		let (e, s) = SExpr::parse(source).unwrap();
		assert_eq!(format!("{}", e), fmt);
		assert_eq!(s, "teehee");
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
		let (e, s) = SExpr::parse(source).unwrap();
		assert_eq!(format!("{}", e), fmt);
		assert!(s.chars().all(char::is_whitespace));
	}

	#[test]
	fn fizzbuzz_parse() {
		let source = "
			(defun fizz-buzz
				(loop for i in 1 to 100 do
					(print
						(cond
							((= (% i 15) 0) \"FizzBuzz\")
							((= (% i 3) 0) \"Fizz\")
							((= (% i 5) 0) \"Buzz\")
							(t i)))))
		";
		let fmt = "(defun fizz-buzz (loop for i in 1 to 100 do (print (cond ((= (% i 15) 0) \"FizzBuzz\") ((= (% i 3) 0) \"Fizz\") ((= (% i 5) 0) \"Buzz\") (t i)))))";
		let (e, s) = SExpr::parse(source).unwrap();
		assert_eq!(format!("{:#}", e), fmt);
		assert!(s.chars().all(char::is_whitespace));
	}
}
