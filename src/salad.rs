use ast;
use ast::{ WireValue, WireWidth, UnOpCode };
use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

/* ╔═════════════════╗
   ║ datatypes stuff ║
   ╚═════════════════╝ */

// proprietary BinOpCode to treat Concat as
// BinMaths (also don't use vanilla Error)
#[derive(Debug, Eq, PartialEq)]
pub enum BopCode
	{ Add
	, Sub
	, Mul
	, Div
	, Or
	, Xor
	, And
	, Equal
	, NotEqual
	, LessEqual
	, GreaterEqual
	, Less
	, Greater
	, LogicalAnd
	, LogicalOr
	, LeftShift
	, RightShift
	, Concat
	}

#[derive(Debug, Eq, PartialEq)]
pub enum Unmatched
	{ RegIn(String)  // e.g. @myreg[in]  -> String=myreg
	, RegOut(String) // e.g. @myreg[out] -> String=myreg
	, Input(String)  // e.g. $inputX     -> String=X
	, Output(String) // e.g. $outputX    -> String=X
	, Src(String)    // e.g. $srcX       -> String=X
	, Dst(String)    // e.g. $dstX       -> String=X
	}

#[derive(Debug, Eq, PartialEq)]
pub enum Unknown
	{ BubbledRegOut(Box<Simple>, Box<Simple>)
	, StalledRegOut(Box<Simple>, Box<Simple>)
	, Mux(Box<Simple>)
	, InSet(Box<Simple>)
	}

#[derive(Debug, Eq, PartialEq)]
pub enum Simple
	{ Literal(WireValue) // literalesque
	, Cool(String)       // "
	, Wildcard           // "
	, OneOfLogic(String, HashMap<u128, Simple>) // hashmap should only contain (boolean) literals
	, Name(String)
	, Unmatched(Unmatched)
	, Error(String)
	, BinMaths(BopCode, Box<Simple>, Box<Simple>) // these contain other Simples
	, UnMaths(UnOpCode, Box<Simple>)              // "
	, Slice(Box<Simple>, u32, u32)                // "
	, Aged(Box<Simple>)                           // "
	, Unknown(Unknown)                            // "
	}

#[derive(Debug, Eq, PartialEq)]
pub enum CoolProperty
	{ OneOf(HashSet<u128>)
	, Neq(Box<Simple>)
	}

#[derive(Debug)]
pub enum Test
	{ Test(String, Vec<Test>)
	, ValueDef(String, Vec<CoolProperty>)
	, Given(Box<Simple>, Box<Simple>)
	, Condition(Box<Simple>, Box<Simple>)
	}

pub enum EquivResult
	{ Equiv        // old "Correct"
	, Ambiguous    // old "WrongMaybe"
	, NotEquiv     // old "NotEquiv"
	, Unsimplified // old "Unknown"
	}

// I have no clue how this memory layout looks like
pub enum TestResult
	{ Condition { res: EquivResult, got: Simple, expected: Simple }
	, Test(String, Vec<TestResult>)
	}

/* ╔════════════════════╗
   ║ fmt::Display stuff ║
   ╚════════════════════╝ */

impl fmt::Display for BopCode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match self
			{ BopCode::Add          => "+"
			, BopCode::Sub          => "-"
			, BopCode::Mul          => "*"
			, BopCode::Div          => "/"
			, BopCode::Or           => "|"
			, BopCode::Xor          => "^"
			, BopCode::And          => "&"
			, BopCode::Equal        => "=="
			, BopCode::NotEqual     => "!="
			, BopCode::LessEqual    => "<="
			, BopCode::GreaterEqual => ">="
			, BopCode::Less         => "<"
			, BopCode::Greater      => ">"
			, BopCode::LogicalAnd   => "&&"
			, BopCode::LogicalOr    => "||"
			, BopCode::LeftShift    => "<<"
			, BopCode::RightShift   => ">>"
			, BopCode::Concat       => "<concat>"
			}
		)
	}
}

impl fmt::Display for UnOpCode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match self
			{ UnOpCode::Plus       => "+"
			, UnOpCode::Negate     => "-"
			, UnOpCode::Complement => "~"
			, UnOpCode::Not        => "!"
			}
		)
	}
}

fn disp_aged(n: u32, x: &Simple) -> (u32, &Simple) {
	match x
	{ Simple::Aged(x) => disp_aged(n+1, x)
	, x => (n, x)
	}
}

impl fmt::Display for Simple {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Simple::*;
		use self::Unmatched::*;
		match self
			{ Literal(WireValue{bits, width}) => match width
				{ WireWidth::Bits(w)   => write!(f, "({}):{}", w, bits)
				, WireWidth::Unlimited => write!(f, "inf:{}", bits)
				}
			, Cool(s) => write!(f, "{}", s)
			, Wildcard => write!(f, "???")
			, OneOfLogic(_, _) => todo!()
			, Name(s) => write!(f, "{}", s)
			, Unmatched(x) => match x
				{ RegIn(s)  => write!(f, "regin:{}", s)
				, RegOut(s) => write!(f, "regout:{}", s)
				, Input(s)  => write!(f, "$input{}", s)
				, Output(s) => write!(f, "$output{}", s)
				, Src(s)    => write!(f, "$src{}", s)
				, Dst(s)    => write!(f, "$dst{}", s)
				}
			, Error(s) => write!(f, "{}", s)
			, BinMaths(op, l, r) => write!(f, "<{} {} {}>", l, op, r)
			, UnMaths(op, x) => write!(f, "{}({})", op, x)
			, Slice(x, lo, hi) => write!(f, "{}[{}..{}]", x, lo, hi)
			, Aged(x) => match disp_aged(1, x)
				{ (n, x) => write!(f, "[{} {}]", n, x)
				}
			, Simple::Unknown(_) => todo!()
			}
	}
}

/* ╔══════════════════════╗
   ║ simplification logic ║
   ╚══════════════════════╝ */

pub fn ageflat(x: Simple) -> Simple {
	use self::Simple::*;
	use self::Unknown::*;
	fn distr(x: Box<Simple>) -> Box<Simple> { // not sure if I should really be doing this
		Box::new(ageflat(Aged(x)))
	}
	fn boxflat(x: Box<Simple>) -> Box<Simple> { // this seems weird but
		Box::new(ageflat(*x))
	}
	match x {
		Aged(x) => match *x // distribution
			{ BinMaths(op, l, r) => BinMaths(op, distr(l), distr(r))
			, UnMaths(op, x) => UnMaths(op, distr(x))
			, Slice(x, lo, hi) => Slice(distr(x), lo, hi)
			, Aged(_) => match ageflat(*x)
				{ Aged(x) => Aged(Box::new(Aged(x))) // it is fully flattened
				, x => ageflat(Aged(Box::new(x)))    // it can be further flattened
				}
			, Unknown(x) => Unknown( match x
				{ BubbledRegOut(x, y) => BubbledRegOut(distr(x), distr(y))
				, StalledRegOut(x, y) => StalledRegOut(distr(x), distr(y))
				, Mux(x) => Mux(distr(x))
				, InSet(x) => InSet(distr(x))
				} )
			, Literal(_) => *x
			, Cool(_)    => *x
			, Wildcard   => *x
			, _ => Aged(x)
			}
		, BinMaths(op, l, r) => BinMaths(op, boxflat(l), boxflat(r))
		, UnMaths(op, x) => UnMaths(op, boxflat(x))
		, Slice(x, lo, hi) => Slice(boxflat(x), lo, hi)
		, Unknown(x) => Unknown( match x
			{ BubbledRegOut(x, y) => BubbledRegOut(boxflat(x), boxflat(y))
			, StalledRegOut(x, y) => StalledRegOut(boxflat(x), boxflat(y))
			, Mux(x) => Mux(boxflat(x))
			, InSet(x) => InSet(boxflat(x))
			} )
		, x => x
	}
}

pub fn test(test: Test, program: Vec<ast::Statement>) {

	match test
		{ Test::Condition(l, r) => println!("{} <- {}", l, r)
		, _ => todo!()
		}

}
