use ast;
use ast::{ WireValue, WireWidth };
use std::fmt;
use std::rc::Rc;
use std::iter::FromIterator;
use std::collections::HashMap;
use std::collections::HashSet;
use serde_json::json;

macro_rules! bool2val {
	($x: expr) => { Literal(WireValue{ bits: if $x {1} else {0}, width: WireWidth::Bits(1) }).rc() }
}

/* ╔═════════════════╗
   ║ datatypes stuff ║
   ╚═════════════════╝ */

// proprietary BinOpCode to treat Concat as
// BinMaths (also don't use vanilla Error)
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
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

impl BopCode {
	fn is_comm(self) -> bool {
		use self::BopCode::*;
		match self
		{ Add          => true
		, Sub          => false
		, Mul          => true
		, Div          => false
		, Or           => true
		, Xor          => true
		, And          => true
		, Equal        => true
		, NotEqual     => true
		, LessEqual    => false
		, GreaterEqual => false
		, Less         => false
		, Greater      => false
		, LogicalAnd   => true
		, LogicalOr    => true
		, LeftShift    => false
		, RightShift   => false
		, Concat       => false
		}
	}
}

// I copied and edited stuff from ast.rs because private and don't feel like editing source
fn lit_bop(code: BopCode, left: WireValue, right: WireValue) -> WireValue {
	use self::BopCode::*;
	use ast::WireWidth::*;

	let width = match code
		{ LogicalAnd | LogicalOr
		| Equal      | NotEqual
		| Less       | LessEqual
		| Greater    | GreaterEqual => Bits(1) // BooleanCombine, BooleamFromEqualWidth

		, Add | Sub
		| Mul | Div => left.width.max(right.width) // EqualWidthWeak

		, Concat => match (left.width, right.width)
			{ (Bits(lw), Bits(rw)) => Bits(lw+rw)
			, _ => panic!("concat with an infinite width")
			}

		, _ => match left.width.combine(right.width) // EqualWidth
			{ Some(x) => x
			, _ => panic!("RuntimeMismatchedWidths")
			}
		};

	let btv = |x| if x { 1 } else { 0 };

	let f = |l: u128, r: u128| match code
		{ Add          => l.wrapping_add(r)
		, Sub          => l.wrapping_sub(r)
		, Mul          => l.wrapping_mul(r)
		, Div          => l.wrapping_div(r) // FIXME: handle divide-by-zero
		, Or           => l | r
		, Xor          => l ^ r
		, And          => l & r
		, Equal        => btv(l == r)
		, NotEqual     => btv(l != r)
		, Less         => btv(l <  r)
		, LessEqual    => btv(l <= r)
		, Greater      => btv(l >  r)
		, GreaterEqual => btv(l >= r)
		, LogicalAnd   => btv(l != 0 && r != 0)
		, LogicalOr    => btv(l != 0 || r != 0)
		, LeftShift =>
			if r >= 128 {
				0
			} else {
				l.wrapping_shl(r as u32)
			}
		, RightShift =>
			if r >= 128 {
				0
			} else {
				l.wrapping_shr(r as u32)
			}
		, Concat => r | (l << match right.width { Bits(x) => x, _ => panic!("concat with an infinite width") })
		};

	left.op(right, f, width)
}

impl From<ast::BinOpCode> for BopCode {
	fn from(code: ast::BinOpCode) -> Self {
		use ast::BinOpCode;
		match code
		{ BinOpCode::Add          => BopCode::Add
		, BinOpCode::Sub          => BopCode::Sub
		, BinOpCode::Mul          => BopCode::Mul
		, BinOpCode::Div          => BopCode::Div
		, BinOpCode::Or           => BopCode::Or
		, BinOpCode::Xor          => BopCode::Xor
		, BinOpCode::And          => BopCode::And
		, BinOpCode::Equal        => BopCode::Equal
		, BinOpCode::NotEqual     => BopCode::NotEqual
		, BinOpCode::LessEqual    => BopCode::LessEqual
		, BinOpCode::GreaterEqual => BopCode::GreaterEqual
		, BinOpCode::Less         => BopCode::Less
		, BinOpCode::Greater      => BopCode::Greater
		, BinOpCode::LogicalAnd   => BopCode::LogicalAnd
		, BinOpCode::LogicalOr    => BopCode::LogicalOr
		, BinOpCode::LeftShift    => BopCode::LeftShift
		, BinOpCode::RightShift   => BopCode::RightShift
		, BinOpCode::Error        => panic!("unexpected BinOpCode::Error")
		}
	}
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum UopCode
	{ Plus
	, Negate
	, Complement
	, Not
	}

fn lit_uop(op: UopCode, x: WireValue) -> WireValue {
	use self::UopCode::*;
	let raw = match op
		{ Plus       => x.bits
		, Negate     => !x.bits + 1
		, Complement => !x.bits
		, Not        => if x.bits != 0 { 0 } else { 1 }
		};
	WireValue
		{ bits: raw & x.width.mask()
		, width: if op == Not { WireWidth::Bits(1) } else { x.width }
		}
}

impl From<ast::UnOpCode> for UopCode {
	fn from(code: ast::UnOpCode) -> Self {
		use ast::UnOpCode;
		match code
		{ UnOpCode::Plus       => UopCode::Plus
		, UnOpCode::Negate     => UopCode::Negate
		, UnOpCode::Complement => UopCode::Complement
		, UnOpCode::Not        => UopCode::Not
		}
	}
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Unreplaced
	{ RegIn(String)  // e.g. @myreg[in]  -> String=myreg
	, RegOut(String) // e.g. @myreg[out] -> String=myreg
	, Input(String)  // e.g. $inputX     -> String=X
	, Output(String) // e.g. $outputX    -> String=X
	, Dst(String)    // e.g. $dstX       -> String=X
	, Src(String)    // e.g. $srcX       -> String=X
	, Value(String)  // e.g. #val        -> String=val
	}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Unknown
	{ BubbledRegOut(Rc<Simple>, Rc<Simple>)
	, StalledRegOut(Rc<Simple>, Rc<Simple>)
	, Mux(Rc<Simple>)
	, InSet(Rc<Simple>)
	}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Simple
	{ Literal(WireValue)                  // literalesque
	, Cool(String, Rc<Vec<CoolProperty>>) // "
	, Wildcard                            // "
	, Name(String)
	, OneOfLogic(String, Vec<(u128, Rc<Simple>)>)
	, Unreplaced(Unreplaced)
	, Error(String)
	, BinMaths(BopCode, Rc<Simple>, Rc<Simple>) // these act like containers for other Simples
	, UnMaths(UopCode, Rc<Simple>)              // "
	, Slice(Rc<Simple>, u8, u8)                 // "
	, Aged(Rc<Simple>)                          // "
	, Unknown(Unknown)                          // " ?
	}

impl Simple {
	pub fn rc(self) -> Rc<Self> {
		Rc::new(self)
	}
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum CoolProperty
	{ OneOf(Vec<u128>)
	, Neq(Rc<Simple>)
	}

#[derive(Debug, Clone)]
pub enum MatchType
	{ Regread(String, String)
	, Regwrite(String, String)
	, Any(Rc<Simple>, Rc<Simple>)
	}

#[derive(Debug)]
pub enum Test
	{ Test(String, Vec<Test>)
	, ValueDef(String, Vec<CoolProperty>)
	, Given(Rc<Simple>, Rc<Simple>)
	, Condition(Rc<Simple>, Rc<Simple>)
	, Match(MatchType)
	}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum EquivResult
	{ Equiv        // old "Correct"
	, Ambiguous    // old "WrongMaybe" -- essentially "I don't know"
	, NotEquiv     // old "NotEquiv"
	, Unsimplified // old "Unknown"    -- essentially Ambiguous but specifically because something didn't simplify
	}

impl EquivResult {
	fn ce(self, x: EquivResult) -> bool { // is self >= x in terms of correctness/equivalence?
		use self::EquivResult::*;
		match self
		{ Equiv        => true
		, Ambiguous    => x != Equiv
		, NotEquiv     => x == NotEquiv || x == Unsimplified
		, Unsimplified => x == Unsimplified
		}
	}
	fn cmax(self, x: EquivResult) -> EquivResult {
		if self.ce(x) { self } else { x }
	}
	fn ke(self, x: EquivResult) -> bool { // sorting Equiv > NotEquiv > Ambiguous > Unsimplified
		use self::EquivResult::*;
		match self
		{ Equiv        => true
		, NotEquiv     => x != Equiv
		, Ambiguous    => x == Ambiguous || x == Unsimplified
		, Unsimplified => x == Unsimplified
		}
	}
}

#[derive(Debug)]
pub enum TestResult
	{ Condition { res: EquivResult, gotstr: String, expectedstr: String /*got: Rc<Simple>, expected: Rc<Simple>*/ } // want to show a bunch of simplification info without storing literally everything
	, Test(String, Vec<Rc<TestResult>>)
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

impl fmt::Display for UopCode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::UopCode::*;
		write!(f, "{}", match self
			{ Plus       => "+"
			, Negate     => "-"
			, Complement => "~"
			, Not        => "!"
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

impl fmt::Display for Unreplaced {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Unreplaced::*;
		match self
		{ RegIn(s)  => write!(f, "regin:{}", s)
		, RegOut(s) => write!(f, "regout:{}", s)
		, Input(s)  => write!(f, "$input{}", s)
		, Output(s) => write!(f, "$output{}", s)
		, Src(s)    => write!(f, "$src{}", s)
		, Dst(s)    => write!(f, "$dst{}", s)
		, Value(s)  => write!(f, "#'{}", s) // the ' differentiates it from Cool
		}
	}
}

impl fmt::Display for Simple {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Simple::*;
		use self::Unknown::*;
		match self
			{ Literal(WireValue{bits, width}) => match width
				{ WireWidth::Bits(w)   => write!(f, "({}):{}", w, bits)
				, WireWidth::Unlimited => write!(f, "inf:{}", bits)
				}
			, Cool(s, _) => write!(f, "#{}", s)
			, Wildcard => write!(f, "???")
			, OneOfLogic(name, xs) =>
				{ write!(f, "[oneof#{}: {}]", name, xs.iter().map(|(x, y)| format!("{}->{}", x, y)).reduce(|a, b| format!("{}, {}", a, b)).unwrap_or("empty".to_string()))
				}
			, Name(s) => write!(f, "{}", s)
			, Unreplaced(x) => write!(f, "{}", x)
			, Error(s) => write!(f, "{}", s)
			, BinMaths(op, l, r) => write!(f, "<{} {} {}>", l, op, r)
			, UnMaths(op, x) => write!(f, "{}({})", op, x)
			, Slice(x, lo, hi) => write!(f, "{}[{}..{}]", x, lo, hi)
			, Aged(x) => match disp_aged(1, x)
				{ (n, x) => write!(f, "[{} {}]", n, x)
				}
			, Simple::Unknown(x) => match x
				{ BubbledRegOut(x, y) => write!(f, "Unknown bubbled {} because {}", x, y)
				, StalledRegOut(x, y) => write!(f, "Unknown stalled {} because {}", x, y)
				, Mux(x)   => write!(f, "Unknown mux because {}", x)
				, InSet(x) => write!(f, "Unknown inset because {}", x)
				}
			}
	}
}

impl fmt::Display for EquivResult {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(self, f)
	}
}

impl TestResult {
	pub fn to_json(&self) -> serde_json::Value {
		match &self {
			TestResult::Test(name, rs) => json!({
				"name": name,
				"res": match rs.iter().filter_map(|x| if let TestResult::Condition{ res, .. } = *(*x) { Some(res) } else { None }).reduce(|a, b| if a.ce(b) { b } else { a })
					{ Some(x) => x.to_string()
					, None => "Empty".to_string()
					},
				"conditions": rs.iter().filter_map(|x|
					if let TestResult::Condition{ .. } = *(*x) {
						Some(x.to_json())
					} else {
						None
					}).collect::<Vec<serde_json::Value>>(),
					"sub": rs.iter().filter_map(|x|
						if let TestResult::Test(_, _) = *(*x) {
						Some(x.to_json())
					} else {
						None
					}).collect::<Vec<serde_json::Value>>(),
			}),
			TestResult::Condition{res, gotstr, expectedstr} => json!({
				"res": res.to_string(),
				"got": gotstr,
				"expected": expectedstr,
			}),
		}
	}
}

/* ╔══════════════════════╗
   ║ simplification logic ║
   ╚══════════════════════╝ */

pub fn ageflat(root: Rc<Simple>) -> Rc<Simple> {
	use self::Simple::*;
	use self::Unknown::*;

	macro_rules! distr {
		($x: expr) => { ageflat(Aged(Rc::clone($x)).rc()) }
	}
	macro_rules! f {
		($x: expr) => { ageflat(Rc::clone($x)) }
	}

	match &*root {
		Aged(x) => match &*(*x) // distribution
			{ BinMaths(op, l, r) => BinMaths(*op, distr!(l), distr!(r)).rc()
			, UnMaths(op, x)     => UnMaths(*op, distr!(x)).rc()
			, Slice(x, lo, hi)   => Slice(distr!(x), *lo, *hi).rc()
			, Aged(_) =>
				{
					let flat_again = f!(x);
					match &*flat_again
					{ Aged(_) => Aged(flat_again).rc() // it is fully flattened
					, _       => ageflat(Aged(flat_again).rc())              // it can be further flattened
					}
				}
			, Unknown(x) => Unknown( match x
				{ BubbledRegOut(x, y) => BubbledRegOut(distr!(x), distr!(y))
				, StalledRegOut(x, y) => StalledRegOut(distr!(x), distr!(y))
				, Mux(x) => Mux(distr!(x))
				, InSet(x) => InSet(distr!(x))
				} ).rc()
			, Literal(_) => Rc::clone(x)
			, Cool(..)   => Rc::clone(x)
			, Simple::Wildcard   => Rc::clone(x)
			, _ => root
			}
		, BinMaths(op, l, r) => BinMaths(*op, f!(l), f!(r)).rc()
		, UnMaths(op, x)     => UnMaths(*op, f!(x)).rc()
		, Slice(x, lo, hi)   => Slice(f!(x), *lo, *hi).rc()
		, Unknown(x) => Unknown( match x
			{ BubbledRegOut(x, y) => BubbledRegOut(f!(x), f!(y))
			, StalledRegOut(x, y) => StalledRegOut(f!(x), f!(y))
			, Mux(x) => Mux(f!(x))
			, InSet(x) => InSet(f!(x))
			} ).rc()
		, _ => root
	}
}

// I hope the lifeline thing makes sense
struct Program<'a>
	{ assignments : HashMap<String, &'a ast::SpannedExpr>
	,     regouts : HashMap<String, (char, String, &'a ast::SpannedExpr)>
	,      widths : HashMap<String, WireWidth>
	,    outchars : HashSet<char>
	,      regins : HashSet<String>
	}

struct EvalState
	{ givens : HashMap<Rc<Simple>, Rc<Simple>>
	// should probably get rid of this struct?
	}

type Memo = HashMap<Rc<Simple>, Rc<Simple>>;

#[derive(Clone, Copy)]
struct Lanz // thing to generate "full path" of recursed Simples
	{ age: u32
	}

impl Lanz {
	fn age(&self) -> Lanz {
		Lanz {age: self.age+1}
	}
	fn fullpath(&self, x: Rc<Simple>) -> Rc<Simple> {
		(0..self.age).fold(x, |x, _| Simple::Aged(x).rc())
	}
}

trait Simplifiable {
	fn simplify(&self, _: &Program, _: &EvalState, memo: &mut Memo, lanz: Lanz, _: bool) -> Rc<Simple>;
}

impl Simplifiable for ast::SpannedExpr {
	fn simplify(&self, p: &Program, state: &EvalState, memo: &mut Memo, lanz: Lanz, b: bool) -> Rc<Simple> {
		self.expr.simplify(p, state, memo, lanz, b)
	}
}

impl Simplifiable for ast::Expr {
	fn simplify(&self, p: &Program, state: &EvalState, memo: &mut Memo, lanz: Lanz, er: bool) -> Rc<Simple> {
		use ast::Expr::*;
		use self::Simple::*;

		macro_rules! f {
			($x: expr) => { $x.simplify(p, state, memo, lanz, er) }
		}
		macro_rules! s {
			($x: expr) => { s_simplify(p, state, memo, lanz, $x, er) }
		}

		match self {
			Constant(v) => Literal(*v).rc(),
			BinOp(op, x, y) => {
				let xx = f!(x); // need to do this so uses memo once at a time or something
				let yy = f!(y);
				s!(BinMaths(BopCode::from(*op), xx, yy).rc())
			},
			UnOp(op, x) => {
				let xx = f!(x);
				s!(UnMaths(UopCode::from(*op), xx).rc())
			},
			NamedWire(name) => s!(Name(name.to_string()).rc()),
			BitSelect{ from, low, high } => {
				let xx = f!(from);
				s!(Slice(xx, *low, *high).rc())
			},
			Concat(x, y) => {
				let xx = f!(x);
				let yy = f!(y);
				s!(BinMaths(BopCode::Concat, xx, yy).rc())
			},
			InSet(e_x, e_xs) => {
				// TODO: cool logic
				let x = f!(e_x);
				let xs : Vec<Rc<Simple>> = e_xs.iter().map(|x| f!(x)).collect();

				xs.iter().map(|thing| sbin(BopCode::Equal, Rc::clone(&x), Rc::clone(thing)))
					.fold(bool2val!(false), |a, b| sbin(BopCode::Or, a, b))
			},
			Mux(opts) => {
				// TODO: cool logic
				for ast::MuxOption{ condition, value } in opts {
					let cond = f!(condition);
					match &*cond
					{ Literal(WireValue{bits:0, ..}) => continue
					, Literal(_) => return f!(value)
					, _ => return Unknown(self::Unknown::Mux(cond)).rc()
					}
				}
				Simple::Error("mux did not select branch".to_string()).rc()
			},
			ast::Expr::Error => panic!("unexpected Expr::Error"),
		}
	}
}

fn sbin_commshort(op: BopCode, l: Rc<Simple>, r: Rc<Simple>) -> Option<Rc<Simple>> {
	use self::Simple::*;
	use self::BopCode::*;
	use ast::WireWidth::*;

	if let Literal(x) = *l {
		return match (op, x)
		{ (Or, WireValue{ bits: 0, .. }) => Some(r)              //  0 | x = x
		, (Or, WireValue{ bits: 1, width: Bits(1) }) => Some(l)  // ~0 | x = ~0
		, (And, WireValue{ bits: 0, .. }) => Some(l)             //  0 & x = 0
		, (And, WireValue{ bits: 1, width: Bits(1) }) => Some(r) // ~0 & x = x
		, (LogicalOr, WireValue{ bits: 0, .. }) => Some(r)                 // false || x = x
		, (LogicalOr, WireValue{ bits: _, .. }) => Some(bool2val!(true))   //  true || x = true
		, (LogicalAnd, WireValue{ bits: 0, .. }) => Some(bool2val!(false)) // false && x = false
		, (LogicalAnd, WireValue{ bits: _, .. }) => Some(r)                //  true && x = x
		, (Add, WireValue{ bits: 0, .. }) => Some(r) //  x + 0 = x
		//, (Or, WireValue{ bits: _, .. }) => Some(l) // 1 | x
		, _ => None
		}
	}

	let lr = (&*l, &*r);

	// this can technically go in sbin to avoid checking twice
	// but I don't want to write out the full default failed-value thing
	if let (OneOfLogic(xname, xs), OneOfLogic(yname, ys)) = lr {
		if xname != yname { // cannot compare different values' OneOfLogics
			return None
		}
		// == LOGIC WARNING ==
		// assumes that xs and ys have same keys in same order
		// because they have the same name <=> were produced by
		// the same Oneof(..). But I'm guessing you can
		// (re)assign values (which I should probably check against)
		// to the same name and break all this logic. luckily
		// as long as the test-writer writes tests well
		// this should not happen.
		if let Or | Xor | And | Equal | NotEqual | LogicalAnd | LogicalOr = op {
			return Some(ool_simplify(OneOfLogic(xname.to_string(), xs.iter().zip(ys.iter()).map(|((n, x), (_, y))| (*n, sbin(op, Rc::clone(x), Rc::clone(y)))).collect()).rc()))
		} else {
			return None
		}
	}

	None
}

fn sbin(op: BopCode, l: Rc<Simple>, r: Rc<Simple>) -> Rc<Simple> {
	use self::Simple::*;
	use self::BopCode::*;

	let lr = (&*l, &*r);

	if let (Literal(x), Literal(y)) = lr {
		return Literal(lit_bop(op, *x, *y)).rc()
	}

	if let (Cool(xname, _), Cool(yname, _)) = lr {
		if xname == yname {
			return Literal(WireValue{bits:1, width:WireWidth::Bits(1)}).rc()
		}
	}

	// this code makes me sad
	if let Cool(name, props) = &*l {
		for prop in props.iter() {
			match prop {
				CoolProperty::OneOf(ns) => {
					if let Literal(_) = &*r {
						return OneOfLogic(name.to_string(), ns.iter().map(|n| (*n, sbin(op, Literal(WireValue { bits: *n, width: WireWidth::Unlimited }).rc(), Rc::clone(&r)))).collect()).rc()
					}
				},
				CoolProperty::Neq(x) => {
					// there are probably misc other simplifications you could
					// do with op=something other than Equal or NotEqual but..
					if let Equal | NotEqual = op {
						if let Literal(WireValue{bits, ..}) = *sbin(Equal, Rc::clone(x), Rc::clone(&r)) {
							// bits represents R == (the thing that is not L) <=> whether L is neq R
							return match op
							{ Equal => bool2val!(bits == 0)
							, _     => bool2val!(bits != 0)
							}
						}
					}
				},
			}
		}
	}

	// I literally copied this from above and changed the orders
	if let Cool(name, props) = &*r {
		for prop in props.iter() {
			match prop {
				CoolProperty::OneOf(ns) => {
					if let Literal(_) = &*l {
						return OneOfLogic(name.to_string(), ns.iter().map(|n| (*n, sbin(op, Rc::clone(&r), Literal(WireValue { bits: *n, width: WireWidth::Unlimited }).rc()))).collect()).rc()
					}
				},
				CoolProperty::Neq(x) => {
					// there are probably misc other simplifications you could
					// do with op=something other than Equal or NotEqual but..
					if let Equal | NotEqual = op {
						if let Literal(WireValue{bits, ..}) = *sbin(Equal, Rc::clone(x), Rc::clone(&l)) {
							// bits represents L == (the thing that is not R) <=> whether R is neq L
							return match op
							{ Equal => bool2val!(bits == 0)
							, _     => bool2val!(bits != 0)
							}
						}
					}
				},
			}
		}
	}

	if let Some(x) = sbin_commshort(op, Rc::clone(&l), Rc::clone(&r)) {
		x
	} else if let Some(x) = sbin_commshort(op, Rc::clone(&r), Rc::clone(&l)) {
		x
	} else {
		BinMaths(op, l, r).rc()
	}
}

fn sun(op: UopCode, x: Rc<Simple>) -> Rc<Simple> {
	use self::Simple::*;
	match &*x
	{ Literal(x) => Literal(lit_uop(op, *x)).rc()
	, _ => UnMaths(op, x).rc()
	}
}

fn deage(x: Rc<Simple>) -> Option<Rc<Simple>> {
	use self::Simple::*;
	match &*x
	{ Aged(x)    => Some(Rc::clone(x))
	, Literal(_) => Some(x)
	, Cool(..)   => Some(x)
	, Wildcard   => Some(x)
	, Slice(x, lo, hi) => match deage(Rc::clone(x))
		{ Some(x) => Some(Slice(x, *lo, *hi).rc())
		, None => None
		}
	, _ => None // does not cover maths but I don't think will ever give maths
	}
}

fn is_aged(x: &Rc<Simple>) -> bool {
	use self::Simple::*;
	match &*(*x)
	{ Aged(_) => true
	, BinMaths(_, l, r) => is_aged(l) || is_aged(r)
	, UnMaths(_, x) => is_aged(x)
	, Slice(x, _, _) => is_aged(x)
	, _ => false
	}
}

fn rawslice(x: u128, lo: u8, hi: u8) -> u128{
	let mask = (!0) >> (128 - (hi - lo));
	x.wrapping_shr(lo as u32) & mask
}

fn ool_simplify(x: Rc<Simple>) -> Rc<Simple> {
	if let Simple::OneOfLogic(_, vals) = &*x {
		let (_, fst) = vals.first().expect("empty oneoflogic");
		if vals.iter().all(|(_, x)| x == fst) {
			Rc::clone(fst)
		} else {
			x
		}
	} else {
		x
	}
}

fn s_simplify(p: &Program, state: &EvalState, memo: &mut Memo, lanz: Lanz, simple: Rc<Simple>, er: bool) -> Rc<Simple> { // er: expand regouts
	use self::Simple::*;

	macro_rules! f {
		($x: expr) => { s_simplify(p, state, memo, lanz, $x, er) }
	}
	macro_rules! simp {
		($x: expr) => { $x.simplify(p, state, memo, lanz, er) }
	}
	macro_rules! last {
		($x: expr) => { s_simplify(p, state, memo, lanz, Aged($x).rc(), er) }
	}
	macro_rules! lastname {
		($n: expr) => { last!(Name($n).rc()) }
	}

	// should re-simplify anything that *could* have expanded.

	if let Some(x) = state.givens.get(&simple) { // given
		return f!(Rc::clone(x))
	}
	if let Some(x) = memo.get(&lanz.fullpath(Rc::clone(&simple))) { // memo
		return Rc::clone(x)
	}

	let simple2 = Rc::clone(&simple);

	let got = match &*simple {
		Slice(x, lo, hi) => {
			match &*f!(Rc::clone(x))
			{ Name(name) =>
				// subslice of given slice (very cool code!)
				if let Some(x) = state.givens.iter().filter_map(|(k, v)| {
					if let Slice(y, l, h) = &*(*k) {
						if let Name(n) = &*(*y) {
							if *n == *name && *l <= *lo && *h >= *hi {
								return Some(Slice(Rc::clone(v), *lo, *hi).rc())
							}
						}
					}
					None
				}).next() { f!(x) } else { simple }
			// Todo: check bounds more
			, &Literal(WireValue{ bits, width }) => Literal(WireValue{ bits: rawslice(bits, *lo, *hi), width }).rc()
			, Slice(x, l, h) =>
				if (*l + *lo) > *h || (*l + *hi) > *h {
					panic!("slice of slice out of bounds")
				} else {
					Slice(Rc::clone(x), *l + *lo, *l + *hi).rc()
				}
			, _ => simple
			}
		},
		OneOfLogic(..) => {
			ool_simplify(simple) // will this ever actually simplify anything?
		},
		Name(n) => {
			let got = if let Some(e) = p.assignments.get(n) {
				simp!(e)
			} else if let Some((c, inname, defval)) = p.regouts.get(n) {
				if !er {
					simple // do not simplify regouts (for matching purposes)
				} else if !state.givens.iter().any(|(k, v)| is_aged(k) || is_aged(v)) {
					simp!(defval) // reached edge of age
				} else {
					let bubble = lastname!(format!("bubble_{}", c));
					match *bubble {
						Literal(WireValue{ bits: 0, ..}) => {

							let stall  = lastname!(format!("stall_{}", c));

							match *stall {
								Literal(WireValue{ bits: 0, ..}) => lastname!(inname.to_string()), // not bubbled or stalled
								Literal(_) => last!(simple), // stalled
								_ => Unknown(self::Unknown::StalledRegOut(simple, stall)).rc(),
							}
						},
						Literal(_) => simp!(defval), // bubbled
						_ => Unknown(self::Unknown::BubbledRegOut(simple, bubble)).rc(),
					}
				}
			} else {
				if let "mem_writebit" = n.as_str() {
					return bool2val!(false)
				}
				if let "reg_dstE" | "reg_dstM" = n.as_str() {
					return Literal(WireValue{ bits: 15, width: WireWidth::Bits(4) }).rc()
				}
				let lenn = n.chars().count();
				if (lenn == 7 && n.starts_with("stall_")) || (lenn == 8 && n.starts_with("bubble_")) {
					let c = n.chars().rev().next().expect("oh no!");
					if p.outchars.contains(&c) {
						return bool2val!(false)
					}
				}
				simple
			};

			got // TODO: squash(?) (IMPORTANT:: if you do squash, you need to add builtin widths).
			// am honestly not a big a fan of (assignment) squashing because I think it only differs
			// in that one really weird thing where -8 = 8 for 3-bit number. will revisit some time.
		},
		BinMaths(op, x, y) => {
			let l = f!(Rc::clone(x));
			let r = f!(Rc::clone(y));
			sbin(*op, l, r)
		},
		UnMaths(op, x) => {
			sun(*op, f!(Rc::clone(x)))
		},
		Aged(x) => {
			// not pretty but probably not many givens so not real worry (maybe)?
			let aged_givens : HashMap<Rc<Simple>, Rc<Simple>> = HashMap::from_iter(
				state.givens.iter().filter_map(
					|(x, y)| match (deage(Rc::clone(x)), deage(Rc::clone(y)))
						{ (Some(x), Some(y)) => Some((x, y))
						, _ => None
						}
				)
			);
			let aged_state = EvalState { givens: aged_givens };
			let got = s_simplify(p, &aged_state, memo, lanz.age(), Rc::clone(x), er);
			ageflat(Aged(got).rc())
		},
		_ => simple,
	};
	memo.insert(lanz.fullpath(simple2), Rc::clone(&got));
	got
}

pub fn equiv_uncomm(l: Rc<Simple>, r: Rc<Simple>) -> Option<EquivResult> {
	use self::Simple::*;
	use self::EquivResult::*;

	macro_rules! abs {
		($x: expr) => { Some(if $x { Equiv } else { NotEquiv }) }
	}

	let lr = (&*l, &*r);

	if let (Name(x), Name(y)) = lr {
		return abs!(x == y)
	}

	if let (Cool(..), Cool(..)) = lr {
		return match *sbin(BopCode::Equal, l, r)
			{ Literal(WireValue{ bits, .. }) => abs!(bits != 0)
			, _ => Some(Ambiguous)
			}
	}

	if let (Literal(WireValue{ bits: x, width: wx }), Literal(WireValue{ bits: y, width: wy })) = lr {
		return if let Some(w) = wx.combine(*wy) {
			abs!((x & w.mask()) == (y & w.mask()))
		} else {
			Some(NotEquiv) // unequal Bits
		}
	}

	if let (Slice(x, xlo, xhi), Slice(y, ylo, yhi)) = lr {
		let got = equiv(Rc::clone(x), Rc::clone(y));
		return if got == Equiv {
			abs!(xlo == ylo && xhi == yhi)
		} else {
			Some(got)
		}
	}

	if let (BinMaths(op1, x1, y1), BinMaths(op2, x2, y2)) = lr {
		if op1 == op2 {
			let most_unknown_incorrect = |l: EquivResult, r: EquivResult| if l.ke(r) { r } else { l }; // most unknown ; most incorrect of two options
			let forward = most_unknown_incorrect(equiv(Rc::clone(x1), Rc::clone(x2)), equiv(Rc::clone(y1), Rc::clone(y2)));
			return Some(if op1.is_comm() {
				let backward = most_unknown_incorrect(equiv(Rc::clone(x1), Rc::clone(y2)), equiv(Rc::clone(y1), Rc::clone(x2)));
				forward.cmax(backward) // most correct of two options
			} else {
				forward
			})
		}
	}

	None
}

pub fn equiv(l: Rc<Simple>, r: Rc<Simple>) -> EquivResult {
	if let Some(x) = equiv_uncomm(Rc::clone(&l), Rc::clone(&r)) {
		return x
	} else if let Some(x) = equiv_uncomm(r, l) {
		return x
	} else {
		EquivResult::Ambiguous
	}
}

// remove side-by-side duplicates
fn prune<T: DoubleEndedIterator>(xs: T) -> Vec<T::Item> where T::Item: Eq {
	let mut res = Vec::<T::Item>::new();
	for x in xs {
		match res.last()
		{ Some(last) => if *last != x
			{ res.push(x);
			}
		, None => { res.push(x); }
		}
	}
	res
}

// very incomplete implemention with the cases that I need hard-coded in.
fn generate_matches(p: &Program, l: Rc<Simple>, r: Rc<Simple>) -> Vec<Vec<(Unreplaced, Rc<Simple>)>> {
	use self::Simple::*;
	use self::Unreplaced::*;
	// for pc
	if let (Unreplaced(RegOut(name)), Name(x)) = (&*l, &*r) {
		if let Some((_, inname, _)) = p.regouts.get(x) {
			return vec![vec![(RegOut(name.to_string()), r), (RegIn(name.to_string()), Name(inname.to_string()).rc())]]
		}
	}
	return vec![vec![]]
}

// replace matches, values before simplification
pub fn unreplacement_replacement(map: &HashMap<Unreplaced, Rc<Simple>>, x: Rc<Simple>) -> Rc<Simple> {
	use self::Simple::*;
	use self::CoolProperty::*;

	macro_rules! f {
		($x: expr) => { unreplacement_replacement(map, $x) }
	}

	match &*x
	{ Unreplaced(u) => match map.get(&u)
		{ Some(v) => Rc::clone(v)
		, None => match u
			{ self::Unreplaced::RegIn(_) => x // lets these pass through because expected to occasionally fail
			, self::Unreplaced::RegOut(_) => x
			, _ => panic!("Could not find in scope: {}", u)
			}
		}
	, BinMaths(op, x, y) => BinMaths(*op, f!(Rc::clone(x)), f!(Rc::clone(y))).rc()
	, UnMaths(op, x) => UnMaths(*op, f!(Rc::clone(x))).rc()
	, Slice(x, lo, hi) => Slice(f!(Rc::clone(x)), *lo, *hi).rc()
	, Aged(x) => Aged(f!(Rc::clone(x))).rc()
	, Cool(n, xs) => Cool(n.to_string(), Rc::new(xs.iter().map(|prop| match prop
			{ OneOf(xs) => OneOf(xs.clone())
			, Neq(x) => Neq(f!(Rc::clone(x)))
			}).collect::<Vec<CoolProperty>>())).rc()
	, _ => x
	}
}

pub fn test(test: Test, ss: Vec<ast::Statement>) -> Rc<TestResult> {
	let mut assignments = HashMap::<String, &ast::SpannedExpr>::new();
	let mut     regouts = HashMap::<String, (char, String, &ast::SpannedExpr)>::new();
	let mut      widths = HashMap::<String, WireWidth>::new();
	let mut    outchars = HashSet::<char>::new();
	let mut      regins = HashSet::<String>::new();

	// Todo: insert builtin width stuff if squashing 🤔

	use ast::Statement;

	for x in ss.iter() {
		match x {
			Statement::ConstDecls(xs) => {
				for c in xs.iter() {
					assignments.insert(c.name.to_string(), &c.value);
				}
			},
			Statement::WireDecls(xs) => {
				for d in xs.iter() {
					widths.insert(d.name.to_string(), d.width);
				}
			},
			Statement::Assignments(xs) => {
				for a in xs.iter() {
					for (name, _) in a.names.iter() {
						assignments.insert(name.to_string(), &a.value);
					}
				}
			},
			Statement::RegisterBankDecl(rb) => {
				let chars = rb.name.chars().collect::<Vec<char>>();
				if let [a, b] = chars.as_slice() {
					outchars.insert(*b);
					for r in rb.registers.iter() {
						let  inname = format!("{}_{}", a, r.name);
						let outname = format!("{}_{}", b, r.name);
						regins.insert(inname.to_string());
						regouts.insert(outname, (*b, inname.to_string(), &r.default));
						widths.insert(inname, r.width); // RegOut width not used; stall/bubble special case.
					}
				} else {
					panic!("register-bank name was not two characters")
				}
			},
			Statement::Error => panic!("unexpected error in Statement::Error")
		}
	}

	use self::TestEval::*;

	let program = Program { assignments, regouts, widths, outchars, regins };
	let mut evals = Vec::<TestEval>::new();
	fill_eval(test, &mut evals);
	// like a stack, but a tree
	let mut e = EvalTree{name: "root".to_string(), givens: Vec::new(), replacements: Vec::new(), bs: Vec::new(), results: Vec::new() };

	for eval in evals {
		match eval {
			Start(name) => {
				// push new env onto every old env
				e.push(&|| EvalTree{name: name.to_string(), givens: Vec::new(), replacements: Vec::new(), bs: Vec::new(), results: Vec::new() })
			},
			End => {
				// pop all current envs
				let x = e.pop().swap_remove(0); // all should be correct (pruning done after condition).
				let ok = Rc::new(TestResult::Test(x.name, x.results));
				e.transf1(&|x| x.results.push(Rc::clone(&ok)));
			},
			Given(l, r) => {
				// add given to each current env
				e.transf1(&|x| x.givens.push((Rc::clone(&l), Rc::clone(&r))));
			},
			Condition(a, b) => {
				let mut max = EquivResult::NotEquiv; // max correctness of any current env
				// add condition to each current env
				e.transf1_accum(&mut |x, gs, ms| {
					// givens can vary across threads (because of matching) so cannot have global memo.
					// is too much pain to add to TestEval so will just use one-time memo for each equiv
					// probably fast enough for all purposes
					let mut memo = HashMap::<Rc<Simple>, Rc<Simple>>::new();
					let matchmap = HashMap::<Unreplaced, Rc<Simple>>::from_iter(ms);
					// givens can depend on values, matches
					let state = EvalState { givens: HashMap::from_iter(gs.into_iter().map(|(x, y)| (unreplacement_replacement(&matchmap, x), unreplacement_replacement(&matchmap, y)) )) };
					let l = Rc::clone(&a);
					let r = Rc::clone(&b);
					let ll = unreplacement_replacement(&matchmap, Rc::clone(&l));
					let rr = unreplacement_replacement(&matchmap, Rc::clone(&r));
					let simpl = s_simplify(&program, &state, &mut memo, Lanz{age:0}, Rc::clone(&ll), true);
					let simpr = s_simplify(&program, &state, &mut memo, Lanz{age:0}, Rc::clone(&rr), true);
					let res = equiv(Rc::clone(&simpl), Rc::clone(&simpr));

					max = if max.ce(res) { max } else { res };

					let got = TestResult::Condition // 🙂
						{ res
						, gotstr: prune(vec![Rc::clone(&l), Rc::clone(&ll), simpl].iter()).iter().map(|x| x.to_string()). reduce(|x, y| format!("{} -> {}", x, y)).unwrap()
						, expectedstr: prune(vec![l, ll, r, rr, simpr].iter()).iter().map(|x| x.to_string()). reduce(|x, y| format!("{} -> {}", x, y)).unwrap()
						};

					x.results.push(Rc::new(got))
				}, &vec!(), &vec!());
				// prune envs
				e.transf2(&mut |x| {
					x.bs.retain(|b| {
						if let TestResult::Condition{ res, .. } = &**b.results.last().expect("unexpected program state") {
							*res == max
						} else {
							panic!("unexpected program state")
						}
					})
				});
			},
			Match(m) => { // TODO: clearly define what happens when rematching something?
				use self::Unreplaced::*;
				macro_rules! ab {
					($sd: expr, $oi: expr, $a: expr, $b: expr, $sda: expr, $sdb: expr, $oia: expr, $oib: expr) => ({
						let sda = Simple::Name($sda.to_string()).rc();
						let sdb = Simple::Name($sdb.to_string()).rc();
						let oia = Simple::Name($oia.to_string()).rc();
						let oib = Simple::Name($oib.to_string()).rc();
						vec!(
							vec!( ($sd($a), Rc::clone(&sda)), ($sd($b), Rc::clone(&sdb)), ($oi($a), Rc::clone(&oia)), ($oi($b), Rc::clone(&oib)) ),
							vec!( ($sd($a), sdb), ($sd($b), sda), ($oi($a), oib), ($oi($b), oia) ),
						)
					})
				}
				// split current (local) env on the different paths
				e.transf2_accum(&mut |x, gs, _ms|{
					let mss : Vec<Vec<(Unreplaced, Rc<Simple>)>> = match &m {
						MatchType::Regread(a, b) => ab!(Src, Output, a.clone(), b.clone(), "reg_srcA", "reg_srcB", "reg_outputA", "reg_outputB"),
						MatchType::Regwrite(a, b) => ab!(Dst, Input, a.clone(), b.clone(), "reg_dstE", "reg_dstM", "reg_inputE", "reg_inputM"),
						MatchType::Any(l, r) => {
							// a complete version would probably
							//    perform match replacement on these givens as well
							//    + l and r??
							let mut memo = HashMap::<Rc<Simple>, Rc<Simple>>::new();
							// givens can depend on values, matches
							let state = EvalState { givens: HashMap::from_iter(gs) };

							let simpl = s_simplify(&program, &state, &mut memo, Lanz{age:0}, Rc::clone(&l), false);
							let simpr = s_simplify(&program, &state, &mut memo, Lanz{age:0}, Rc::clone(&r), false);

							generate_matches(&program, simpl, simpr)
						},
					};
					x.bs = x.bs.iter().flat_map(|b|
						mss.iter().map(move |ms| EvalTree
							{ name: b.name.to_string()
							, givens: b.givens.clone()
							, replacements: b.replacements.clone().into_iter().chain(ms.clone().into_iter()).collect()
							, bs: Vec::new() // b.bs *should* be empty..
							, results: b.results.clone()
							}
						)
					).collect()
				}, &vec!(), &vec!())
			},
			Value(name, props) => {
				let ps = Rc::new(props);
				e.transf1_accum(&mut |x, _, rs|{
					let ok = unreplacement_replacement(&HashMap::from_iter(rs.into_iter()), Simple::Cool(name.clone(), Rc::clone(&ps)).rc());
					x.replacements.push((Unreplaced::Value(name.clone()), ok))
				}, &vec!(), &vec!());
			},
		}
	}

	e.results.pop().expect("program logic error")
}

type AcGivens = Vec<(Rc<Simple>, Rc<Simple>)>;
type AcMatches = Vec<(Unreplaced, Rc<Simple>)>;

// something like tree used as stack?
#[derive(Debug)]
struct EvalTree
	{ name: String
	, givens: Vec<(Rc<Simple>, Rc<Simple>)>
	, replacements: Vec<(Unreplaced, Rc<Simple>)>
	, bs: Vec<EvalTree> // branches
	, results: Vec<Rc<TestResult>>
	}

impl EvalTree {
	fn pop(&mut self) -> Vec<EvalTree> { // removes level
		let mut res = Vec::new();
		self.transf2(&mut |r| {
				res.extend(std::mem::replace(&mut r.bs, Vec::new()))
			//refs.into_iter().map(|r| std::mem::replace(&mut r.bs, Vec::new())).collect()
		});
		res
	}
	fn push<F>(&mut self, f: &F) where F: Fn() -> EvalTree { // adds level
		if self.bs.is_empty() {
			self.bs.push(f());
		} else {
			for x in self.bs.iter_mut() {
				x.push(f);
			}
		}
	}
	fn transf2<F>(&mut self, f: &mut F) where F: FnMut(&mut EvalTree) -> () {
		if self.bs.is_empty() {
			return
		} else if self.bs.iter().all(|x| x.bs.is_empty()) {
			f(self) // second level
		} else {
			for b in self.bs.iter_mut() {
				b.transf2(f)
			}
		}
	}
	fn transf2_accum<F>(&mut self, f: &mut F, givens: &AcGivens, replacements: &AcMatches) where F: FnMut(&mut EvalTree, AcGivens, AcMatches) -> () {
		let mut givens2 = givens.clone();
		let mut replacements2 = replacements.clone();

		givens2.extend(self.givens.clone());
		replacements2.extend(self.replacements.clone());

		if self.bs.is_empty() {
			return
		} else if self.bs.iter().all(|x| x.bs.is_empty()) {
			f(self, givens2, replacements2) // second level
		} else {
			for b in self.bs.iter_mut() {
				b.transf2_accum(f, &givens2, &replacements2)
			}
		}
	}
	fn transf1_accum<F>(&mut self, f: &mut F, givens: &AcGivens, replacements: &AcMatches) where F: FnMut(&mut EvalTree, AcGivens, AcMatches) -> () {
		let mut givens2 = givens.clone();
		let mut replacements2 = replacements.clone();

		givens2.extend(self.givens.clone());
		replacements2.extend(self.replacements.clone());

		if self.bs.is_empty() {
			f(self, givens2, replacements2)
		} else {
			for b in self.bs.iter_mut() {
				b.transf1_accum(f, &givens2, &replacements2)
			}
		}
	}
	// apply function over all leaves
	fn transf1<F>(&mut self, f: &F) where F: Fn(&mut EvalTree) -> () {
		if self.bs.is_empty() {
			f(self)
		} else {
			for b in self.bs.iter_mut() {
				b.transf1(f)
			}
		}
	}
}

// intermediate representation form which is probably totally unnecessary and just clutters code
#[derive(Debug)]
enum TestEval
	{ Start(String)
	, End
	, Given(Rc<Simple>, Rc<Simple>)
	, Value(String, Vec<CoolProperty>)
	, Condition(Rc<Simple>, Rc<Simple>)
	, Match(MatchType)
	}

fn fill_eval(ast: Test, stack: &mut Vec<TestEval>) {
	use self::TestEval::*;
	match ast {
		Test::Test(s, ts) => {
			stack.push(Start(s));
			for t in ts {
				fill_eval(t, stack);
			}
			stack.push(End);
		},
		Test::ValueDef(x, y) => stack.push(Value(x, y)),
		Test::Given(x, y) => stack.push(Given(x, y)),
		Test::Condition(x, y) => stack.push(Condition(x, y)),
		Test::Match(m) => stack.push(Match(m)),
	}
}
