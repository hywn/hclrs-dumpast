use ast;
use ast::{ WireValue, WireWidth };
use std::fmt;
use std::rc::Rc;
use std::iter::FromIterator;
use std::collections::HashMap;
use std::collections::HashSet;
use serde_json::json;

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

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Unmatched
	{ RegIn(String)  // e.g. @myreg[in]  -> String=myreg
	, RegOut(String) // e.g. @myreg[out] -> String=myreg
	, Input(String)  // e.g. $inputX     -> String=X
	, Output(String) // e.g. $outputX    -> String=X
	, Src(String)    // e.g. $srcX       -> String=X
	, Dst(String)    // e.g. $dstX       -> String=X
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
	{ Literal(WireValue) // literalesque
	, Cool(String)       // "
	, Wildcard           // "
	, OneOfLogic(String, Vec<(u128, Simple)>)
	, Name(String)
	, Unmatched(Unmatched)
	, Error(String)
	, BinMaths(BopCode, Rc<Simple>, Rc<Simple>) // these contain other Simples
	, UnMaths(UopCode, Rc<Simple>)              // "
	, Slice(Rc<Simple>, u8, u8)                 // "
	, Aged(Rc<Simple>)                          // "
	, Unknown(Unknown)                          // "
	}

impl Simple {
	pub fn rc(self) -> Rc<Self> {
		Rc::new(self)
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum CoolProperty
	{ OneOf(HashSet<u128>)
	, Neq(Rc<Simple>)
	}

#[derive(Debug)]
pub enum Test
	{ Test(String, Vec<Test>)
	, ValueDef(String, Vec<CoolProperty>)
	, Given(Rc<Simple>, Rc<Simple>)
	, Condition(Rc<Simple>, Rc<Simple>)
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
}

#[derive(Debug)]
pub enum TestResult
	{ Condition { res: EquivResult, gotstr: String, expectedstr: String /*got: Rc<Simple>, expected: Rc<Simple>*/ } // want to show a bunch of simplification info without storing literally everything
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

impl fmt::Display for Simple {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::Simple::*;
		use self::Unmatched::*;
		use self::Unknown::*;
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
				"res": match rs.iter().filter_map(|x| if let TestResult::Condition{ res, .. } = x { Some(res) } else { None }).reduce(|a, b| if a.ce(*b) { a } else { b })
					{ Some(x) => x.to_string()
					, None => "Empty".to_string()
					},
				"conditions": rs.iter().filter_map(|x|
					if let TestResult::Condition{ .. } = x {
						Some(x.to_json())
					} else {
						None
					}).collect::<Vec<serde_json::Value>>(),
				"sub": rs.iter().filter_map(|x|
					if let TestResult::Test(_, _) = x {
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
			, Cool(_)    => Rc::clone(x)
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
	}

struct EvalState
	{ givens : HashMap<Rc<Simple>, Rc<Simple>>
	//; cool stuff
	//; cache
	//; matches?
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

				let no = Literal(WireValue{bits: 0, width: WireWidth::Bits(1)}).rc();

				xs.iter().map(|thing| sbin(BopCode::Equal, Rc::clone(&x), Rc::clone(thing)))
					.fold(no, |a, b| sbin(BopCode::Or, a, b))
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

fn sbin_commshort(op: BopCode, l: Rc<Simple>, _r: Rc<Simple>) -> Option<Rc<Simple>> {
	use self::Simple::*;
	use self::BopCode::*;

	if let Literal(x) = *l {
		return match (op, x)
		{ (Or, WireValue{ bits: 0, .. }) => Some(l)
		, _ => None
		}
	}

	None
}

fn sbin(op: BopCode, l: Rc<Simple>, r: Rc<Simple>) -> Rc<Simple> {
	use self::Simple::*;

	let lr = (&*l, &*r);

	if let (Literal(x), Literal(y)) = lr {
		return Literal(lit_bop(op, *x, *y)).rc()
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
	, Cool(_)    => Some(x)
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
		OneOfLogic(_, _) => {
			todo!()
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
				//Simple::Error(format!("<not found: {}>", n)).rc() // TODO: complete e.g. default values
				simple
			};

			got // TODO: squash(?)
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
		_ => simple
	};
	memo.insert(lanz.fullpath(simple2), Rc::clone(&got));
	got
}

pub fn equiv_uncomm(l: Rc<Simple>, r: Rc<Simple>) -> Option<EquivResult> {
	use self::Simple::*;
	use self::EquivResult::*;
	use ast::WireWidth::*;

	macro_rules! abs {
		($x: expr) => { Some(if $x { Equiv } else { NotEquiv }) }
	}

	let lr = (&*l, &*r);

	if let (Name(x), Name(y)) = lr {
		return abs!(x == y)
	}

	if let (Literal(WireValue{ bits: x, width: wx }), Literal(WireValue{ bits: y, width: wy })) = lr {
		return if let Some(w) = wx.combine(*wy) {
			let mask = w.mask();
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

pub fn test(test: Test, ss: Vec<ast::Statement>) -> TestResult {
	let mut assignments = HashMap::<String, &ast::SpannedExpr>::new();
	let mut     regouts = HashMap::<String, (char, String, &ast::SpannedExpr)>::new();
	let mut      widths = HashMap::<String, WireWidth>::new();

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
					for r in rb.registers.iter() {
						let  inname = format!("{}_{}", a, r.name);
						let outname = format!("{}_{}", b, r.name);
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

	use self::Test::*;

	let program = Program { assignments, regouts, widths };
	let mut memo = HashMap::<Rc<Simple>, Rc<Simple>>::new(); // drained whenever givens change

	let mut evalstack = Vec::<Option<self::Test>>::new(); // None represents end-of-test
	let mut givens_stack = Vec::<HashMap::<Rc<Simple>, Rc<Simple>>>::new();
	let mut result_stack = Vec::<TestResult>::new();

	evalstack.push(Some(test));
	result_stack.push(TestResult::Test("root".to_string(), Vec::new()));

	loop {
		match evalstack.pop() {
			Some(Some(Test(name, ts))) => { // new test
				givens_stack.push(HashMap::new());
				result_stack.push(TestResult::Test(name, Vec::new()));
				evalstack.push(None);
				evalstack.extend(ts.into_iter().rev().map(|x| Some(x)));
			},
			Some(Some(Given(l, r))) => {
				memo.drain();
				if let Some(x) = givens_stack.last_mut() {
					x.insert(l, r);
				} else {
					panic!("test underflow?")
				}
			},
			Some(Some(Condition(l, r))) => {
				let givens : HashMap<Rc<Simple>, Rc<Simple>> = HashMap::from_iter(
					givens_stack.iter().flat_map(|x| x.iter())
						.map(|(x, y)| (Rc::clone(x), Rc::clone(y)))
				);
				let state = EvalState { givens };
				let simpl = s_simplify(&program, &state, &mut memo, Lanz{age:0}, Rc::clone(&l), true);
				let simpr = s_simplify(&program, &state, &mut memo, Lanz{age:0}, Rc::clone(&r), true);
				let res = equiv(Rc::clone(&simpl), Rc::clone(&simpr));
				if let TestResult::Test(_, rs) = result_stack.last_mut().expect("test underflow?") {
					rs.push(TestResult::Condition // 🙂
						{ res
						, gotstr: prune(vec![l, simpl].iter()).iter().map(|x| x.to_string()). reduce(|x, y| format!("{} -> {}", x, y)).unwrap()
						, expectedstr: prune(vec![r, simpr].iter()).iter().map(|x| x.to_string()). reduce(|x, y| format!("{} -> {}", x, y)).unwrap()
						});
				} else {
					panic!("only Tests should have gone on the result_stack");
				}
			},
			Some(None) => {
				memo.drain(); // could probably see if givens was empty
				givens_stack.pop();
				let res = result_stack.pop().expect("stack underflow?");
				if let Some(TestResult::Test(_, rs)) = result_stack.last_mut() {
					rs.push(res)
				} else {
					panic!("no parent for popped child test")
				}
			},
			None => break,
			_ => todo!(),
		}
	}
	if let Some(TestResult::Test(_, mut rs)) = result_stack.pop() {
		rs.pop().expect("no top-level test")
	} else {
		panic!("program stack logic problems")
	}
}
