extern crate extprim;
extern crate num_traits;

use extprim::u128::u128;

use std::str::FromStr;
use std::num::ParseIntError;
use std::cmp;

use self::num_traits::cast::ToPrimitive;

#[derive(Debug,Eq,PartialEq)]
pub enum Error {
    MismatchedWidths,
}

#[derive(Clone,Copy,Debug,Eq,PartialEq)]
pub enum WireWidth {
    Bits(u8),
    Unlimited,
}

impl From<usize> for WireWidth {
    fn from(s: usize) -> Self { WireWidth::Bits(s as u8) }
}

impl FromStr for WireWidth {
    type Err = ParseIntError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match u8::from_str(s) {
            Ok(s) => Ok(WireWidth::Bits(s)),
            Err(x) => Err(x)
        }
    }
}

impl WireWidth {
    fn bits_or(self, s: u8) -> u8 {
        match self {
            WireWidth::Bits(t) => t,
            WireWidth::Unlimited => s,
        }
    }

    pub fn min(self, w: WireWidth) -> WireWidth {
        match self {
            WireWidth::Unlimited => w,
            WireWidth::Bits(s) => WireWidth::Bits(cmp::min(s, w.bits_or(s)))
        }
    }

    pub fn max(self, w: WireWidth) -> WireWidth {
        match self {
            WireWidth::Unlimited => w,
            WireWidth::Bits(s) => WireWidth::Bits(cmp::max(s, w.bits_or(s)))
        }
    }

    pub fn combine(self, other: WireWidth) -> Result<WireWidth, Error> {
        match (self, other) {
            (WireWidth::Unlimited, _) => Ok(other),
            (_, WireWidth::Unlimited) => Ok(self),
            (WireWidth::Bits(s), WireWidth::Bits(t)) =>
                if s == t {
                    Ok(self)
                } else {
                    Err(Error::MismatchedWidths)
                }
        }
    }

    pub fn mask(self) -> u128 {
        match self {
            WireWidth::Unlimited => !u128::new(0),
            WireWidth::Bits(s) => ((!u128::new(0)) >> (128 - s)),
        }
    }
}

// FIXME: disallow Eq?
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub struct WireValue {
    pub bits: u128,
    pub width: WireWidth
}

impl WireValue {
    pub fn new(v: u128) -> WireValue {
        WireValue { bits: v, width: WireWidth::Unlimited }
    }

    pub fn from_binary(s: &str) -> WireValue {
        WireValue {
            bits: u128::from_str_radix(s, 2).unwrap(),
            width: WireWidth::Bits(s.len() as u8),
        }
    }

    pub fn from_decimal(s: &str) -> WireValue {
        WireValue {
            bits: u128::from_str_radix(s, 10).unwrap(),
            width: WireWidth::Unlimited,
        }
    }

    pub fn from_hexadecimal(s: &str) -> WireValue {
        WireValue {
            bits: u128::from_str_radix(s, 16).unwrap(),
            width: WireWidth::Unlimited,
        }
    }

    pub fn as_width(self, new_width: WireWidth) -> WireValue {
        WireValue { bits: self.bits & new_width.mask(), width: new_width }
    }

    pub fn op<F>(self, other: WireValue, f: F, new_width: WireWidth) -> WireValue
            where F: Fn(u128, u128) -> u128 {
        WireValue { bits: f(self.bits, other.bits) & new_width.mask(), width: new_width }
    }

    pub fn is_true(self) -> bool {
        self.bits > u128::new(0)
    }

    pub fn is_false(self) -> bool {
        self.bits == u128::new(0)
    }

}

#[derive(Debug,Eq,PartialEq,Clone,Copy)]
enum BinOpKind {
    Boolean,
    EqualWidth
}

#[derive(Debug,Eq,PartialEq)]
pub struct WireDecl {
    pub name: String,
    pub width: WireWidth,
}

#[derive(Debug,Eq,PartialEq,Clone,Copy)]
pub enum BinOpCode {
    Add,
    Sub,
    Mul,
    Div,
    Or,
    Xor,
    And,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    LogicalAnd,
    LogicalOr,
    LeftShift,
    RightShift,
}

fn boolean_to_value(x: bool) -> u128 {
    if x { u128::new(1) } else { u128::new(0) }
}

impl BinOpCode {
    fn kind(self) -> BinOpKind {
        match self {
            BinOpCode::LogicalAnd => BinOpKind::Boolean,
            BinOpCode::LogicalOr => BinOpKind::Boolean,
            BinOpCode::Equal => BinOpKind::Boolean,
            BinOpCode::LessEqual => BinOpKind::Boolean,
            BinOpCode::GreaterEqual => BinOpKind::Boolean,
            BinOpCode::Less => BinOpKind::Boolean,
            BinOpCode::Greater => BinOpKind::Boolean,
            _ => BinOpKind::EqualWidth,
        }
    }

    fn apply_raw(self, left: u128, right: u128) -> u128 {
        match self {
            BinOpCode::Add => left.wrapping_add(right),
            BinOpCode::Sub => left.wrapping_sub(right),
            BinOpCode::Mul => left.wrapping_mul(right),
            BinOpCode::Div => left.wrapping_div(right),  // FIXME: handle divide-by-zero
            BinOpCode::Or =>  left | right,
            BinOpCode::Xor => left ^ right,
            BinOpCode::And => left & right,
            BinOpCode::Equal => boolean_to_value(left == right),
            BinOpCode::NotEqual => boolean_to_value(left != right),
            BinOpCode::LessEqual => boolean_to_value(left <= right),
            BinOpCode::GreaterEqual => boolean_to_value(left >= right),
            BinOpCode::Less => boolean_to_value(left < right),
            BinOpCode::Greater => boolean_to_value(left > right),
            BinOpCode::LogicalAnd => boolean_to_value(
                left != u128::new(0) && right != u128::new(0)
            ),  // FIXME: shortcircuit support?
            BinOpCode::LogicalOr =>  boolean_to_value(
                left != u128::new(0) || right != u128::new(0)
            ),
            BinOpCode::LeftShift =>  match (
                    left.wrapping_shl(right.to_u32().unwrap_or(0)),
                    right >= u128::new(128)
                ) {
                (_, true) => u128::new(0),
                (x, false) => x,
            },
            BinOpCode::RightShift => match (
                    left.wrapping_shr(right.to_u32().unwrap_or(0)),
                    right >= u128::new(128)
                ) {
                (_, true) => u128::new(0),
                (x, false) => x,
            },
        }
    }

    fn apply(self, left: WireValue, right: WireValue) -> Result<WireValue, Error> {
        let final_width = match self.kind() {
            BinOpKind::EqualWidth => try!(left.width.combine(right.width)),
            BinOpKind::Boolean => WireWidth::Bits(1),
        };
        Ok(left.op(right, |l, r| self.apply_raw(l, r), final_width))
    }
}

#[derive(Debug,Eq,PartialEq)]
pub enum UnOpCode {
    Negate,
    Complement,
}

#[derive(Debug,Eq,PartialEq)]
pub struct MuxOption {
    condition: Box<Expr>,
    value: Box<Expr>,
}

#[derive(Debug,Eq,PartialEq)]
pub enum Expr {
    Constant(WireValue),
    BinOp(BinOpCode, Box<Expr>, Box<Expr>),
    UnOp(UnOpCode, Box<Expr>),
    Mux(Vec<MuxOption>),
}

impl Expr {
    pub fn width(&self) -> Result<WireWidth, Error> {
        match *self {
            Expr::Constant(ref value) => Ok(value.width),
            Expr::BinOp(opcode, ref left, ref right) =>
                match opcode.kind() {
                    BinOpKind::EqualWidth => try!(left.width()).combine(try!(right.width())),
                    BinOpKind::Boolean => Ok(WireWidth::Bits(1)),
                },
            Expr::Mux(ref options) =>
                options.iter().fold(Ok(WireWidth::Unlimited),
                                    |maybe_width, ref item| try!(maybe_width).combine(try!(item.value.width()))),
            Expr::UnOp(UnOpCode::Negate, _) => Ok(WireWidth::Bits(1)),
            Expr::UnOp(UnOpCode::Complement, ref covered) => covered.width(),
        }
    }

    pub fn evaluate(&self) -> Result<WireValue, Error> {
        match *self {
            Expr::Constant(value) => Ok(value),
            Expr::BinOp(opcode, ref left, ref right) => {
                let left_value = try!(left.evaluate());
                let right_value = try!(right.evaluate());
                opcode.apply(left_value, right_value)
            }
            _ => unimplemented!(),
        }
    }
}
