use std::env;
use std::process;
use std::path::Path;

#[macro_use]
extern crate log;
#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub parser);
use parser::StatementsParser;

mod lexer;
use lexer::Lexer;

mod ast;
use ast::Statement;

mod io;
use io::FileContents;

mod errors;
use errors::Error;

pub const Y86_PREAMBLE: &'static str = "
const STAT_BUB = 0b000, STAT_AOK = 0b001, STAT_HLT = 0b010;  # expected behavior
const STAT_ADR = 0b011, STAT_INS = 0b100, STAT_PIP = 0b110;  # error conditions
const REG_RAX = 0b0000, REG_RCX = 0b0001, REG_RDX = 0b0010, REG_RBX = 0b0011;
const REG_RSP = 0b0100, REG_RBP = 0b0101, REG_RSI = 0b0110, REG_RDI = 0b0111;
const REG_R8  = 0b1000, REG_R9  = 0b1001, REG_R10 = 0b1010, REG_R11 = 0b1011;
const REG_R12 = 0b1100, REG_R13 = 0b1101, REG_R14 = 0b1110, REG_NONE= 0b1111;
# icodes; see figure 4.2
const HALT   = 0b0000, NOP    = 0b0001, RRMOVQ = 0b0010, IRMOVQ = 0b0011;
const RMMOVQ = 0b0100, MRMOVQ = 0b0101, OPQ    = 0b0110, JXX    = 0b0111;
const CALL   = 0b1000, RET    = 0b1001, PUSHQ  = 0b1010, POPQ   = 0b1011;
const CMOVXX = RRMOVQ;
# ifuns; see figure 4.3
const ALWAYS = 0b0000, LE   = 0b0001, LT   = 0b0010, EQ   = 0b0011;
const NE     = 0b0100, GE   = 0b0101, GT   = 0b0110;
const ADDQ   = 0b0000, SUBQ = 0b0001, ANDQ = 0b0010, XORQ = 0b0011;
const true = 1;
const false = 0;
const TRUE = 1;
const FALSE = 0;
";

fn main() {
	let args: Vec<String> = env::args().collect();

	if args.len() < 3 {
		eprintln!("usage: ./tool <test file> <program file>");
		process::abort();
	}

	match FileContents::new_from_file_with_preamble(Y86_PREAMBLE, Path::new(&args[2])) {
		Ok(fc) => {
			match internal_dump_ast(&fc) {
				Ok(stuff) => println!("{:?}", stuff),
				Err(_) => {
					eprintln!("program file did not parse");
					process::abort();
				}
			}
		}
		Err(x) => println!("{:?}", x)
	}

}

pub fn internal_dump_ast(contents: &FileContents) -> Result<Vec<Statement>, Error> {
	let mut errors = Vec::new();
	let lexer = Lexer::new_for_file(contents);
	let statements;
	match StatementsParser::new().parse(&mut errors, lexer) {
		Ok(s) => statements = s,
		Err(e) => {
			let mut errors: Vec<Error> = errors.into_iter().map(|err_rec| Error::from(err_rec)).collect();
			errors.push(Error::from(e));
			return Err(Error::MultipleErrors(errors));
		},
	}
	if errors.len() > 0 {
		return Err(Error::MultipleErrors(errors.into_iter().map(|err_rec| Error::from(err_rec)).collect()));
	}
	Ok(statements)
}
