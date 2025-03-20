pub mod ast;
mod error;
pub mod interpret;
pub mod lex;
pub mod parse;
pub mod token;

use std::fs;

fn execute(prgm: &str) {
    let word_map = lex::default_word_map();
    let toks = dbg!(lex::lex(prgm.to_owned(), &word_map).unwrap());
    let tree = dbg!(parse::parse(toks.as_slice()).unwrap());
    interpret::interpret(&tree).unwrap();
}

fn main() {
    let prgm = fs::read_to_string("program.p").unwrap();
    execute(prgm.as_ref());
}
