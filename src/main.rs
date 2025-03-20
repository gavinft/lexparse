pub mod ast;
pub mod error;
pub mod interpret;
pub mod lex;
pub mod parse;
pub mod token;

use std::fs;

fn execute(prgm: &str) -> Result<(), Box<dyn std::error::Error>> {
    let word_map = lex::default_word_map();
    let toks = dbg!(lex::lex(prgm.to_owned(), &word_map)?);
    let tree = dbg!(parse::parse(toks.as_slice())?);
    interpret::interpret(&tree)?;
    Ok(())
}

fn main_err() -> Result<(), Box<dyn std::error::Error>> {
    let prgm = fs::read_to_string("program.p")?;
    execute(prgm.as_ref())?;
    Ok(())
}

fn main() {
    if let Err(err) = main_err() {
        println!("{err}");
    }
}
