pub mod ast;
pub mod interpret;
pub mod lex;
pub mod parse;
pub mod token;

fn main() {
    let input = "print 5 + 5 + 13+-2 * 4 + 5";
    let word_map = lex::default_word_map();
    let toks = dbg!(lex::lex(input.to_owned(), &word_map));
    let tree = dbg!(parse::parse(toks.as_slice()).unwrap());
    interpret::interpret(&[tree]).unwrap();
}
