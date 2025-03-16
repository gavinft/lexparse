pub mod ast;
pub mod interpret;
pub mod lex;
pub mod parse;
pub mod token;

fn execute(prgm: &str) {
    let word_map = lex::default_word_map();
    let toks = dbg!(lex::lex(prgm.to_owned(), &word_map));
    let tree = dbg!(parse::parse(toks.as_slice()).unwrap());
    interpret::interpret(&tree).unwrap();
}

fn main() {
    execute("if 2 > 3 then print 1 else print 0 print 2 print 4 fi");
}
