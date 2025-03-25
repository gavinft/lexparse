use expect_with::ExpectWith;

use self::charlist::CharList;
use crate::{error::BacktracedError, token::Token};
use std::{collections::HashMap, fmt::Display, rc::Rc};

mod charlist {
    use std::{ops::Deref, rc::Rc};

    #[derive(Clone, Hash, Debug)]
    pub struct CharList {
        head: Rc<CharListNode>,
    }

    #[derive(Clone, Hash, Debug)]
    enum CharListNode {
        Node(char, Rc<CharListNode>),
        Edge,
    }

    #[derive(Clone, Hash, Debug)]
    pub enum CharListPop {
        Node(char, CharList),
        Edge,
    }

    /**
     * Linked list of chars, meant to work similarly to Ocaml. Makes my life easier
     */
    impl CharList {
        pub fn empty() -> CharList {
            CharList::of(Rc::new(CharListNode::Edge))
        }
        pub fn push(&self, c: char) -> CharList {
            CharList::of(Rc::new(CharListNode::Node(c, self.head.clone())))
        }
        /**
         * The result of this should be matched on to create Ocaml-like ergonomics
         */
        pub fn pop(&self) -> CharListPop {
            match self.head.deref() {
                CharListNode::Node(c, child) => {
                    CharListPop::Node(c.clone(), CharList::of(child.clone()))
                }
                CharListNode::Edge => CharListPop::Edge,
            }
        }
        pub fn new(c: char) -> CharList {
            Self::empty().push(c)
        }
        /**
         * Adds this element to the string, then recurses on the next element
         */
        fn implode_rec(&self, string: &mut String) {
            match self.pop() {
                CharListPop::Node(c, child) => {
                    string.push(c);
                    child.implode_rec(string);
                }
                CharListPop::Edge => (),
            }
        }
        /**
         * Turns a list into a string
         */
        pub fn implode(&self) -> String {
            let mut string = String::new();
            self.implode_rec(&mut string);
            string
        }
        fn of(rc: Rc<CharListNode>) -> CharList {
            CharList { head: rc }
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexErrorKind {
    UnclosedStringLit,
}

impl Display for LexErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexErrorKind::UnclosedStringLit => {
                write!(f, "The string literal is missing a closing quotation mark")
            }
        }
    }
}

pub type LexError = BacktracedError<LexErrorKind>;
pub type LexRes<T> = Result<T, LexError>;

enum EatResult {
    Chars(CharList),
    Token(Token),
}

fn consume_whitespace(text: &str) -> &str {
    for (i, c) in text.chars().enumerate() {
        if !c.is_whitespace() {
            return &text[i..];
        }
    }

    &text[text.len()..]
}

/**
 * checks if token matches, and if so advances the pointer
 */
fn matches_tok(text: &mut &str, tok: &str) -> bool {
    if text.len() < tok.len() {
        false
    } else {
        for (tokc, textc) in tok.chars().zip(text.chars()) {
            if tokc != textc {
                return false;
            }
        }

        *text = &text[tok.len()..];
        true
    }
}

/**
 * reads characters until closing quotes are seen. Assumes the opening quotes have already been read.
 */
fn eat_str_lit(text: &mut &str) -> LexRes<Rc<str>> {
    let mut lit_slice: Option<&str> = None;
    // find slice that contains the literal
    for (i, chr) in text.chars().enumerate() {
        if chr == '"' {
            // save this lit slice
            lit_slice = Some(&text[..i]);
            // update text pointer
            *text = &text[i + 1..];
            break;
        }
    }
    // if we found slice, return, otherwise give error
    lit_slice
        .ok_or_else(|| LexError::new(LexErrorKind::UnclosedStringLit))
        .map(|slice| Rc::from(slice))
}

fn eat_int_lit(text: &mut &str) -> Option<i64> {
    // If there are no characters give up
    if text.len() < 1 {
        None
    } else {
        // grab the first character
        let first_ch = text
            .chars()
            .next()
            .expect("there should be at least 1 character in text");
        // if it's not a digit or a minus, give up
        if first_ch != '-' && !first_ch.is_numeric() {
            None
        } else {
            // loop through digits and add to string
            let mut number_str = &text[..1];
            for (i, c) in text.chars().enumerate().skip(1) {
                if !c.is_numeric() {
                    break;
                }
                number_str = &text[..i + 1];
            }
            // If there's only a - but no number afterwards
            if number_str.len() == 1 && number_str.chars().next().unwrap() == '-' {
                None
            } else {
                let num = number_str
                    .parse::<i64>()
                    .expect_with(|| format!("\"{number_str}\" should be an integer"));
                // increment text ptr
                *text = &text[number_str.len()..];
                Some(num)
            }
        }
    }
}

/**
 * Returns true if first character of text is whitespace
 */
fn peek_whitespace(text: &str) -> bool {
    let ch = text
        .chars()
        .next()
        .expect("string should have at least 1 character");
    ch.is_whitespace()
}

/**
 * ignore_int_lit: ignores int literals. should only be true if mid parsing an identifier so that it can have digits in the name
 */
fn eat_rec(text: &mut &str, ignore_int_lit: bool) -> LexRes<EatResult> {
    let int_lit = if ignore_int_lit {
        // Only checks if this is an int lit if we are supposed to
        None
    } else {
        eat_int_lit(text)
    };

    Ok(if let Some(num) = int_lit {
        // Check if this is an int lit
        EatResult::Token(Token::IntLit(num))
    } else if matches_tok(text, "+") {
        // Check if this is a character token
        EatResult::Token(Token::Plus)
    } else if matches_tok(text, "-") {
        EatResult::Token(Token::Minus)
    } else if matches_tok(text, "*") {
        EatResult::Token(Token::Asterisk)
    } else if matches_tok(text, "/") {
        EatResult::Token(Token::FSlash)
    } else if matches_tok(text, "%") {
        EatResult::Token(Token::Percent)
    } else if matches_tok(text, "<=") {
        EatResult::Token(Token::LE)
    } else if matches_tok(text, "<") {
        EatResult::Token(Token::LT)
    } else if matches_tok(text, "==") {
        EatResult::Token(Token::EQ)
    } else if matches_tok(text, ">=") {
        EatResult::Token(Token::GE)
    } else if matches_tok(text, ">") {
        EatResult::Token(Token::GT)
    } else if matches_tok(text, "=") {
        EatResult::Token(Token::AssignEq)
    } else if matches_tok(text, "\"") {
        EatResult::Token(Token::StringLit(eat_str_lit(text)?))
    } else {
        let ch = text
            .chars()
            .next()
            .expect("string should have at least 1 character");
        *text = &text[1..]; // advance text pointer
        let cur_text_ptr = *text; // save copy of pointer to next char

        if text.len() < 1 || peek_whitespace(&text) {
            // If this is the last character of the string or if the next one is whitespace, just return it
            EatResult::Chars(CharList::new(ch))
        } else {
            // Process next token
            let next = eat_rec(text, true)?;
            // if next token was a character, prepend this one to the string. otherwise, reset pointer and just return this character
            match next {
                EatResult::Chars(char_list) => EatResult::Chars(char_list.push(ch)),
                EatResult::Token(_) => {
                    // reset pointer
                    *text = cur_text_ptr;
                    // return this char
                    EatResult::Chars(CharList::new(ch))
                }
            }
        }
    })
}

/**
 * Consumes the next token in text
 */
fn eat(text: &mut &str, words: &HashMap<&str, Token>) -> LexRes<Token> {
    Ok(match eat_rec(text, false)? {
        EatResult::Chars(word_chars) => {
            // turn the char list into a string
            let word = word_chars.implode();
            // get the corresponding word and return, or return an ident
            words
                .get(word.as_str())
                .map(|w| w.clone())
                .unwrap_or_else(|| Token::Ident(Rc::from(word)))
        }
        // just return the token
        EatResult::Token(token) => token,
    })
}

/**
 * Turns text into a string of tokens
 */
pub fn lex(text: String, words: &HashMap<&str, Token>) -> LexRes<Vec<Token>> {
    // list of tokens for saving each iteration
    let mut tokens: Vec<Token> = Vec::new();
    // cursor that points to where we currently are in the input text
    let mut pointer: &str = text.as_str();
    // clear any beginning whitespace
    pointer = consume_whitespace(pointer);
    while pointer.len() > 0 {
        // eat the next token
        let tok = eat(&mut pointer, words);
        // add to list
        tokens.push(tok?);
        // consume any whitespace after token
        pointer = consume_whitespace(pointer);
    }

    // return the list
    Ok(tokens)
}

pub fn default_word_map() -> HashMap<&'static str, Token> {
    HashMap::from([
        ("let", Token::Let),
        ("if", Token::If),
        ("then", Token::Then),
        ("else", Token::Else),
        ("fi", Token::Fi),
        ("for", Token::For),
        ("do", Token::Do),
        ("done", Token::Done),
        ("while", Token::While),
        ("finish", Token::Finish),
        ("print", Token::Print),
        ("true", Token::True),
        ("false", Token::False),
    ])
}
