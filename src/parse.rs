use self::ParseError::*;
use crate::ast::{Expr::*, Statement::*, *};
use crate::token::Token;
use std::{error::Error, fmt::Display};

#[derive(Debug, Clone)]
pub enum ParseError {
    Eof,
    ExpectedEof(Token),
    ExpectedStatement(Token),
    ExpectedExpr(Token),
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "Parser reached end of file but more input was expected"),
            Self::ExpectedStatement(tok) => {
                write!(
                    f,
                    "A statement was expected, but a \'{}\' token was found instead",
                    { tok.kind() }
                )
            }
            Self::ExpectedExpr(tok) => {
                write!(
                    f,
                    "An expression was expected, but a \'{}\' token was found instead",
                    { tok.kind() }
                )
            }
            Self::ExpectedEof(tok) => {
                write!(
                    f,
                    "The end of the file was expected, but a \'{}\' token was found instead",
                    { tok.kind() }
                )
            }
        }
    }
}

pub type ParseRes<T> = Result<T, ParseError>;

#[derive(Debug, Clone, Hash)]
struct TokenList<'a> {
    ptr: &'a [Token],
}

impl<'a> TokenList<'a> {
    pub fn from(lst: &'a [Token]) -> TokenList<'a> {
        TokenList { ptr: lst }
    }
    pub fn peek(&self) -> ParseRes<Token> {
        self.ptr.first().map(|t| t.clone()).ok_or(Eof)
    }
    pub fn eat(self) -> ParseRes<(Token, TokenList<'a>)> {
        let tok = self.ptr.first().ok_or(Eof)?.clone();
        Ok((
            tok,
            TokenList {
                ptr: &self.ptr[1..],
            },
        ))
    }
    pub fn next(self) -> ParseRes<TokenList<'a>> {
        if self.empty() {
            Err(Eof)
        } else {
            Ok(TokenList {
                ptr: &self.ptr[1..],
            })
        }
    }
    // pub fn lookahead(&self, n: usize) -> ParseRes<Token> {
    //     self.ptr.get(n).map(|t| t.clone()).ok_or(Eof)
    // }
    pub fn empty(&self) -> bool {
        self.ptr.len() < 1
    }
}

fn parse_lit(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (next, remaining) = toks.eat()?;
    match next {
        Token::Int(i) => Ok((Int(i), remaining)),
        found => Err(ExpectedExpr(found)),
    }
}

fn parse_mult_div_mod(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (left, remaining) = parse_lit(toks)?;
    let next_res = remaining.peek();
    if next_res.is_ok() {
        match next_res.unwrap() {
            Token::Asterisk => {
                let (right, remaining) = parse_mult_div_mod(remaining.next()?)?;
                Ok((Mult(Box::new(left), Box::new(right)), remaining))
            }
            Token::FSlash => {
                let (right, remaining) = parse_mult_div_mod(remaining.next()?)?;
                Ok((Div(Box::new(left), Box::new(right)), remaining))
            }
            Token::Percent => {
                let (right, remaining) = parse_mult_div_mod(remaining.next()?)?;
                Ok((Mod(Box::new(left), Box::new(right)), remaining))
            }
            _ => Ok((left, remaining)),
        }
    } else {
        Ok((left, remaining))
    }
}

fn parse_sum_diff(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (left, remaining) = parse_mult_div_mod(toks)?;
    let next_res = remaining.peek();
    if next_res.is_ok() {
        match next_res.unwrap() {
            Token::Plus => {
                let (right, remaining) = parse_sum_diff(remaining.next()?)?;
                Ok((Sum(Box::new(left), Box::new(right)), remaining))
            }
            Token::Minus => {
                let (right, remaining) = parse_sum_diff(remaining.next()?)?;
                Ok((Diff(Box::new(left), Box::new(right)), remaining))
            }
            _ => Ok((left, remaining)),
        }
    } else {
        Ok((left, remaining))
    }
}

fn parse_expr(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    parse_sum_diff(toks)
}

fn parse_stmt(toks: TokenList) -> ParseRes<(Statement, TokenList)> {
    let (tok, rem) = toks.eat()?;
    Ok(match tok {
        Token::If => todo!(),
        Token::Fi => todo!(),
        Token::For => todo!(),
        Token::Do => todo!(),
        Token::Done => todo!(),
        Token::Plus => todo!(),
        Token::Ident(_) => todo!("variable equals, func call"),
        Token::Print => {
            let (expr, rem) = parse_expr(rem)?;
            (Print(expr), rem)
        }
        tok => Err(ParseError::ExpectedStatement(tok))?,
    })
}

fn parse_global(toks: TokenList) -> ParseRes<Statement> {
    let (stmt, rem) = parse_stmt(toks)?;

    if !rem.empty() {
        Err(ExpectedEof(
            rem.peek()
                .expect("rem should not be empty so peek should succeed"),
        ))
    } else {
        Ok(stmt)
    }
}

pub fn parse(toks: &[Token]) -> ParseRes<Statement> {
    let tok_lst = TokenList::from(toks);
    parse_global(tok_lst)
}
