use self::ParseErrorKind::*;
use crate::ast::{Expr::*, Statement::*, *};
use crate::error::BacktracedError;
use crate::token::Token;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    Eof,
    ExpectedEof(Token),
    ExpectedStatement(Token),
    ExpectedExpr(Token),
    ExpectedTok(Token, Token),
    ExpectedIdent(Token),
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "Parser reached end of file but more input was expected"),
            Self::ExpectedStatement(tok) => {
                write!(
                    f,
                    "A statement was expected, but a \'{}\' token was found instead",
                    tok.kind()
                )
            }
            Self::ExpectedExpr(tok) => {
                write!(
                    f,
                    "An expression was expected, but a \'{}\' token was found instead",
                    tok.kind()
                )
            }
            Self::ExpectedEof(tok) => {
                write!(
                    f,
                    "The end of the file was expected, but a \'{}\' token was found instead",
                    tok.kind()
                )
            }
            Self::ExpectedTok(expected, received) => {
                write!(
                    f,
                    "A '{}' token was expected, but a '{}' was found instead",
                    expected.kind(),
                    received.kind()
                )
            }
            Self::ExpectedIdent(received) => {
                write!(
                    f,
                    "An identifier was expected, but a \'{}\' token was found instead",
                    received.kind()
                )
            }
        }
    }
}

pub type ParseError = BacktracedError<ParseErrorKind>;

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
        self.ptr
            .first()
            .map(|t| t.clone())
            .ok_or(ParseError::new(Eof))
    }
    pub fn eat(self) -> ParseRes<(Token, TokenList<'a>)> {
        let tok = self.ptr.first().ok_or(ParseError::new(Eof))?.clone();
        Ok((
            tok,
            TokenList {
                ptr: &self.ptr[1..],
            },
        ))
    }
    pub fn next(self) -> ParseRes<TokenList<'a>> {
        if self.empty() {
            Err(ParseError::new(Eof))
        } else {
            Ok(TokenList {
                ptr: &self.ptr[1..],
            })
        }
    }
    pub fn expect(self, tok: Token) -> ParseRes<TokenList<'a>> {
        let (next, remaining) = self.eat()?;
        if next == tok {
            Ok(remaining)
        } else {
            Err(ParseError::new(ExpectedTok(tok, next)))
        }
    }
    // pub fn lookahead(&self, n: usize) -> ParseRes<Token> {
    //     self.ptr.get(n).map(|t| t.clone()).ok_or(Eof)
    // }
    pub fn empty(&self) -> bool {
        self.ptr.len() < 1
    }
}

fn parse_lit_or_ident(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (next, remaining) = toks.eat()?;
    match next {
        Token::IntLit(i) => Ok((Int(i), remaining)),
        Token::True => Ok((Bool(true), remaining)),
        Token::False => Ok((Bool(false), remaining)),
        Token::StringLit(s) => Ok((String(s), remaining)),
        Token::Ident(name) => Ok((Ident(name), remaining)),
        found => Err(ParseError::new(ExpectedExpr(found))),
    }
}

fn parse_mult_div_mod(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (left, remaining) = parse_lit_or_ident(toks)?;
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

fn parse_comp(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (left, remaining) = parse_sum_diff(toks)?;
    let next_res = remaining.peek();
    if next_res.is_ok() {
        match next_res.unwrap() {
            Token::LT => {
                let (right, remaining) = parse_sum_diff(remaining.next()?)?;
                Ok((CompLT(Box::new(left), Box::new(right)), remaining))
            }
            Token::LE => {
                let (right, remaining) = parse_sum_diff(remaining.next()?)?;
                Ok((CompLE(Box::new(left), Box::new(right)), remaining))
            }
            Token::EQ => {
                let (right, remaining) = parse_sum_diff(remaining.next()?)?;
                Ok((CompEQ(Box::new(left), Box::new(right)), remaining))
            }
            Token::GE => {
                let (right, remaining) = parse_sum_diff(remaining.next()?)?;
                Ok((CompGE(Box::new(left), Box::new(right)), remaining))
            }
            Token::GT => {
                let (right, remaining) = parse_sum_diff(remaining.next()?)?;
                Ok((CompGT(Box::new(left), Box::new(right)), remaining))
            }
            _ => Ok((left, remaining)),
        }
    } else {
        Ok((left, remaining))
    }
}

fn parse_if(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (guard, remaining) = parse_expr(toks)?;
    let remaining = remaining.expect(Token::Then)?;
    let (block, remaining) = parse_block(remaining, Some(&[Token::Fi, Token::Else]))?;
    Ok(
        if let Token::Else = remaining
            .peek()
            .expect("there should be another token in the list")
        {
            let remaining = remaining.next()?;
            use Token as T;
            match remaining.peek()? {
                T::If => {
                    let (else_if, remaining) = parse_stmt(remaining)?;
                    if let Stmt(else_if_stmt) = &else_if {
                        if let FullIf(..) = **else_if_stmt {
                            (
                                Stmt(Box::new(FullIf(guard, Stmt(Box::new(block)), else_if))),
                                remaining,
                            )
                        } else {
                            (
                                Stmt(Box::new(HalfIf(
                                    guard,
                                    Stmt(Box::new(block)),
                                    Some(else_if),
                                ))),
                                remaining,
                            )
                        }
                    } else {
                        // this only runs if somehow we successfully parsed the else if clause but
                        // somehow it's not a statement (should be impossible)
                        panic!("else if statement is not actually a statement");
                    }
                }
                _ => {
                    let (else_block, remaining) = parse_block(remaining, Some(&[Token::Fi]))?;
                    (
                        Stmt(Box::new(FullIf(
                            guard,
                            Stmt(Box::new(block)),
                            Stmt(Box::new(else_block)),
                        ))),
                        // last token is fi
                        remaining.next()?,
                    )
                }
            }
        } else {
            // token is fi
            (
                Stmt(Box::new(HalfIf(guard, Stmt(Box::new(block)), None))),
                remaining.next()?,
            )
        },
    )
}

fn parse_let(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let (next, remaining) = toks.eat()?;
    match next {
        Token::Ident(name) => {
            let remaining = remaining.expect(Token::AssignEq)?;
            let (value, remaining) = parse_expr(remaining)?;
            Ok((Stmt(Box::new(Assign(name.clone(), value))), remaining))
        }
        tok => Err(ParseError::new(ExpectedIdent(tok))),
    }
}

fn parse_stmt(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    let tok = toks.peek()?;
    Ok(match tok {
        Token::If => parse_if(toks.next()?)?,
        Token::For => todo!(),
        Token::Do => todo!(),
        Token::Let => parse_let(toks.next()?)?,
        // Token::Ident(_) => todo!("variable equals, func call"), // TODO: implement reassignment (function call can be parse_expr)
        Token::Print => {
            let (expr, rem) = parse_expr(toks.next()?)?;
            (Stmt(Box::new(Print(expr))), rem)
        }
        // tok => Err(ParseError::new(ExpectedStatement(tok)))?,
        _ => parse_comp(toks)?,
    })
}

fn parse_expr(toks: TokenList) -> ParseRes<(Expr, TokenList)> {
    parse_stmt(toks)
}

fn parse_block<'a>(
    toks: TokenList<'a>,
    closing_toks: Option<&[Token]>,
) -> ParseRes<(Statement, TokenList<'a>)> {
    let mut statements = Vec::new();
    let mut rem_toks = toks;

    fn should_continue(rem_toks: &TokenList, closing_toks: Option<&[Token]>) -> bool {
        dbg!(rem_toks.empty());
        dbg!(closing_toks);
        dbg!(rem_toks.peek());

        !rem_toks.empty()
            && if let Some(closing_toks) = closing_toks {
                !closing_toks.contains(&rem_toks.peek().expect("rem_toks shouldn't be empty"))
            } else {
                true
            }
    }

    while should_continue(&rem_toks, closing_toks) {
        println!("continuing");
        let (stmt, remaining) = parse_stmt(rem_toks)?;
        println!("parsed statement: {stmt:?}");
        statements.push(stmt);
        rem_toks = remaining;
    }

    Ok((BlockStmt(statements.into()), rem_toks))
}

pub fn parse(toks: &[Token]) -> ParseRes<Expr> {
    let tok_lst = TokenList::from(toks);
    let (stmt, _) = parse_block(tok_lst, None)?;
    Ok(Stmt(Box::new(stmt)))
}
