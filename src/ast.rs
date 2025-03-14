use std::rc::Rc;

#[derive(Debug, Clone, Hash)]
pub enum Statement {
    Print(Expr),
}

#[derive(Debug, Clone, Hash)]
pub enum Expr {
    Sum(Box<Expr>, Box<Expr>),
    Diff(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Int(i64),
    Ident(Rc<str>),
}

pub type Program = [Statement];
