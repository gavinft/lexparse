use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Statement {
    Print(Expr),
    HalfIf(Expr, Expr, Option<Expr>),
    FullIf(Expr, Expr, Expr),
    Assign(Rc<str>, Expr),
    BlockStmt(Box<Block>),
}

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum Expr {
    Stmt(Box<Statement>),
    Sum(Box<Expr>, Box<Expr>),
    Diff(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    CompLT(Box<Expr>, Box<Expr>),
    CompLE(Box<Expr>, Box<Expr>),
    CompEQ(Box<Expr>, Box<Expr>),
    CompGE(Box<Expr>, Box<Expr>),
    CompGT(Box<Expr>, Box<Expr>),
    Int(i64),
    Bool(bool),
    String(Rc<str>),
    Unit,

    Ident(Rc<str>),
}

pub type Block = [Expr];
