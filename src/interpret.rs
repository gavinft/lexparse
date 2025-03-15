use std::error::Error;
use std::fmt::Display;

use crate::ast::Expr::{self, *};
use crate::ast::Statement::{self, *};
use crate::ast::Program;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    TypeMismatch
}

impl Error for RuntimeError {}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type mismatch")
    }
}

type InterpRes<T> = Result<T, RuntimeError>;

fn type_int(e: &Expr) -> InterpRes<i64> {
    match e {
        Int(i) => Ok(*i),
        Bool(_) => Err(RuntimeError::TypeMismatch),
        Ident(_) => todo!("no idents yet ):"),
        e => type_int(&resolve_expr(e)?)
    }
}

fn type_bool(e: &Expr) -> InterpRes<bool> {
    match e {
        Int(_) => Err(RuntimeError::TypeMismatch),
        Bool(b) => Ok(*b),
        Ident(_) => todo!("no idents yet ):"),
        e => type_bool(&resolve_expr(e)?)
    }
}

fn resolve_expr(e: &Expr) -> InterpRes<Expr> {
    match e {
        Sum(left, right) => Ok(Int(type_int(left)? + type_int(right)?)),
        Diff(left, right) => Ok(Int(type_int(left)? - type_int(right)?)),
        Mult(left, right) => Ok(Int(type_int(left)? * type_int(right)?)),
        Div(left, right) => Ok(Int(type_int(left)? / type_int(right)?)),
        Mod(left, right) => Ok(Int(type_int(left)? % type_int(right)?)),
        CompLT(left, right) => Ok(Bool(type_int(left)? < type_int(right)?)),
        CompLE(left, right) => Ok(Bool(type_int(left)? <= type_int(right)?)),
        CompEQ(left, right) => Ok(Bool(type_int(left)? == type_int(right)?)),
        CompGE(left, right) => Ok(Bool(type_int(left)? >= type_int(right)?)),
        CompGT(left, right) => Ok(Bool(type_int(left)? > type_int(right)?)),
        Int(i) => Ok(Int(*i)),
        Bool(b) => Ok(Bool(*b)),
        Ident(_) => todo!(),
    }
}

fn print_expr(e: &Expr) -> InterpRes<()> {
    match e {
        Int(i) => Ok(println!("{i}")),
        Bool(b) => Ok(println!("{b}")),
        Ident(_) => todo!(),
        other => print_expr(&resolve_expr(other)?),
    }
}

pub fn interpret(p: &Program) -> InterpRes<()> {
    for s in p {
        match s {
            Print(e) => print_expr(e)?,
        }
    }
    Ok(())
}