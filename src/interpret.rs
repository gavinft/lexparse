use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::ast::Block;
use crate::ast::Expr::{self, *};
use crate::ast::Statement::*;
use crate::error::BacktracedError;

#[derive(Debug, Clone, Hash)]
pub enum Type {
    Unit,
    Int,
    Bool,
    String,
}

trait TryIntoWith<R, U> {
    fn try_into_with(self, uses: U) -> R;
}

impl TryIntoWith<InterpRes<Type>, &InterpreterState> for &Expr {
    /**
     * Converts an expression into its type
     */
    fn try_into_with(self, state: &InterpreterState) -> InterpRes<Type> {
        Ok(match self {
            Sum(..) => Type::Int,
            Diff(..) => Type::Int,
            Mult(..) => Type::Int,
            Div(..) => Type::Int,
            Mod(..) => Type::Int,
            CompLT(..) => Type::Bool,
            CompLE(..) => Type::Bool,
            CompEQ(..) => Type::Bool,
            CompGE(..) => Type::Bool,
            CompGT(..) => Type::Bool,
            Int(_) => Type::Int,
            Bool(_) => Type::Bool,
            String(_) => Type::String,
            Unit => Type::Unit,
            Ident(name) => state.get_var_type(name)?,
            Stmt(statement) => match &**statement {
                Print(_) => Type::Unit,
                HalfIf(..) => Type::Unit,
                FullIf(_, _, else_clause_expr) => else_clause_expr.try_into_with(state)?,
                Assign(..) => Type::Unit,
                Reassign(..) => Type::Unit,
                BlockStmt(exprs) => exprs
                    .last()
                    .map(|e| e.try_into_with(state))
                    .unwrap_or(Ok(Type::Unit))?,
            },
        })
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    TypeMismatch(Type, Type),
    InvalidVariableName(Rc<str>),
    UnknownIdentAssignment(Rc<str>),
}

impl Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch(expected, received) => {
                write!(
                    f,
                    "Type mismatch: expected {expected:?}, received {received:?}"
                )
            }
            Self::InvalidVariableName(name) => {
                write!(f, "Invalid variable name \"{name}\"")
            }
            Self::UnknownIdentAssignment(name) => {
                write!(f, "There is no variable with name \"{name}\"")
            }
        }
    }
}

pub type RuntimeError = BacktracedError<RuntimeErrorKind>;

pub type InterpRes<T> = Result<T, RuntimeError>;

trait FiniteType<T> {
    fn convert(state: InterpreterState, e: &Expr) -> InterpRes<(T, InterpreterState)>;
}

impl FiniteType<i64> for i64 {
    fn convert(state: InterpreterState, e: &Expr) -> InterpRes<(i64, InterpreterState)> {
        match e {
            Int(i) => Ok((*i, state)),
            Bool(_) | String(_) => Err(RuntimeError::new(RuntimeErrorKind::TypeMismatch(
                Type::Int,
                e.try_into_with(&state)?,
            ))),
            e => {
                let (value, state) = state.resolve_expr(e)?;
                Self::convert(state, &value)
            }
        }
    }
}

impl FiniteType<bool> for bool {
    fn convert(state: InterpreterState, e: &Expr) -> InterpRes<(bool, InterpreterState)> {
        match e {
            Int(_) | String(_) => Err(RuntimeError::new(RuntimeErrorKind::TypeMismatch(
                Type::Bool,
                e.try_into_with(&state)?,
            ))),
            Bool(b) => Ok((*b, state)),
            e => {
                let (value, state) = state.resolve_expr(e)?;
                Self::convert(state, &value)
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
struct InterpreterState {
    vars: HashMap<Rc<str>, Expr>,
}

impl InterpreterState {
    fn execute1<T: FiniteType<T>>(
        self,
        e: &Expr,
        f: impl FnOnce(InterpreterState, T) -> InterpRes<(Expr, InterpreterState)>,
    ) -> InterpRes<(Expr, InterpreterState)> {
        let (val, state) = T::convert(self, e)?;
        f(state, val)
    }

    fn compute2<T: FiniteType<T>, U: FiniteType<U>>(
        self,
        et: &Expr,
        eu: &Expr,
        f: impl FnOnce(T, U) -> Expr,
    ) -> InterpRes<(Expr, Self)> {
        let (valt, state) = T::convert(self, et)?;
        let (valu, state) = U::convert(state, eu)?;
        Ok((f(valt, valu), state))
    }

    pub fn resolve_expr(self, e: &Expr) -> InterpRes<(Expr, Self)> {
        match e {
            Int(i) => Ok((Int(*i), self)),
            Bool(b) => Ok((Bool(*b), self)),
            String(s) => Ok((String(s.clone()), self)),
            Unit => Ok((Unit, self)),
            Ident(s) => {
                let e_res = self
                    .vars
                    .get(s)
                    .ok_or_else(|| {
                        RuntimeError::new(RuntimeErrorKind::InvalidVariableName(s.clone()))
                    })
                    .map(|e| e.clone());
                e_res.map(|e| (e, self))
            }
            Sum(l, r) => self.compute2(l, r, |left: i64, right: i64| Int(left + right)),
            Diff(l, r) => self.compute2(l, r, |left: i64, right: i64| Int(left - right)),
            Mult(l, r) => self.compute2(l, r, |left: i64, right: i64| Int(left * right)),
            Div(l, r) => self.compute2(l, r, |left: i64, right: i64| Int(left / right)),
            Mod(l, r) => self.compute2(l, r, |left: i64, right: i64| Int(left % right)),
            CompLT(l, r) => self.compute2(l, r, |left: i64, right: i64| Bool(left < right)),
            CompLE(l, r) => self.compute2(l, r, |left: i64, right: i64| Bool(left <= right)),
            CompEQ(l, r) => self.compute2(l, r, |left: i64, right: i64| Bool(left == right)),
            CompGE(l, r) => self.compute2(l, r, |left: i64, right: i64| Bool(left >= right)),
            CompGT(l, r) => self.compute2(l, r, |left: i64, right: i64| Bool(left > right)),
            Stmt(_) => Self::interpret(self, e),
        }
    }

    fn get_var_type(&self, name: &Rc<str>) -> InterpRes<Type> {
        let e = self.vars.get(name);
        e.ok_or_else(|| RuntimeError::new(RuntimeErrorKind::InvalidVariableName(name.clone())))
            .and_then(|e| e.try_into_with(self))
    }

    fn print_expr(self, e: &Expr) -> InterpRes<InterpreterState> {
        match e {
            Int(i) => {
                println!("{i}");
                Ok(self)
            }
            Bool(b) => {
                println!("{b}");
                Ok(self)
            }
            String(s) => {
                println!("{s}");
                Ok(self)
            }
            other => {
                let (e, state) = self.resolve_expr(other)?;
                state.print_expr(&e)
            }
        }
    }

    fn interpret_partial_if(
        state: InterpreterState,
        guard: &Expr,
        if_statement: &Expr,
        else_statement_opt: Option<&Expr>,
    ) -> InterpRes<(Expr, InterpreterState)> {
        state.execute1(guard, |state, guard: bool| {
            if guard {
                let (_, state) = state.resolve_expr(if_statement)?;
                Ok((Unit, state))
            } else if let Some(else_statement) = else_statement_opt {
                let (_, state) = state.resolve_expr(else_statement)?;
                Ok((Unit, state))
            } else {
                Ok((Unit, state))
            }
        })
    }

    fn interpret_full_if(
        state: InterpreterState,
        guard: &Expr,
        if_statement: &Expr,
        else_statement: &Expr,
    ) -> InterpRes<(Expr, InterpreterState)> {
        state.execute1(guard, |state, guard: bool| {
            if guard {
                state.resolve_expr(if_statement)
            } else {
                state.resolve_expr(else_statement)
            }
        })
    }

    fn interpret_block(
        state: InterpreterState,
        statements: &Block,
    ) -> InterpRes<(Expr, InterpreterState)> {
        let mut state = state;
        let mut ret = Unit;
        for s in statements {
            (ret, state) = Self::interpret(state, s)?;
        }
        Ok((ret, state))
    }

    pub fn interpret(state: InterpreterState, p: &Expr) -> InterpRes<(Expr, InterpreterState)> {
        if let Stmt(s) = p {
            match &**s {
                Print(e) => {
                    let state = state.print_expr(e)?;
                    Ok((Unit, state))
                }
                HalfIf(guard, if_statement, else_statement) => {
                    Self::interpret_partial_if(state, guard, if_statement, else_statement.as_ref())
                }
                FullIf(guard, if_statement, else_statement) => {
                    Self::interpret_full_if(state, guard, if_statement, else_statement)
                }
                BlockStmt(statements) => Self::interpret_block(state, statements),
                Assign(name, expr) => {
                    let (resolved_expr, mut state) = state.resolve_expr(expr)?;
                    state.vars.insert(name.clone(), resolved_expr);
                    Ok((Unit, state))
                }
                Reassign(name, expr) => {
                    if !state.vars.contains_key(name) {
                        Err(RuntimeError::new(RuntimeErrorKind::UnknownIdentAssignment(name.clone())))
                    } else {
                        let (resolved_expr, mut state) = state.resolve_expr(expr)?;
                        state.vars.insert(name.clone(), resolved_expr);
                        Ok((Unit, state))
                    }
                }
            }
        } else {
            state.resolve_expr(p)
        }
    }
}

pub fn interpret(p: &Expr) -> InterpRes<()> {
    let _ = InterpreterState::interpret(InterpreterState::default(), p)?;
    Ok(())
}
