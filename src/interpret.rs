use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::ast::Block;
use crate::ast::Expr::{self, *};
use crate::ast::Statement::{self, *};
use crate::error::BacktracedError;

#[derive(Debug, Clone, Hash)]
pub enum Type {
    Int,
    Bool,
    String,
}

trait TryIntoWith<R, U> {
    fn try_into_with(self, uses: U) -> R;
}

impl TryIntoWith<InterpRes<Type>, &InterpreterState> for &Expr {
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
            Ident(name) => state.get_var_type(name)?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    TypeMismatch(Type, Type),
    InvalidVariableName(Rc<str>),
}

impl Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch(expected, received) => write!(
                f,
                "Type mismatch: expected {expected:?}, received {received:?}"
            ),
            Self::InvalidVariableName(name) => {
                write!(f, "Invalid variable name \"{name}\"")
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
        f: impl FnOnce(InterpreterState, T) -> InterpRes<InterpreterState>,
    ) -> InterpRes<InterpreterState> {
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

    fn interpret_if(
        state: InterpreterState,
        guard: &Expr,
        if_statement: &Statement,
        else_statement_opt: Option<&Statement>,
    ) -> InterpRes<InterpreterState> {
        state.execute1(guard, |state, guard: bool| {
            if guard {
                Self::interpret(state, if_statement)
            } else if let Some(else_statement) = else_statement_opt {
                Self::interpret(state, else_statement)
            } else {
                Ok(state)
            }
        })
    }

    fn interpret_block(state: InterpreterState, statements: &Block) -> InterpRes<InterpreterState> {
        let mut state = state;
        for s in statements {
            state = Self::interpret(state, s)?;
        }
        Ok(state)
    }

    pub fn interpret(state: InterpreterState, p: &Statement) -> InterpRes<InterpreterState> {
        match p {
            Print(e) => state.print_expr(e),
            If(guard, if_statement, else_statement) => Self::interpret_if(
                state,
                guard,
                if_statement,
                else_statement.as_ref().map(|s| s.as_ref()),
            ),
            BlockStmt(statements) => Self::interpret_block(state, statements),
            Assign(name, expr) => {
                let (resolved_expr, mut state) = state.resolve_expr(expr)?;
                state.vars.insert(name.clone(), resolved_expr);
                Ok(state)
            }
        }
    }
}

pub fn interpret(p: &Statement) -> InterpRes<()> {
    let _ = InterpreterState::interpret(InterpreterState::default(), p)?;
    Ok(())
}
