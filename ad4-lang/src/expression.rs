use std::fmt::{Debug, Display, Formatter};

pub use eval::{Eval, EvalError};
pub use ifs::{If, SimpleIf};
pub use parse::ParseError;

use crate::{Block, Literal, Operator, Variable};

mod eval;
mod ifs;
mod loops;
mod parse;

#[cfg(test)]
mod test;

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable),
    Literal(Literal),
    Operation {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Block(Block),
    If(If),
    While(loops::While),
}

impl Expression {
    fn operation(lhs: Self, op: Operator, rhs: Self) -> Self {
        Self::Operation {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl From<String> for EvalError {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&'static str> for EvalError {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Variable(var) => write!(f, "{}", var.name),
            Self::Operation { operator, lhs, rhs } => write!(f, "({operator} {lhs} {rhs})"),
            Self::Block(block) => write!(f, "{block}"),
            Self::If(r#if) => write!(f, "{if}"),
            Self::While(r#while) => write!(f, "{while}"),
        }
    }
}
