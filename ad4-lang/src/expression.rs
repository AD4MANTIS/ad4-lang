use std::fmt::{Debug, Display};

pub use eval::EvalError;
pub use parse::ParseError;

use crate::{Literal, Operator, Variable};

mod eval;
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
    // Block(Vec<Statement>, Option<Box<Expression>>),
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Variable(var) => write!(f, "{}", var.name),
            Self::Operation { operator, lhs, rhs } => write!(f, "({operator} {lhs} {rhs})"),
        }
    }
}
