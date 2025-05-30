use std::fmt::{Debug, Display, Formatter};

pub use eval::{Eval, EvalError};
pub use parse::ParseError;

use crate::{Literal, Operator, Statement, Variable};

mod eval;
mod parse;

#[cfg(test)]
mod test;

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub result_expr: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub block: Block,
    pub elses: Vec<SimpleIf>,
    pub r#else: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct SimpleIf {
    pub condition: Box<Expression>,
    pub block: Block,
}

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
        }
    }
}

impl Display for If {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.block)?;

        for else_if in &self.elses {
            write!(f, "\nelse {else_if}")?;
        }

        if let Some(r#else) = &self.r#else {
            write!(f, "\nelse {else}")?;
        }

        Ok(())
    }
}

impl Display for SimpleIf {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.block)
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match (self.statements.as_slice(), &self.result_expr) {
            ([], None) => write!(f, "{{ }}"),
            ([], Some(expr)) => write!(f, "{{ {expr} }}"),
            (_, _) => {
                const INDENT: &str = "    ";

                writeln!(f, "{{")?;

                for statement in &self.statements {
                    writeln!(f, "{INDENT}{statement};")?;
                }

                if let Some(expr) = &self.result_expr {
                    write!(f, "{INDENT}{expr}",)?;
                }

                writeln!(f)?;

                write!(f, "}}")
            }
        }
    }
}
