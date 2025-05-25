use crate::{Expression, prelude::Variable};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    Declaration(Variable, Expression),
}
