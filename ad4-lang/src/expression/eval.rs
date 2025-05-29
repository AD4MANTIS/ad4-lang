use std::{collections::HashMap, fmt::Debug};

use crate::{Operator, Value, Variable};

use super::Expression;

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("{0}")]
    Static(&'static str),
    #[error("{0}")]
    String(String),
}

impl Expression {
    pub fn eval(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError> {
        match self {
            Self::Variable(variable) => variables
                .get(variable)
                .cloned()
                .ok_or_else(|| EvalError::String(format!("unknown variable '{}'", variable.name))),
            Self::Literal(literal) => Ok(literal.0.clone()),
            Self::Operation { operator, lhs, rhs } => {
                let rhs = rhs.eval(variables)?;

                match operator {
                    Operator::Add => (lhs.eval(variables)? + &rhs).map_err(EvalError::from),
                    Operator::Sub => (lhs.eval(variables)? - &rhs).map_err(EvalError::from),
                    Operator::Mul => (lhs.eval(variables)? * &rhs).map_err(EvalError::from),
                    Operator::Div => (lhs.eval(variables)? / &rhs).map_err(EvalError::from),
                    Operator::Eq => Ok((lhs.eval(variables)? == rhs).into()),
                    Operator::Neq => Ok((lhs.eval(variables)? != rhs).into()),
                    Operator::Assign => match &**lhs {
                        Self::Variable(var) => {
                            variables.insert(var.clone(), rhs.clone());
                            Ok(rhs)
                        }
                        _ => Err(EvalError::String(format!("Expected Variable, found {lhs}"))),
                    },
                    Operator::Dot => todo!(),
                    Operator::OpeningBracket
                    | Operator::ClosingBracket
                    | Operator::OpeningCurlyBrace
                    | Operator::ClosingCurlyBrace => Err(EvalError::String(format!(
                        "Expected Expression, found Operator '{operator}'"
                    ))),
                }
            }
            Self::Block {
                statements,
                result_expr,
            } => {
                for statement in statements {
                    statement.execute(variables)?;
                }

                result_expr
                    .as_ref()
                    .map_or(Ok(Value::Void), |expr| expr.eval(variables))
            }
        }
    }
}
