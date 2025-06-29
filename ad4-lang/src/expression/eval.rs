use std::{collections::HashMap, fmt::Debug};

use crate::{Operator, Value, Variable, expression::loops};

use super::{Block, Expression, If};

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("{0}")]
    Static(&'static str),
    #[error("{0}")]
    String(String),
    #[error("Expression in an if Condition must be of type `bool`, found {found}")]
    IfConditionMustBeABool { found: Value },
}

pub trait Eval {
    fn eval(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError>;
}

impl Eval for Expression {
    fn eval(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError> {
        match self {
            Self::Variable(variable) => variables
                .get(variable)
                .cloned()
                .ok_or_else(|| EvalError::String(format!("unknown variable '{}'", variable.name))),
            Self::Literal(literal) => Ok(literal.0.clone()),
            Self::Operation { operator, lhs, rhs } => {
                type Op = Operator;

                let rhs = rhs.eval(variables)?;
                let mut get_lhs = || lhs.eval(variables);

                match operator {
                    Op::Add => (get_lhs()? + &rhs).map_err(EvalError::from),
                    Op::Sub => (get_lhs()? - &rhs).map_err(EvalError::from),
                    Op::Mul => (get_lhs()? * &rhs).map_err(EvalError::from),
                    Op::Div => (get_lhs()? / &rhs).map_err(EvalError::from),
                    Op::Eq => Ok((get_lhs()? == rhs).into()),
                    Op::Neq => Ok((get_lhs()? != rhs).into()),
                    Op::Assign => match &**lhs {
                        Self::Variable(var) => {
                            variables.insert(var.clone(), rhs.clone());
                            Ok(rhs)
                        }
                        _ => Err(EvalError::String(format!("Expected Variable, found {lhs}"))),
                    },
                    Op::Lt => Ok((get_lhs()? < rhs).into()),
                    Op::Leq => Ok((get_lhs()? <= rhs).into()),
                    Op::Gt => Ok((get_lhs()? > rhs).into()),
                    Op::Geq => Ok((get_lhs()? >= rhs).into()),
                    Op::Dot => todo!(),
                    Op::OpeningBracket
                    | Op::ClosingBracket
                    | Op::OpeningCurlyBrace
                    | Op::ClosingCurlyBrace
                    | Op::OpeningSquareBracket
                    | Op::ClosingSquareBracket => Err(EvalError::String(format!(
                        "Expected Expression, found Operator '{operator}'"
                    ))),
                }
            }
            Self::Block(block) => block.eval(variables),
            Self::Vec(vec) => Ok(Value::Vec(
                vec.items
                    .iter()
                    .map(|item_expr| item_expr.eval(variables))
                    .collect::<Result<_, _>>()?,
            )),
            Self::If(r#if) => r#if.eval(variables),
            Self::While(r#while) => r#while.eval(variables),
        }
    }
}

impl Eval for Block {
    fn eval(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError> {
        for statement in &self.statements {
            statement.execute(variables)?;
        }

        self.result_expr
            .as_ref()
            .map_or(Ok(Value::Void), |expr| expr.eval(variables))
    }
}

fn check_if_condition(condition: Value) -> Result<bool, EvalError> {
    match condition {
        Value::Bool(condition) => Ok(condition),
        rest => Err(EvalError::IfConditionMustBeABool { found: rest }),
    }
}

fn check_if_conditions<'a>(
    r#if: &'a If,
    variables: &mut HashMap<Variable, Value>,
) -> Result<Option<&'a Block>, EvalError> {
    let condition = check_if_condition(r#if.condition.eval(variables)?)?;

    Ok(if condition {
        Some(&r#if.block)
    } else {
        for r#else in &r#if.elses {
            if check_if_condition(r#else.condition.eval(variables)?)? {
                return Ok(Some(&r#else.block));
            }
        }

        r#if.r#else.as_ref()
    })
}

impl Eval for If {
    fn eval(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError> {
        let block = check_if_conditions(self, variables)?;

        block.map_or(Ok(Value::Void), |block| block.eval(variables))
    }
}

impl Eval for loops::While {
    fn eval(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError> {
        while self.condition.eval(variables)? == Value::Bool(true) {
            self.block.eval(variables)?;
        }

        Ok(Value::Void)
    }
}
