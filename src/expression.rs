use std::{collections::HashMap, fmt::Display};

use crate::{Lexer, Literal, Operator, Token, Value};

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    Literal(Literal),
    Operation(Operator, [Box<Expression>; 2]),
}

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("{0}")]
    Static(&'static str),
    #[error(transparent)]
    ParseFloat(#[from] std::num::ParseFloatError),
    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),
}

impl Expression {
    pub fn parse(lexer: &mut Lexer, binding_power_lhs: u32) -> Self {
        let mut expr = match lexer.next().expect("expected Token") {
            Token::Variable(v) => Self::Variable(v),
            Token::Literal(literal) => Self::Literal(literal),
            Token::Keyword(keyword) => panic!("expected Expression, found Keyword '{keyword}'"),
            Token::Op(bracket @ Operator::OpeningBracket) => {
                let expr = Self::parse(lexer, bracket.infix_binding_power().1);
                assert!(matches!(
                    lexer.next(),
                    Some(Token::Op(Operator::ClosingBracket))
                ));
                expr
            }
            Token::Op(operator) => panic!("expected Expression, found Operator '{operator}'"),
        };

        loop {
            let operator = match lexer.peek() {
                Some(Token::Op(operator)) => *operator,
                Some(token) => {
                    panic!("expected Operator, found '{token}'")
                }
                None => {
                    break;
                }
            };

            let infix_binding_power = operator.infix_binding_power();
            if infix_binding_power.0 < binding_power_lhs {
                break;
            }
            lexer.next();

            let rhs = Self::parse(lexer, infix_binding_power.1);

            expr = Self::Operation(operator, [Box::new(expr), Box::new(rhs)]);
        }

        expr
    }

    #[must_use]
    pub fn from_str(s: &str) -> Self {
        let mut lexer = Lexer::build(s);
        Self::parse(&mut lexer, 0)
    }

    pub fn eval(&self, variables: &HashMap<String, Value>) -> Result<Value, EvalError> {
        match self {
            Self::Variable(name) => variables
                .get(name)
                .cloned()
                .ok_or(EvalError::Static("unknown variable")),
            Self::Literal(literal) => match literal {
                Literal::String(s) => Ok(s.clone().into()),
                Literal::Number(n) => {
                    if n.contains('.') {
                        n.parse().map(Value::Float).map_err(EvalError::from)
                    } else if n.ends_with('u') {
                        n[0..n.len() - 1]
                            .parse()
                            .map(Value::U64)
                            .map_err(EvalError::from)
                    } else {
                        n.trim_end_matches('i')
                            .parse()
                            .map(Value::I64)
                            .map_err(EvalError::from)
                    }
                }
                Literal::Char(c) => {
                    let char: [char; 1] = c
                        .trim_end_matches('\'')
                        .chars()
                        .collect::<Vec<_>>()
                        .try_into()
                        .expect("chars must contain exactly one character");

                    Ok(char[0].into())
                }
            },
            Self::Operation(operator, expressions) => {
                let lhs = expressions[0].eval(variables)?;
                let rhs = expressions[1].eval(variables)?;

                match operator {
                    Operator::Add => (lhs + &rhs).ok_or(EvalError::Static("invalid operation")),
                    Operator::Sub => todo!(),
                    Operator::Mul => todo!(),
                    Operator::Div => todo!(),
                    Operator::Assign => todo!(),
                    Operator::OpeningBracket => todo!(),
                    Operator::ClosingBracket => todo!(),
                    Operator::Dot => todo!(),
                }
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => literal.fmt(f),
            Self::Variable(var) => var.fmt(f),
            Self::Operation(operator, expressions) => f.write_fmt(format_args!(
                "({operator} {} {})",
                expressions[0], expressions[1]
            )),
        }
    }
}
