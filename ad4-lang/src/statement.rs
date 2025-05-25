use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

use crate::{
    EvalError, Expression, Lexer, Literal, Value, Variable, expression,
    lexer::{Keyword, Token},
    prelude::Operator,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    Declaration(Variable, Expression),
}

fn error_found_unexpected_extra_text<T: Display>(found: Option<&T>) -> String {
    found
        .as_ref()
        .map(|token| format!(",found {token}"))
        .unwrap_or_default()
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Failed to parse Expression: {0}")]
    Expression(#[from] expression::ParseError),
    #[error("Expected variable name{}", error_found_unexpected_extra_text(.found.as_ref()))]
    ExpectedVariableName { found: Option<Token> },
    #[error("Expected `{token}`{}", error_found_unexpected_extra_text(.found.as_ref()))]
    Expected { token: Token, found: Option<Token> },
}

impl Statement {
    pub fn parse(lexer: &mut Lexer) -> Result<Self, ParseError> {
        macro_rules! expect_token {
            ($token:expr) => {
                let token = lexer.next();
                if Some($token) != token {
                    return Err(ParseError::Expected {
                        token: $token,
                        found: token,
                    });
                };
            };
        }

        Ok(match lexer.peek() {
            Some(Token::Keyword(Keyword::Let)) => {
                lexer.next();

                let var = match lexer.next() {
                    Some(Token::Variable(var)) => var,
                    rest => {
                        return Err(ParseError::ExpectedVariableName { found: rest });
                    }
                };

                expect_token!(Token::Op(Operator::Assign));

                let declaration = Self::Declaration(var, Expression::parse(lexer, 0)?);

                expect_token!(Token::Semicolon());

                declaration
            }
            Some(_) => Self::Expr(Expression::parse(lexer, 0)?),
            None => Self::Expr(Expression::Literal(Literal(Value::Void))),
        })
    }

    pub fn execute(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError> {
        match self {
            Self::Expr(expression) => expression.eval(variables),
            Self::Declaration(variable, expression) => {
                let value = expression.eval(variables)?;
                variables.insert(variable.clone(), value);

                Ok(Value::Void)
            }
        }
    }
}
