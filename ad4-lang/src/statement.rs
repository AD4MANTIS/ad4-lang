use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

use crate::{
    EvalError, Expression, Keyword, Lexer, Literal, Operator, Token, Value, Variable,
    expression::{self, Eval},
    keyword,
};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    Declaration(Variable, Expression),
}

fn error_found_unexpected_extra_text<T: Display>(found: Option<&T>) -> String {
    found
        .map(|token| format!(", found {token}"))
        .unwrap_or_default()
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Failed to parse Expression: {0}")]
    Expression(#[from] expression::ParseError),
    #[error("Expected variable name{}", error_found_unexpected_extra_text(.found.as_ref()))]
    ExpectedVariableName { found: Option<Token> },
    #[error("Expected `{token}`{}", error_found_unexpected_extra_text(.found.as_ref()))]
    ExpectedToken { token: Token, found: Option<Token> },
}

macro_rules! consume_next_token {
    ($lexer:expr, $token:expr) => {
        let token = $lexer.next();
        if Some($token) != token {
            return Err(ParseError::ExpectedToken {
                token: $token,
                found: token,
            });
        };
    };
}

impl Statement {
    pub fn parse(lexer: &mut Lexer) -> Result<Self, ParseError> {
        Ok(match lexer.peek() {
            Some(Token::Keyword(Keyword::Declaration(declaration))) => {
                parse_declaration_statement(lexer, *declaration)?
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

fn parse_declaration_statement(
    lexer: &mut Lexer,
    declaration: keyword::Declaration,
) -> Result<Statement, ParseError> {
    Ok(match declaration {
        keyword::Declaration::Let => {
            lexer.next();

            let var = match lexer.next() {
                Some(Token::Variable(var)) => var,
                rest => {
                    return Err(ParseError::ExpectedVariableName { found: rest });
                }
            };

            consume_next_token!(lexer, Token::Op(Operator::Assign));

            let declaration = Statement::Declaration(var, Expression::parse(lexer, 0)?);

            consume_next_token!(lexer, Token::Semicolon());

            declaration
        }
        _ => todo!(),
    })
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expression) => expression.fmt(f),
            Self::Declaration(variable, expression) => {
                write!(f, "let {variable} = {expression}")
            }
        }
    }
}
