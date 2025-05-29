use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

use crate::{Lexer, Literal, Operator, Token, TokenizeError, Value, prelude::Statement, statement};

use super::Expression;

pub trait DisplayDebug: Display + Debug {}

impl<T: Display + Debug> DisplayDebug for T {}

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    Lexer(#[from] TokenizeError),
    #[error("Expected Expression, but there were no tokens left")]
    NoTokensLeft,
    #[error("Expected {x}, found {y} '{found:?}'")]
    ExpectedXFoundY {
        x: &'static str,
        y: &'static str,
        found: Box<dyn DisplayDebug>,
    },
    #[error("Missing closing delimiter for `{for_opening}`")]
    MissingClosingDelimiter { for_opening: Operator },
    #[error("Mismatched closing delimiter for `{opening}`, expected {expected}, found {found}")]
    MismatchedClosingDelimiter {
        opening: Operator,
        expected: Operator,
        found: Token,
    },
    #[error("Unexpected end of expression")]
    UnexpectedEndOfExpression,
    #[error(transparent)]
    Statement(#[from] Box<statement::ParseError>),
}

impl FromStr for Expression {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lexer = Lexer::build(s)?;
        Self::parse(&mut lexer, 0)
    }
}

impl Expression {
    pub fn parse(lexer: &mut Lexer, binding_power_lhs: u32) -> Result<Self, ParseError> {
        let mut lhs = match lexer.next().ok_or(ParseError::NoTokensLeft)? {
            Token::Semicolon() => return Err(ParseError::UnexpectedEndOfExpression),
            Token::Variable(v) => Self::Variable(v),
            Token::Literal(literal) => Self::Literal(literal),
            Token::Keyword(keyword) => {
                return Err(ParseError::ExpectedXFoundY {
                    x: "Expression",
                    y: "Keyword",
                    found: Box::new(keyword),
                });
            }
            Token::Op(bracket @ Operator::OpeningBracket) => {
                let expr = Self::parse(lexer, bracket.infix_binding_power().1)?;

                expect_closing_bracket(lexer, bracket)?;

                expr
            }
            Token::Op(bracket @ Operator::OpeningCurlyBrace) => {
                let mut statements = vec![];

                let expr = loop {
                    if lexer.peek()
                        == Some(&Token::Op(
                            bracket.get_closing_bracket().expect("should be a bracket"),
                        ))
                    {
                        break None;
                    }

                    let statement = Statement::parse(lexer).map_err(Box::new)?;

                    if let Statement::Expr(expr) = statement {
                        break Some(expr);
                    }

                    statements.push(statement);
                };

                expect_closing_bracket(lexer, bracket)?;

                Self::Block {
                    statements,
                    result_expr: expr.map(Box::new),
                }
            }
            // Prefix operators
            Token::Op(operator @ (Operator::Add | Operator::Sub)) => Self::operation(
                Self::Literal(Literal(Value::I64(0))),
                operator,
                Self::parse(lexer, operator.infix_binding_power().1)?,
            ),
            Token::Op(operator) => {
                return Err(ParseError::ExpectedXFoundY {
                    x: "Expression",
                    y: "Operator",
                    found: Box::new(operator),
                });
            }
        };

        loop {
            let operator = match lexer.peek() {
                Some(Token::Op(operator)) => *operator,
                Some(Token::Semicolon()) | None => {
                    break;
                }
                Some(token) => {
                    return Err(ParseError::ExpectedXFoundY {
                        x: "Operator",
                        y: "Token",
                        found: Box::new(token.clone()),
                    });
                }
            };

            let infix_binding_power = operator.infix_binding_power();
            if infix_binding_power.0 <= binding_power_lhs {
                break;
            }
            lexer.next();

            let rhs = Self::parse(lexer, infix_binding_power.1)?;

            lhs = Self::operation(lhs, operator, rhs);
        }

        Ok(lhs)
    }
}

fn expect_closing_bracket(lexer: &mut Lexer, bracket: Operator) -> Result<(), ParseError> {
    let expected_closing_bracket = bracket.get_closing_bracket().expect("should be a bracket");

    let Some(found_closing_bracket) = lexer.next() else {
        return Err(ParseError::MissingClosingDelimiter {
            for_opening: bracket,
        });
    };

    if found_closing_bracket != Token::Op(expected_closing_bracket) {
        return Err(ParseError::MismatchedClosingDelimiter {
            opening: bracket,
            expected: expected_closing_bracket,
            found: found_closing_bracket,
        });
    }
    Ok(())
}
