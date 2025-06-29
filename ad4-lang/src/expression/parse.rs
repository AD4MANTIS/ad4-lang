use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

use crate::{
    Lexer, Literal, Operator, Token, Value,
    expression::{VecExpression, loops},
    keyword::{self, Keyword},
    lexer::TokenizeError,
    prelude::Statement,
    statement,
};

use super::{Block, Expression, SimpleIf};

pub trait DisplayDebug: Display + Debug {}

impl<T: Display + Debug> DisplayDebug for T {}

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    Lexer(#[from] TokenizeError),
    #[error("Expected Expression, but there were no tokens left")]
    NoTokensLeft,
    #[error("Expected {x}, found {found} '{found_item}'")]
    ExpectedXFoundY {
        x: Box<dyn DisplayDebug>,
        found: Box<dyn DisplayDebug>,
        found_item: Box<dyn DisplayDebug>,
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

impl ParseError {
    fn expected<T: DisplayDebug + 'static, U: DisplayDebug + 'static, V: DisplayDebug + 'static>(
        x: T,
        found: U,
        found_item: V,
    ) -> Self {
        Self::ExpectedXFoundY {
            x: Box::new(x),
            found: Box::new(found),
            found_item: Box::new(found_item),
        }
    }
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
            Token::Keyword(Keyword::Expression(keyword)) => match keyword {
                keyword::Expression::If => {
                    let r#if = SimpleIf::parse(lexer)?;

                    let mut elses = vec![];
                    let r#else = loop {
                        if !lexer.is_next(&Token::kw(Keyword::else_())) {
                            break None;
                        }

                        lexer.next();

                        if lexer.is_next(&Token::kw(Keyword::if_())) {
                            lexer.next();
                            elses.push(SimpleIf::parse(lexer)?);
                        } else {
                            break Some(Block::parse(lexer, Operator::OpeningCurlyBrace)?);
                        }
                    };

                    Self::If(super::If {
                        condition: r#if.condition,
                        block: r#if.block,
                        elses,
                        r#else,
                    })
                }
                keyword::Expression::While => {
                    let condition = Box::new(Self::parse(lexer, 0)?);

                    Self::While(loops::While {
                        condition,
                        block: Block::parse(lexer, Operator::OpeningCurlyBrace)?,
                    })
                }
                _ => todo!(),
            },
            Token::Keyword(keyword) => {
                return Err(ParseError::expected("Expression", "Keyword", keyword));
            }
            Token::Op(bracket @ Operator::OpeningBracket) => {
                let expr = Self::parse(lexer, bracket.infix_binding_power().1)?;

                expect_closing_bracket(lexer, bracket)?;

                expr
            }
            Token::Op(square_bracket @ Operator::OpeningSquareBracket) => {
                let mut items = vec![];

                while lexer.peek()
                    != Some(&Token::Op(
                        square_bracket
                            .get_closing_bracket()
                            .expect("Should be ClosingSquareBracket"),
                    ))
                {
                    if !items.is_empty() {
                        match lexer.next() {
                            Some(Token::Op(Operator::Comma)) => {}
                            Some(token) => {
                                return Err(ParseError::expected(Operator::Comma, "", token));
                            }
                            None => return Err(ParseError::UnexpectedEndOfExpression),
                        }
                    }
                    items.push(Self::parse(lexer, square_bracket.infix_binding_power().1)?);
                }

                Self::Vec(VecExpression { items })
            }
            Token::Op(brace @ Operator::OpeningCurlyBrace) => {
                Self::Block(Block::parse(lexer, brace)?)
            }
            // Prefix operators
            Token::Op(operator @ (Operator::Add | Operator::Sub)) => Self::operation(
                Self::Literal(Literal(Value::I64(0))),
                operator,
                Self::parse(lexer, operator.infix_binding_power().1)?,
            ),
            Token::Op(operator) => {
                return Err(ParseError::expected("Expression", "Operator", operator));
            }
        };

        loop {
            let operator = match lexer.peek() {
                Some(Token::Op(operator)) => *operator,
                Some(Token::Semicolon()) | None => {
                    break;
                }
                Some(token) => {
                    return Err(ParseError::expected("Operator", "Token", token.clone()));
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

impl Block {
    fn parse(lexer: &mut Lexer, bracket: Operator) -> Result<Self, ParseError> {
        let closing = Token::Op(bracket.get_closing_bracket().expect("should be a bracket"));

        if lexer.peek() == Some(&Token::Op(bracket)) {
            lexer.next();
        }

        let mut statements = vec![];

        let expr = loop {
            if lexer.peek() == Some(&closing) {
                break None;
            }

            let statement = Statement::parse(lexer).map_err(Box::new)?;

            if let Statement::Expr(expr) = statement {
                break Some(expr);
            }

            statements.push(statement);
        };

        expect_closing_bracket(lexer, bracket)?;

        Ok(Self {
            statements,
            result_expr: expr.map(Box::new),
        })
    }
}

impl SimpleIf {
    fn parse(lexer: &mut Lexer) -> Result<Self, ParseError> {
        let condition = Box::new(Expression::parse(lexer, 0)?);

        let block = Block::parse(lexer, Operator::OpeningCurlyBrace)?;

        Ok(Self { condition, block })
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
