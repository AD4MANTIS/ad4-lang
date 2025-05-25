use std::str::FromStr;

use strum::{Display, IntoEnumIterator};

use crate::{Keyword, Literal, Operator, ParseLiteralError, Variable, literal::parse_float};

pub struct Lexer {
    tokens: Vec<Token>,
}

#[derive(thiserror::Error, Debug)]
pub enum TokenizeError {
    #[error(transparent)]
    Token(#[from] TokenError),
}

impl Lexer {
    pub fn build(input: &str) -> Result<Self, TokenizeError> {
        let mut tokens = tokenize(input)
            .collect::<Result<Vec<_>, _>>()
            .map_err(TokenizeError::from)?;
        tokens.reverse();
        Ok(Self { tokens })
    }

    #[must_use]
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.last()
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.pop()
    }
}

#[derive(thiserror::Error, Debug)]
pub enum TokenError {
    #[error(transparent)]
    ParseFloat(#[from] std::num::ParseFloatError),
    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("chars must contain exactly one character")]
    InvalidCharLength,
}

fn tokenize(input: &str) -> impl Iterator<Item = Result<Token, TokenError>> {
    let split_input = split_into_tokens(input);

    split_input.into_iter().map(str::parse)
}

fn split_into_tokens(input: &str) -> impl Iterator<Item = &str> {
    let mut current_literal_start: Option<char> = None;

    #[allow(
        clippy::needless_collect,
        reason = "Because the closure captures `current_literal_start` and we can't return a reference to this local variable"
    )]
    let split_input = input
        .split(|char| match char {
            '\'' | '"' => {
                if let Some(current_start) = current_literal_start.take() {
                    assert_eq!(
                        current_start, char,
                        "Expected matching closing delimiter {current_start}, found {char}"
                    );
                } else {
                    current_literal_start = Some(char);
                }
                false
            }
            _ => char.is_whitespace() && current_literal_start.is_none(),
        })
        .flat_map(|parsing_str| split_special_operators(parsing_str))
        .filter(|char| !char.is_empty())
        .collect::<Vec<_>>();

    assert!(
        current_literal_start.is_none(),
        "Expected closing delimiter for `{}`",
        current_literal_start.unwrap_or_default()
    );

    split_input.into_iter()
}

fn split_special_operators(parsing_str: &str) -> Vec<&str> {
    let tokens = std::iter::once(SEMICOLON).chain(Operator::iter().map(Operator::as_str));

    for token_str in tokens {
        let Some(op_pos) = parsing_str.find(token_str) else {
            continue;
        };

        if token_str == Operator::Dot.as_str() && parse_float(parsing_str).is_ok() {
            continue;
        }

        return [
            split_special_operators(&parsing_str[..op_pos]),
            vec![token_str],
            split_special_operators(&parsing_str[(op_pos + token_str.len())..]),
        ]
        .into_iter()
        .flatten()
        .collect();
    }

    vec![parsing_str]
}

#[derive(Debug, Clone, Display, PartialEq)]
pub enum Token {
    #[strum(to_string = "{0}")]
    Keyword(Keyword),
    #[strum(to_string = "{0}")]
    Op(Operator),
    #[strum(to_string = "{0}")]
    Literal(Literal),
    #[strum(to_string = "{0}")]
    Variable(Variable),
    #[strum(to_string = "{SEMICOLON}")]
    Semicolon(),
}

const SEMICOLON: &str = ";";

impl FromStr for Token {
    type Err = TokenError;

    fn from_str(token: &str) -> Result<Self, Self::Err> {
        if token == SEMICOLON {
            return Ok(Self::Semicolon());
        }

        if let Ok(keyword) = Keyword::try_from(token) {
            return Ok(Self::Keyword(keyword));
        }

        for op in Operator::iter() {
            if token == op.as_str() {
                return Ok(Self::Op(op));
            }
        }

        Ok(match Literal::from_str(token) {
            Ok(literal) => Self::Literal(literal),
            Err(ParseLiteralError::NotALiteral) => Self::Variable(Variable::new(token.to_string())),
            Err(ParseLiteralError::Token(e)) => return Err(e),
        })
    }
}
