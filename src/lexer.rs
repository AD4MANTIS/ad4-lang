use std::str::FromStr;

use strum::{Display, EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};

use crate::{Literal, ParseLiteralError};

pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    #[must_use]
    pub fn build(input: &str) -> Self {
        let mut tokens: Vec<_> = tokenize(input).collect();
        tokens.reverse();
        Self { tokens }
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

fn tokenize(input: &str) -> impl Iterator<Item = Token> {
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
        .filter(|char| !char.is_empty())
        .collect::<Vec<_>>();

    assert!(
        current_literal_start.is_none(),
        "Expected closing delimiter {}",
        current_literal_start.unwrap_or_default()
    );

    split_input
        .into_iter()
        .flat_map(|token| -> Box<dyn Iterator<Item = Token>> {
            if let Ok(keyword) = Keyword::try_from(token) {
                return Box::new(std::iter::once(Token::Keyword(keyword)));
            }

            if let Some(x) = Operator::iter()
                .filter(|op| *op != Operator::Dot)
                .find_map(|op| tokenize_operator(token, op))
            {
                return x;
            }

            match Literal::from_str(token) {
                Ok(literal) => return Box::new(std::iter::once(Token::Literal(literal))),
                Err(ParseLiteralError::NotALiteral) => {}
                Err(e) => panic!("{e:?}"),
            }

            if let Some(x) = tokenize_operator(token, Operator::Dot) {
                return x;
            }

            Box::new(std::iter::once(Token::Variable(token.to_string())))
        })
}

fn tokenize_operator(
    token: &str,
    operator: Operator,
) -> Option<Box<dyn Iterator<Item = Token> + '_>> {
    let operator_str: &str = operator.into();
    let op_pos = token.find(operator_str)?;

    Some(Box::new(
        tokenize(&token[..op_pos])
            .chain([Token::Op(operator)])
            .chain(tokenize(&token[(op_pos + operator_str.len())..])),
    ))
}

#[derive(Debug, Clone, Display)]
pub enum Token {
    Keyword(Keyword),
    Op(Operator),
    Literal(Literal),
    Variable(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr, EnumIter, Display)]
pub enum Operator {
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Sub,
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,
    #[strum(serialize = "==")]
    Eq,
    #[strum(serialize = "=")]
    Assign,
    #[strum(serialize = "(")]
    OpeningBracket,
    #[strum(serialize = ")")]
    ClosingBracket,
    #[strum(serialize = ".")]
    Dot,
}

impl Operator {
    #[must_use]
    pub const fn infix_binding_power(&self) -> (u32, u32) {
        match self {
            Self::Add | Self::Sub => (10, 11),
            Self::Div | Self::Mul => (12, 13),
            Self::Assign => (2, 1),
            Self::OpeningBracket => (0, 1),
            Self::Eq | Self::ClosingBracket => (0, 0),
            Self::Dot => (20, 21),
        }
    }
}

#[derive(Debug, Clone, EnumString, Display)]
pub enum Keyword {
    Let,
}
