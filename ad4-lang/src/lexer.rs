use std::str::FromStr;

use strum::{Display, EnumString, IntoEnumIterator};

use crate::{Literal, Operator, ParseLiteralError, prelude::Variable};

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

            Box::new(std::iter::once(Token::Variable(Variable::new(
                token.to_string(),
            ))))
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
    Variable(Variable),
}

#[derive(Debug, Clone, EnumString, Display)]
pub enum Keyword {
    Let,
}
