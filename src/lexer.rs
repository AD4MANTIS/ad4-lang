use std::fmt::Display;

use strum::{Display, EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};

use crate::Value;

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

            if let Some(literal) = try_parse_literal(token) {
                return Box::new(std::iter::once(Token::Literal(literal.unwrap())));
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

#[derive(Debug, Clone)]
pub struct Literal(pub Value);

fn try_parse_literal(value: &str) -> Option<Result<Literal, TokenError>> {
    Some(
        match value.chars().next().unwrap_or(' ') {
            '0'..='9' => {
                let n = value;
                if n.contains('.') {
                    #[allow(clippy::option_if_let_else)]
                    if let Some(f) = n.strip_suffix('f') {
                        f.parse().map(Value::F32).map_err(TokenError::from)
                    } else {
                        n.trim_end_matches('d')
                            .parse()
                            .map(Value::F64)
                            .map_err(TokenError::from)
                    }
                } else if let Some(n) = n.strip_suffix('u') {
                    n.parse().map(Value::U64).map_err(TokenError::from)
                } else {
                    n.trim_end_matches('i')
                        .parse()
                        .map(Value::I64)
                        .map_err(TokenError::from)
                }
            }
            '\'' => value
                .chars()
                .nth(1)
                .map(Value::Char)
                .ok_or(TokenError::InvalidCharLength),
            '\"' => Ok(Value::String(value[1..value.len() - 1].to_string())),
            _ => return None,
        }
        .map(Literal),
    )
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
