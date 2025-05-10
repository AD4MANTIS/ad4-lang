use std::fmt::Display;

use strum::{Display, EnumIter, EnumString, IntoEnumIterator, IntoStaticStr};

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

fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    input
        .split_whitespace()
        .flat_map(|token| -> Box<dyn Iterator<Item = Token>> {
            if let Ok(keyword) = Keyword::try_from(token) {
                return Box::new(std::iter::once(Token::Keyword(keyword)));
            }

            for operator in Operator::iter() {
                let operator_str: &str = operator.into();
                let Some(op_pos) = token.find(operator_str) else {
                    continue;
                };

                return Box::new(
                    tokenize(&token[0..op_pos])
                        .chain([Token::Op(operator)])
                        .chain(tokenize(&token[(op_pos + operator_str.len())..])),
                );
            }

            if let Ok(literal) = Literal::try_from(token) {
                return Box::new(std::iter::once(Token::Literal(literal)));
            }

            Box::new(std::iter::once(Token::Variable(token.to_string())))
        })
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
            Self::ClosingBracket => (0, 0),
            Self::Dot => (0, 21),
        }
    }
}

#[derive(Debug, Clone, EnumString, Display)]
pub enum Keyword {
    Let,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(String),
    Char(String),
}

impl TryFrom<&str> for Literal {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value
            .chars()
            .next()
            .and_then(|val| {
                Some(match val {
                    '0'..='9' => Self::Number(value.to_string()),
                    '\'' => Self::Char(value.to_string()),
                    '\"' => Self::String(value.to_string()),
                    _ => return None,
                })
            })
            .ok_or(())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::String(s) | Self::Number(s) | Self::Char(s) => s,
        })
    }
}
