use std::str::FromStr;

#[cfg(test)]
use crate::Value;
use crate::{Keyword, Literal, Operator, Variable, literal::ParseError};

use strum::{Display, IntoEnumIterator};

pub const SEMICOLON: &str = ";";

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

#[derive(thiserror::Error, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum TokenError {
    #[error(transparent)]
    ParseFloat(#[from] std::num::ParseFloatError),
    #[error(transparent)]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("chars must contain exactly one character")]
    InvalidCharLength,
}

impl FromStr for Token {
    type Err = TokenError;

    fn from_str(token: &str) -> Result<Self, Self::Err> {
        if token == SEMICOLON {
            return Ok(Self::Semicolon());
        }

        if let Ok(keyword) = Keyword::from_str(token) {
            return Ok(Self::kw(keyword));
        }

        for op in Operator::iter() {
            if token == op.as_str() {
                return Ok(Self::Op(op));
            }
        }

        Ok(match Literal::from_str(token) {
            Ok(literal) => Self::Literal(literal),
            Err(ParseError::NotALiteral) => Self::var(token),
            Err(ParseError::Token(e)) => return Err(e),
        })
    }
}

impl Token {
    pub const fn kw(kw: Keyword) -> Self {
        Self::Keyword(kw)
    }

    #[cfg(test)]
    pub const fn literal(val: Value) -> Self {
        Self::Literal(Literal(val))
    }

    pub fn var(name: &str) -> Self {
        Self::Variable(Variable::new(name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! expect_error {
        ($input:expr, $expected:pat) => {
            let expr = Token::from_str($input).unwrap_err();
            assert!(
                matches!(&expr, $expected),
                "Input: {}\nExpected: {:?}\nActual: {:?}",
                $input,
                stringify!($expected),
                &expr
            );
        };
    }

    #[test]
    fn semicolon() {
        assert_eq!(Token::from_str(";"), Ok(Token::Semicolon()));
    }

    #[test]
    fn char() {
        assert_eq!(Token::from_str("'a'"), Ok(Token::literal('a'.into())));
    }

    #[test]
    fn float() {
        assert_eq!(Token::from_str("1.0"), Ok(Token::literal(1.0.into())));
    }

    #[test]
    fn string() {
        assert_eq!(
            Token::from_str(r#""Hi""#),
            Ok(Token::literal("Hi".to_string().into()))
        );
    }

    #[test]
    fn number() {
        assert_eq!(Token::from_str("1"), Ok(Token::literal(1.into())));
    }

    #[test]
    fn keyword() {
        assert_eq!(Token::from_str("let"), Ok(Token::kw(Keyword::let_())));
    }

    #[test]
    fn operator() {
        for op in Operator::iter() {
            assert_eq!(Token::from_str(op.as_str()), Ok(Token::Op(op)));
        }
    }

    #[test]
    fn variable() {
        assert_eq!(Token::from_str("a"), Ok(Token::var("a")));
    }

    #[test]
    fn invalid_char_length() {
        expect_error!("'a", TokenError::InvalidCharLength);
        expect_error!("''", TokenError::InvalidCharLength);
        expect_error!("'1a'", TokenError::InvalidCharLength);
    }

    #[test]
    fn invalid() {
        expect_error!("1a", TokenError::ParseInt(..));
        expect_error!("1e1a", TokenError::ParseInt(..));
        expect_error!("1a", TokenError::ParseInt(..));
        expect_error!("1a", TokenError::ParseInt(..));
        expect_error!("1.2a", TokenError::ParseFloat(..));
        expect_error!("1.a", TokenError::ParseFloat(..));
        expect_error!("1ad", TokenError::ParseFloat(..));
    }
}
