use std::str::FromStr;

use crate::{Keyword, Literal, Operator, ParseLiteralError, Variable};
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
