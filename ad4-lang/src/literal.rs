use std::{fmt::Display, num::ParseFloatError, str::FromStr};

use crate::{TokenError, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Literal(pub Value);

#[derive(thiserror::Error, Debug)]
pub enum ParseLiteralError {
    #[error("Token is not a literal")]
    NotALiteral,
    #[error(transparent)]
    Token(#[from] TokenError),
}

impl FromStr for Literal {
    type Err = ParseLiteralError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value.chars().next().unwrap_or(' ') {
            '0'..='9' => {
                let n = value;

                if n.contains('.') || n.ends_with(['f', 'd']) {
                    parse_float(n).map_err(TokenError::from)
                } else if let Some(n) = n.strip_suffix("i32") {
                    n.parse().map(Value::I32).map_err(TokenError::from)
                } else if let Some(n) = n.strip_suffix("u32") {
                    n.parse().map(Value::U32).map_err(TokenError::from)
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
            _ if value == "true" => Ok(true.into()),
            _ if value == "false" => Ok(false.into()),
            _ => return Err(ParseLiteralError::NotALiteral),
        }
        .map(Literal)
        .map_err(ParseLiteralError::from)
    }
}

pub fn parse_float(n: &str) -> Result<Value, ParseFloatError> {
    #[allow(clippy::option_if_let_else)]
    if let Some(f) = n.strip_suffix('f') {
        f.parse().map(Value::F32)
    } else {
        n.trim_end_matches('d').parse().map(Value::F64)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
