use std::{fmt::Display, str::FromStr};

use crate::{TokenError, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Literal(pub Value);

#[derive(thiserror::Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    Token(#[from] TokenError),
}

fn parse<T: std::str::FromStr, TErr: From<T::Err>>(
    value: &str,
    map: impl FnOnce(T) -> Value,
) -> Result<Value, TErr> {
    value.parse::<T>().map(map).map_err(TErr::from)
}

impl FromStr for Literal {
    type Err = Option<ParseError>;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(Self(match value.as_bytes() {
            [b'0'..=b'9', ..] => {
                let n = value;

                if n.contains('.') || n.ends_with(['f', 'd']) {
                    parse_float(n)
                } else if let Some(n) = n.strip_suffix("i32") {
                    parse(n, Value::I32)
                } else if let Some(n) = n.strip_suffix("u32") {
                    parse(n, Value::U32)
                } else if let Some(n) = n.strip_suffix("i64") {
                    parse(n, Value::I64)
                } else if let Some(n) = n.strip_suffix("u64") {
                    parse(n, Value::U64)
                } else if let Some(n) = n.strip_suffix('u') {
                    parse(n, Value::U64)
                } else {
                    parse(n.trim_end_matches('i'), Value::I64)
                }
                .map_err(ParseError::from)
                .map_err(Some)?
            }
            [b'\'', .., b'\''] => {
                let char = &value[1..value.len() - 1];

                if char.len() != 1 {
                    Err(Some(TokenError::InvalidCharLength.into()))?;
                }

                Value::Char(char.chars().next().unwrap())
            }
            [b'\'', ..] => Err(Some(TokenError::InvalidCharLength.into()))?,
            [b'\"', .., b'\"'] => Value::String(value[1..value.len() - 1].to_string()),
            b"true" => true.into(),
            b"false" => false.into(),
            b"void" => Value::Void,
            _ => return Err(None),
        }))
    }
}

pub fn parse_float(n: &str) -> Result<Value, TokenError> {
    #[allow(clippy::option_if_let_else)]
    if let Some(f) = n.strip_suffix('f') {
        parse(f, Value::F32)
    } else {
        parse(n.trim_end_matches('d'), Value::F64)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
