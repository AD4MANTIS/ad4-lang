use std::fmt::Display;

use strum::EnumIter;

// Operators witch contain other Operator Strings need to be sorted by the longest first
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Leq,
    Geq,
    Lt,
    Gt,
    Assign,
    OpeningBracket,
    ClosingBracket,
    OpeningCurlyBrace,
    ClosingCurlyBrace,
    OpeningSquareBracket,
    ClosingSquareBracket,
    Dot,
}

impl From<Operator> for &'static str {
    fn from(value: Operator) -> Self {
        type Op = Operator;
        match value {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Eq => "==",
            Op::Neq => "!=",
            Op::Leq => "<=",
            Op::Geq => ">=",
            Op::Lt => "<",
            Op::Gt => ">",
            Op::Assign => "=",
            Op::OpeningBracket => "(",
            Op::ClosingBracket => ")",
            Op::OpeningCurlyBrace => "{",
            Op::ClosingCurlyBrace => "}",
            Op::OpeningSquareBracket => "[",
            Operator::ClosingSquareBracket => "]",
            Op::Dot => ".",
        }
    }
}

impl Operator {
    pub fn as_str(self) -> &'static str {
        self.into()
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str((*self).into())
    }
}

impl Operator {
    #[must_use]
    pub const fn infix_binding_power(self) -> (u32, u32) {
        match self {
            Self::Add | Self::Sub => (10, 11),
            Self::Div | Self::Mul => (12, 13),
            Self::Assign => (2, 1),
            Self::Eq | Self::Neq | Self::Lt | Self::Leq | Self::Gt | Self::Geq => (5, 6),
            Self::OpeningBracket | Self::OpeningCurlyBrace | Self::OpeningSquareBracket => (0, 1),
            Self::ClosingBracket | Self::ClosingCurlyBrace | Self::ClosingSquareBracket => (0, 0),
            Self::Dot => (20, 21),
        }
    }

    pub const fn get_closing_bracket(self) -> Option<Self> {
        Some(match self {
            Self::OpeningBracket => Self::ClosingBracket,
            Self::OpeningCurlyBrace => Self::ClosingCurlyBrace,
            Self::OpeningSquareBracket => Self::ClosingSquareBracket,
            _ => return None,
        })
    }
}
