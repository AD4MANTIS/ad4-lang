use std::fmt::Display;

use strum::EnumIter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumIter)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Assign,
    OpeningBracket,
    ClosingBracket,
    OpeningCurlyBrace,
    ClosingCurlyBrace,
    Dot,
}

impl From<Operator> for &'static str {
    fn from(value: Operator) -> Self {
        match value {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Eq => "==",
            Operator::Neq => "!=",
            Operator::Assign => "=",
            Operator::OpeningBracket => "(",
            Operator::ClosingBracket => ")",
            Operator::OpeningCurlyBrace => "{",
            Operator::ClosingCurlyBrace => "}",
            Operator::Dot => ".",
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
            Self::Eq | Self::Neq => (8, 9),
            Self::OpeningBracket | Self::OpeningCurlyBrace => (0, 1),
            Self::ClosingBracket | Self::ClosingCurlyBrace => (0, 0),
            Self::Dot => (20, 21),
        }
    }
}
