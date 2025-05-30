use std::{fmt::Display, str::FromStr};

use strum::{Display, EnumString};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Declaration(Declaration),
    Expression(Expression),
    ControlFlow(ControlFlow),
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declaration(declaration) => write!(f, "{declaration}"),
            Self::Expression(expression) => write!(f, "{expression}"),
            Self::ControlFlow(control_flow) => write!(f, "{control_flow}"),
        }
    }
}

impl FromStr for Keyword {
    type Err = strum::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Declaration::from_str(s)
            .map(Self::Declaration)
            .or_else(|_| Expression::from_str(s).map(Self::Expression))
            .or_else(|_| ControlFlow::from_str(s).map(Self::ControlFlow))
    }
}

#[allow(dead_code)]
impl Keyword {
    pub const fn let_() -> Self {
        Self::Declaration(Declaration::Let)
    }

    pub const fn struct_() -> Self {
        Self::Declaration(Declaration::Struct)
    }

    pub const fn enum_() -> Self {
        Self::Declaration(Declaration::Enum)
    }

    pub const fn type_() -> Self {
        Self::Declaration(Declaration::Type)
    }

    pub const fn fun_() -> Self {
        Self::Declaration(Declaration::Function)
    }

    pub const fn trait_() -> Self {
        Self::Declaration(Declaration::Trait)
    }

    pub const fn impl_() -> Self {
        Self::Declaration(Declaration::Impl)
    }

    pub const fn use_() -> Self {
        Self::Declaration(Declaration::Use)
    }

    pub const fn if_() -> Self {
        Self::Expression(Expression::If)
    }

    pub const fn else_() -> Self {
        Self::Expression(Expression::Else)
    }

    pub const fn while_() -> Self {
        Self::Expression(Expression::While)
    }

    pub const fn for_() -> Self {
        Self::Expression(Expression::For)
    }

    pub const fn match_() -> Self {
        Self::Expression(Expression::Match)
    }

    pub const fn void_() -> Self {
        Self::Expression(Expression::Void)
    }

    pub const fn return_() -> Self {
        Self::ControlFlow(ControlFlow::Return)
    }

    pub const fn continue_() -> Self {
        Self::ControlFlow(ControlFlow::Continue)
    }

    pub const fn break_() -> Self {
        Self::ControlFlow(ControlFlow::Break)
    }

    pub const fn yield_() -> Self {
        Self::ControlFlow(ControlFlow::Yield)
    }
}

#[derive(Debug, Clone, Copy, EnumString, Display, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum Declaration {
    Let,
    Struct,
    Enum,
    Type,
    Function,
    Trait,
    Impl,
    Use,
}

#[derive(Debug, Clone, Copy, EnumString, Display, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum Expression {
    If,
    Else,
    While,
    For,
    Match,

    Void,
}

#[derive(Debug, Clone, Copy, EnumString, Display, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum ControlFlow {
    Return,
    Continue,
    Break,
    Yield,
}
