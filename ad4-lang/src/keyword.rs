use strum::{Display, EnumString};

#[derive(Debug, Clone, EnumString, Display, PartialEq, Eq)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    Struct,
    Enum,
    Type,
    Function,
    Trait,
    Impl,
    Use,

    Let,
    If,
    Else,
    While,
    For,
    Match,

    Void,

    Return,
    Continue,
    Break,
    Yield,
}
