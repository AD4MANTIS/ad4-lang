use strum::{Display, EnumIter, IntoStaticStr};

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
