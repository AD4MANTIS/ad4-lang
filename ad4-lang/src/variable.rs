use std::{convert::Infallible, str::FromStr};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub name: String,
}

impl Variable {
    pub const fn new(name: String) -> Self {
        Self { name }
    }
}

impl FromStr for Variable {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            name: s.to_string(),
        })
    }
}
