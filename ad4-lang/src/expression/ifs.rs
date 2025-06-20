use std::fmt::{Display, Formatter};

use crate::{Block, Expression};

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub block: Block,
    pub elses: Vec<SimpleIf>,
    pub r#else: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct SimpleIf {
    pub condition: Box<Expression>,
    pub block: Block,
}

impl Display for If {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.block)?;

        for else_if in &self.elses {
            write!(f, "\nelse {else_if}")?;
        }

        if let Some(r#else) = &self.r#else {
            write!(f, "\nelse {else}")?;
        }

        Ok(())
    }
}

impl Display for SimpleIf {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.block)
    }
}
