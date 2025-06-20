use std::fmt::Display;

use crate::{Block, Expression, keyword};

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Box<Expression>,
    pub block: Block,
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            keyword::Expression::While,
            self.condition,
            self.block
        )
    }
}
