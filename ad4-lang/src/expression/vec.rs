use std::fmt::Display;

use crate::Expression;

#[derive(Debug, Clone)]
pub struct VecExpression {
    pub items: Vec<Expression>,
}

impl Display for VecExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.items[..] {
            [] => f.write_str("[]"),
            [single_item] => write!(f, "[{single_item}]"),
            _ => {
                writeln!(f, "[")?;

                for item in &self.items {
                    writeln!(f, "{item},")?;
                }

                writeln!(f, "]")
            }
        }
    }
}
