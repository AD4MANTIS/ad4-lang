use std::fmt::{Display, Formatter};

use crate::{Expression, Statement};

pub const INDENT: &str = "    ";

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub result_expr: Option<Box<Expression>>,
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match (self.statements.as_slice(), &self.result_expr) {
            ([], None) => write!(f, "{{ }}"),
            ([], Some(expr)) => write!(f, "{{ {expr} }}"),
            (_, _) => {
                writeln!(f, "{{")?;

                for statement in &self.statements {
                    writeln!(f, "{INDENT}{statement};")?;
                }

                if let Some(expr) = &self.result_expr {
                    write!(f, "{INDENT}{expr}",)?;
                    writeln!(f)?;
                }

                write!(f, "}}")
            }
        }
    }
}
