use crate::Expression;

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(String, Expression),
    Assign(String, Expression),
}
