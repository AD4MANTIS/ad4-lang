use std::fmt::Display;

use crate::{Lexer, Literal, Operator, Token};

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    Literal(Literal),
    Operation(Operator, [Box<Expression>; 2]),
}

impl Expression {
    pub fn parse(lexer: &mut Lexer) -> Expression {
        let lhs = match lexer.next().expect("expected Token") {
            Token::Variable(v) => Self::Variable(v),
            Token::Literal(literal) => Self::Literal(literal),
            Token::Keyword(keyword) => panic!("expected Expression, found Keyword"),
            Token::Op(operator) => panic!("expected Expression, found Operator"),
        };

        let operator = match lexer.next() {
            Some(Token::Op(operator)) => operator,
            Some(_) => {
                panic!("expected Operator")
            }
            None => {
                return lhs;
            }
        };

        Self::Operation(operator, [Box::new(lhs), Box::new(Self::parse(lexer))])
    }

    pub fn from_str(s: &str) -> Self {
        let mut lexer = Lexer::build(s);
        Self::parse(&mut lexer)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => literal.fmt(f),
            Self::Variable(var) => var.fmt(f),
            Expression::Operation(operator, expressions) => f.write_fmt(format_args!(
                "({operator} {} {})",
                expressions[0], expressions[1]
            )),
        }
    }
}
