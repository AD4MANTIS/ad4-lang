use std::fmt::Display;

use crate::{Lexer, Literal, Operator, Token};

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    Literal(Literal),
    Operation(Operator, [Box<Expression>; 2]),
}

impl Expression {
    pub fn parse(lexer: &mut Lexer, binding_power_lhs: u32) -> Self {
        let mut expr = match lexer.next().expect("expected Token") {
            Token::Variable(v) => Self::Variable(v),
            Token::Literal(literal) => Self::Literal(literal),
            Token::Keyword(keyword) => panic!("expected Expression, found Keyword '{keyword}'"),
            Token::Op(bracket @ Operator::OpeningBracket) => {
                let expr = Self::parse(lexer, bracket.infix_binding_power().1);
                assert!(matches!(
                    lexer.next(),
                    Some(Token::Op(Operator::ClosingBracket))
                ));
                expr
            }
            Token::Op(operator) => panic!("expected Expression, found Operator '{operator}'"),
        };

        loop {
            let operator = match lexer.peek() {
                Some(Token::Op(operator)) => operator.clone(),
                Some(token) => {
                    panic!("expected Operator, found '{token}'")
                }
                None => {
                    break;
                }
            };

            let infix_binding_power = operator.infix_binding_power();
            if infix_binding_power.0 <= binding_power_lhs {
                break;
            }
            lexer.next();

            let rhs = Self::parse(lexer, infix_binding_power.1);

            expr = Self::Operation(operator, [Box::new(expr), Box::new(rhs)]);
        }

        expr
    }

    #[must_use]
    pub fn from_str(s: &str) -> Self {
        let mut lexer = Lexer::build(s);
        Self::parse(&mut lexer, 0)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => literal.fmt(f),
            Self::Variable(var) => var.fmt(f),
            Self::Operation(operator, expressions) => f.write_fmt(format_args!(
                "({operator} {} {})",
                expressions[0], expressions[1]
            )),
        }
    }
}
