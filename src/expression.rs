use std::{collections::HashMap, fmt::Display};

use crate::{Lexer, Literal, Operator, Token, Value};

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    Literal(Literal),
    Operation(Operator, [Box<Expression>; 2]),
}

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("{0}")]
    Static(&'static str),
}

impl From<&'static str> for EvalError {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
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
            Token::Op(operator @ (Operator::Add | Operator::Sub)) => Self::Operation(
                operator,
                [
                    Box::new(Self::Literal(Literal(Value::I64(0)))),
                    Box::new(Self::parse(lexer, operator.infix_binding_power().1)),
                ],
            ),
            Token::Op(operator) => panic!("expected Expression, found Operator '{operator}'"),
        };

        loop {
            let operator = match lexer.peek() {
                Some(Token::Op(operator)) => *operator,
                Some(token) => {
                    panic!("expected Operator, found '{token}'")
                }
                None => {
                    break;
                }
            };

            let infix_binding_power = operator.infix_binding_power();
            if infix_binding_power.0 < binding_power_lhs {
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

    pub fn eval(&self, variables: &HashMap<String, Value>) -> Result<Value, EvalError> {
        match self {
            Self::Variable(name) => variables
                .get(name)
                .cloned()
                .ok_or(EvalError::Static("unknown variable")),
            Self::Literal(literal) => Ok(literal.0.clone()),
            Self::Operation(operator, expressions) => {
                let lhs = expressions[0].eval(variables)?;
                let rhs = expressions[1].eval(variables)?;

                match operator {
                    Operator::Add => (lhs + &rhs).map_err(EvalError::from),
                    Operator::Sub => (lhs - &rhs).map_err(EvalError::from),
                    Operator::Mul => (lhs * &rhs).map_err(EvalError::from),
                    Operator::Div => (lhs / &rhs).map_err(EvalError::from),
                    Operator::Assign => todo!(),
                    Operator::OpeningBracket => todo!(),
                    Operator::ClosingBracket => todo!(),
                    Operator::Dot => todo!(),
                }
            }
        }
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

#[cfg(test)]
mod test {
    use super::*;

    fn expr(input: &str, expected: &str) {
        let s = Expression::from_str(input);
        assert_eq!(s.to_string(), expected, "Input: {input}");
    }

    macro_rules! expr_cases {
        ($($name:ident: $input:expr => $expected:expr),* $(,)?) => {
            $(
                #[test]
                fn $name() {
                    expr($input, $expected);
                }
            )*
        };
    }

    expr_cases! {
        literal_number: "1" => "1",
        literal_char: "'c'" => "c",
        literal_char_whitespace: "' '" => " ",
        literal_string: "\"Hi\"" => "Hi",
        literal_string_with_spaces: "\"Hello, World!\"" => "Hello, World!",
        negativ_literal_number: "-1" => "(- 0 1)",
        precedence_mul_before_add: "1+2 * 3" => "(+ 1 (* 2 3))",
        left_associative_mul: "a * 2 * b" => "(* (* a 2) b)",
        mixed_precedence_and_div: "a + b * 2 * c + a/4" => "(+ (+ a (* (* b 2) c)) (/ a 4))",
        complex_chain: "2 + b * 5 - 3 / 5 + 5 - 3" => "(- (+ (- (+ 2 (* b 5)) (/ 3 5)) 5) 3)",
        parens_affect_precedence: "(2 + b) * 5" => "(* (+ 2 b) 5)",
        nested_parens: "(( ( a )) )" => "a",
        parens_with_mul_and_add: "a + b * 2 * ( c + a ) / 4" => "(+ a (/ (* (* b 2) (+ c a)) 4))",
        variable_x: "x" => "x",
        add_two_vars: "x + y" => "(+ x y)",
        mul_then_add: "x * y + z" => "(+ (* x y) z)",
        dot_operator: "a.b" => "(. a b)",
        chained_dot: "a.b.c.d" => "(. (. (. a b) c) d)",
        left_associative_add: "1 + 2 + 3" => "(+ (+ 1 2) 3)",
        left_associative_mul_numbers: "1 * 2 * 3" => "(* (* 1 2) 3)",
        parens_around_number: "((1))" => "1",
        add_with_parens_right: "a + (b + c)" => "(+ a (+ b c))",
        mul_with_parens_both: "(a + b) * (c + d)" => "(* (+ a b) (+ c d))",
        div_then_mul: "a / b * c" => "(* (/ a b) c)",
        long_add_chain: "a + b + c + d" => "(+ (+ (+ a b) c) d)",
    }
}
