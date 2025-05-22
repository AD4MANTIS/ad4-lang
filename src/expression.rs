use std::{collections::HashMap, convert::Infallible, fmt::Display, str::FromStr};

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

impl FromStr for Expression {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lexer = Lexer::build(s);
        Ok(Self::parse(&mut lexer, 0))
    }
}

impl Expression {
    pub fn parse(lexer: &mut Lexer, binding_power_lhs: u32) -> Self {
        let mut lhs = match lexer.next().expect("expected Token") {
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

            lhs = Self::Operation(operator, [Box::new(lhs), Box::new(rhs)]);
        }

        lhs
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
                    Operator::Eq => Ok(Value::Bool(lhs == rhs)),
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
        let s = Expression::from_str(input).unwrap();
        assert_eq!(s.to_string(), expected, "Input: {input}");
    }

    macro_rules! expr_cases {
        ($mod_name:ident $($name:ident: $input:expr => $expected:expr),* $(,)?) => {
            mod $mod_name {
                use super::*;

                $(
                    #[test]
                    fn $name() {
                        expr($input, $expected);
                    }
                )*
            }
        };
    }

    expr_cases! { literals
        number: "1" => "1",
        char: "'c'" => "c",
        char_whitespace: "' '" => " ",
        string: "\"Hi\"" => "Hi",
        string_with_spaces: "\"Hello, World!\"" => "Hello, World!",
        negativ_number: "-1" => "(- 0 1)",
        signed_number: "12i" => "12",
        unsigned_number: "123u" => "123",
        float: "1.23" => "1.23",
        explicit_float: "32.1f" => "32.1",
        explicit_double: "4.99d" => "4.99",
        boolean_true: "true" => "true",
        boolean_false: "false" => "false",
    }

    expr_cases! { math
        precedence_mul_before_add: "1+2 * 3" => "(+ 1 (* 2 3))",
        left_associative_mul: "a * 2 * b" => "(* (* a 2) b)",
        mixed_precedence_and_div: "a + b * 2 * c + a/4" => "(+ (+ a (* (* b 2) c)) (/ a 4))",
        complex_chain: "2 + b * 5 - 3 / 5 + 5 - 3" => "(- (+ (- (+ 2 (* b 5)) (/ 3 5)) 5) 3)",
        parens_affect_precedence: "(2 + b) * 5" => "(* (+ 2 b) 5)",
        nested_parens: "(( ( a )) )" => "a",
        parens_with_mul_and_add: "a + b * 2 * ( c + a ) / 4" => "(+ a (/ (* (* b 2) (+ c a)) 4))",
        add_floats: "1.5 + 4.0" => "(+ 1.5 4)",
    }

    expr_cases! { variables
        variable_x: "x" => "x",
        add_two_vars: "x + y" => "(+ x y)",
        mul_then_add: "x * y + z" => "(+ (* x y) z)",
        left_associative_add: "1 + 2 + 3" => "(+ (+ 1 2) 3)",
        left_associative_mul_numbers: "1 * 2 * 3" => "(* (* 1 2) 3)",
        parens_around_number: "((1))" => "1",
        add_with_parens_right: "a + (b + c)" => "(+ a (+ b c))",
        mul_with_parens_both: "(a + b) * (c + d)" => "(* (+ a b) (+ c d))",
        div_then_mul: "a / b * c" => "(* (/ a b) c)",
        long_add_chain: "a + b + c + d" => "(+ (+ (+ a b) c) d)",
    }

    expr_cases! { dot
        dot_operator: "a.b" => "(. a b)",
        chained_dot: "a.b.c.d" => "(. (. (. a b) c) d)",
        spaced_dots: "a . b. c" => "(. (. a b) c)",
    }

    expr_cases! { equality
        number_eq: "1 == 3" => "(== 1 3)",
        sub_expr_eq: "(1 + 2) * 3 == 10 - 1" => "(== (* (+ 1 2) 3) (- 10 1))",
    }
}
