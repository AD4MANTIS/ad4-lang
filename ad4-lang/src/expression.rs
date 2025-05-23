use std::{collections::HashMap, convert::Infallible, fmt::Display, str::FromStr};

use crate::{Lexer, Literal, Operator, Token, Value, Variable};

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(Variable),
    Literal(Literal),
    Operation {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

#[derive(thiserror::Error, Debug)]
pub enum EvalError {
    #[error("{0}")]
    Static(&'static str),
    #[error("{0}")]
    String(String),
}

impl From<String> for EvalError {
    fn from(value: String) -> Self {
        Self::String(value)
    }
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
        let mut lhs = match lexer
            .next()
            .expect("expected Token, but there were no Tokens left")
        {
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
            Token::Op(operator @ (Operator::Add | Operator::Sub)) => Self::Operation {
                operator,
                lhs: Box::new(Self::Literal(Literal(Value::I64(0)))),
                rhs: Box::new(Self::parse(lexer, operator.infix_binding_power().1)),
            },
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

            lhs = Self::Operation {
                operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        lhs
    }

    pub fn eval(&self, variables: &mut HashMap<Variable, Value>) -> Result<Value, EvalError> {
        match self {
            Self::Variable(variable) => variables
                .get(variable)
                .cloned()
                .ok_or_else(|| EvalError::String(format!("unknown variable '{}'", variable.name))),
            Self::Literal(literal) => Ok(literal.0.clone()),
            Self::Operation { operator, lhs, rhs } => {
                let rhs = rhs.eval(variables)?;

                match operator {
                    Operator::Add => (lhs.eval(variables)? + &rhs).map_err(EvalError::from),
                    Operator::Sub => (lhs.eval(variables)? - &rhs).map_err(EvalError::from),
                    Operator::Mul => (lhs.eval(variables)? * &rhs).map_err(EvalError::from),
                    Operator::Div => (lhs.eval(variables)? / &rhs).map_err(EvalError::from),
                    Operator::Eq => Ok((lhs.eval(variables)? == rhs).into()),
                    Operator::Neq => Ok((lhs.eval(variables)? != rhs).into()),
                    Operator::Assign => match &**lhs {
                        Self::Variable(var) => {
                            variables.insert(var.clone(), rhs.clone());
                            Ok(rhs)
                        }
                        _ => Err(EvalError::String(format!("Expected Variable, found {lhs}"))),
                    },
                    Operator::Dot => todo!(),
                    Operator::OpeningBracket | Operator::ClosingBracket => Err(EvalError::String(
                        format!("Expected Expression, found Operator '{operator}'"),
                    )),
                }
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => literal.fmt(f),
            Self::Variable(var) => write!(f, "{}", var.name),
            Self::Operation { operator, lhs, rhs } => write!(f, "({operator} {lhs} {rhs})"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn expr(input: &str, expected: &str, result: Option<Value>) {
        let s = Expression::from_str(input).unwrap();
        assert_eq!(s.to_string(), expected, "Input: {input}");

        if let Some(result) = result {
            if let Ok(eval_result) = s.eval(
                &mut [("a", 1i64.into()), ("b", 2i64.into()), ("c", 3i64.into())]
                    .into_iter()
                    .map(|x| (Variable::new(x.0.to_string()), x.1))
                    .collect::<HashMap<_, _>>(),
            ) {
                assert_eq!(eval_result, result);
            }
        }
    }

    macro_rules! expr_cases {
        ($mod_name:ident $($name:ident: $input:expr => $expected:expr $(=> $result:expr )?),* $(,)?) => {
            mod $mod_name {
                use super::*;

                $(
                    #[test]
                    fn $name() {
                        #[allow(unused_variables)]
                        let result: Option<Value> = None;

                        $(
                            let result = Some($result.into());
                        )?

                        expr($input, $expected, result);
                    }
                )*
            }
        };
    }

    expr_cases! { literals
        number: "1" => "1" => 1,
        char: "'c'" => "c" => 'c',
        char_whitespace: "' '" => " " => ' ',
        string: "\"Hi\"" => "Hi" => "Hi".to_string(),
        string_with_spaces: "\"Hello, World!\"" => "Hello, World!" => "Hello, World!".to_string(),
        negativ_number: "-1" => "(- 0 1)" => -1,
        signed_number: "12i" => "12" => 12,
        unsigned_number: "123u" => "123" => 123,
        float: "1.23" => "1.23" => 1.23,
        explicit_float: "32.1f" => "32.1" => 32.1f32,
        explicit_double: "4.99d" => "4.99" => 4.99,
        boolean_true: "true" => "true" => true,
        boolean_false: "false" => "false" => false,
    }

    expr_cases! { math
        precedence_mul_before_add: "1+2 * 3" => "(+ 1 (* 2 3))" => 7,
        left_associative_mul: "a * 2 * b" => "(* (* a 2) b)" => 4,
        mixed_precedence_and_div: "a + b * 2 * c + a/4" => "(+ (+ a (* (* b 2) c)) (/ a 4))" => 13,
        complex_chain: "2 + b * 5 - 3 / 5 + 5 - 3" => "(- (+ (- (+ 2 (* b 5)) (/ 3 5)) 5) 3)" => 14,
        parens_affect_precedence: "(2 + b) * 5" => "(* (+ 2 b) 5)" => 20,
        nested_parens: "(( ( a )) )" => "a" => 1,
        parens_with_mul_and_add: "a + b * 2 * ( c + a ) / 4" => "(+ a (/ (* (* b 2) (+ c a)) 4))" => 5,
        add_floats: "1.5 + 4.0" => "(+ 1.5 4)" => 5.5,
        prefix_sub: "-5 + -5" => "(+ (- 0 5) (- 0 5))" => -10,
    }

    expr_cases! { variables
        variable_x: "a" => "a" => 1,
        add_two_vars: "a + b" => "(+ a b)" => 3,
        mul_then_add: "a * b + c" => "(+ (* a b) c)" => 5,
        left_associative_add: "1 + 2 + 3" => "(+ (+ 1 2) 3)" => 6,
        left_associative_mul_numbers: "1 * 2 * 3" => "(* (* 1 2) 3)" => 6,
        parens_around_number: "((1))" => "1" => 1,
        add_with_parens_right: "a + (b + c)" => "(+ a (+ b c))" => 6,
        mul_with_parens_both: "(a + b) * (c + d)" => "(* (+ a b) (+ c d))" => 7,
        div_then_mul_zero: "a / b * c" => "(* (/ a b) c)" => 0,
        div_then_mul: "16 / 4 * 2" => "(* (/ 16 4) 2)" => 8,
        long_add_chain: "a + b + c + d" => "(+ (+ (+ a b) c) d)" => 6,

        assign_variable: "p = 3.1" => "(= p 3.1)" => 3.1,
        assign_from_expr: "x = a == 2" => "(= x (== a 2))" => false,
    }

    expr_cases! { dot
        dot_operator: "a.b" => "(. a b)",
        chained_dot: "a.b.c.d" => "(. (. (. a b) c) d)",
        spaced_dots: "a . b. c" => "(. (. a b) c)",
    }

    expr_cases! { equality
        basic_eq: "1 == 1" => "(== 1 1)" => true,
        boolean_neq: "true != false" => "(!= true false)" => true,
        number_eq: "1 == 3" => "(== 1 3)" => false,
        sub_expr_eq: "(1 + 2) * 3 == 10 - 1" => "(== (* (+ 1 2) 3) (- 10 1))" => true,
    }
}
