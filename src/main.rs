use std::{
    collections::HashMap,
    io::{Write, stdin, stdout},
};

#[derive(Debug, Clone)]
enum Token {
    Keyword(Keyword),
    Op(Operator),
    Literal(Literal),
    Variable(String),
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
}

impl TryFrom<&str> for Operator {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "+" => Self::Add,
            "-" => Self::Sub,
            "/" => Self::Div,
            "*" => Self::Mul,
            "=" => Self::Assign,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone)]
enum Keyword {
    Let,
}

impl TryFrom<&str> for Keyword {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "let" => Self::Let,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone)]
enum Literal {
    String(String),
    Number(String),
    Char(String),
}

impl TryFrom<&str> for Literal {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value
            .chars()
            .next()
            .and_then(|val| {
                Some(match val {
                    '0'..='9' => Self::Number(value.to_string()),
                    '\'' => Self::Char(value.to_string()),
                    '\"' => Self::String(value.to_string()),
                    _ => return None,
                })
            })
            .ok_or(())
    }
}

#[derive(Debug, Clone)]
enum Expression {
    Atomic(Token),
    Expr(String, [Box<Expression>; 2]),
}

#[derive(Debug, Clone)]
enum Statement {}

#[derive(Debug, Clone)]
enum Variable {
    String(String),
    I64(i64),
    U64(u64),
    Float(f32),
}

fn main() {
    println!("Hello, world!");
    let mut variables = HashMap::<String, Variable>::new();

    let mut input = String::new();
    loop {
        input.clear();
        print!(">> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut input).unwrap();

        if input.trim() == "exit" {
            break;
        }

        let tokens = tokenize(&input);
        println!("{tokens:?}");
    }
}

fn tokenize(input: &str) -> Vec<Token> {
    input
        .split_whitespace()
        .map(|token| {
            if let Ok(keyword) = Keyword::try_from(token) {
                return Token::Keyword(keyword);
            }

            if let Ok(operator) = Operator::try_from(token) {
                return Token::Op(operator);
            }

            if let Ok(literal) = Literal::try_from(token) {
                return Token::Literal(literal);
            }

            Token::Variable(token.to_string())
        })
        .collect()
}
