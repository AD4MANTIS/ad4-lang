pub use expression::*;
pub use lexer::*;
use std::{
    collections::HashMap,
    io::{Write, stdin, stdout},
};

mod expression;
mod lexer;
#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
enum Statement {
    Declaration(Variable, Expression),
    Assign(Variable, Expression),
}

#[derive(Debug, Clone)]
enum Variable {
    String(String),
    I64(i64),
    U64(u64),
    Float(f32),
}

fn main() {
    println!("Hello, world!");
    let variables = HashMap::<String, Variable>::new();

    let mut input = String::new();
    loop {
        input.clear();
        print!(">> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut input).unwrap();

        if input.trim() == "exit" {
            break;
        }

        let mut lexer = Lexer::build(&input);
        let expr = Expression::parse(&mut lexer, 0);
        println!("{expr}");
    }
}
