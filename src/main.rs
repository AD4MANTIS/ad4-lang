pub use expression::*;
pub use lexer::*;
use std::{
    collections::HashMap,
    io::{Write, stdin, stdout},
};
pub use value::*;

mod expression;
mod lexer;
mod statement;
mod value;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Hello, world!");
    let variables = HashMap::<String, Value>::new();

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
        println!("{:?}", expr.eval(&variables)?);
    }

    Ok(())
}
