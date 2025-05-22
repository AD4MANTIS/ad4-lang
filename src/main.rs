use color_eyre::eyre::eyre;
pub use expression::*;
pub use lexer::*;
pub use literal::*;
pub use statement::*;
use std::{
    collections::HashMap,
    env::args,
    io::{Write, stdin, stdout},
};
pub use value::*;

mod expression;
mod lexer;
mod literal;
mod statement;
mod value;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let debug = args().any(|args| matches!(args.as_str(), "--debug" | "-d"));
    color_eyre::install()?;

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
        let result = expr.eval(&variables).map_err(|e| eyre!(e));

        if debug {
            match result {
                Ok(result) => println!("{result:?}"),
                Err(e) => println!("{e:?}"),
            }
        } else {
            match result {
                Ok(result) => println!("{result}"),
                Err(e) => println!("{e}"),
            }
        }
    }

    Ok(())
}
