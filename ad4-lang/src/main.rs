use color_eyre::eyre::eyre;
use prelude::*;
use std::{
    collections::HashMap,
    env::args,
    io::{Write, stdin, stdout},
};

mod expression;
mod lexer;
mod literal;
mod operator;
mod prelude;
mod statement;
mod value;
mod variable;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let debug = args().any(|args| matches!(args.as_str(), "--debug" | "-d"));
    color_eyre::install()?;

    let mut variables = HashMap::<Variable, Value>::new();

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
        let result = expr.map(|expr| expr.eval(&mut variables).map_err(|e| eyre!(e)));

        if debug {
            match result {
                Ok(result) => println!("{result:?}"),
                Err(e) => println!("{e:?}"),
            }
        } else {
            match result {
                Ok(Ok(result)) => println!("{result}"),
                Ok(Err(e)) => println!("{e}"),
                Err(e) => println!("{e}"),
            }
        }
    }

    Ok(())
}
