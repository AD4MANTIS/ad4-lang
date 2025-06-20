use prelude::*;
use std::{
    collections::HashMap,
    env::args,
    io::{Write, stdin, stdout},
};

mod block;
mod expression;
mod keyword;
mod lexer;
mod literal;
mod operator;
mod prelude;
mod statement;
mod token;
mod value;
mod variable;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let debug = args().any(|args| matches!(args.as_str(), "--debug" | "-d"));
    color_eyre::install()?;

    let mut variables = HashMap::<Variable, Value>::new();

    let mut input = String::new();
    loop {
        input.clear();

        loop {
            print!(">> ");
            stdout().flush().unwrap();
            stdin().read_line(&mut input).unwrap();

            if are_brackets_closed(&input, Operator::OpeningBracket)
                && are_brackets_closed(&input, Operator::OpeningCurlyBrace)
            {
                break;
            }
        }

        if input.trim() == "exit" {
            break;
        }

        macro_rules! print_err {
            { $expr:expr } => {
                match $expr {
                    Ok(x) => x,
                    Err(e) => {
                        print_cond(e, debug);
                        continue;
                    }
                }
            };
        }

        let mut lexer = print_err! { Lexer::build(&input) };
        let expr = print_err! { Statement::parse(&mut lexer) };
        let result = print_err! { expr.execute(&mut variables) };

        print_cond(result, debug);
    }

    Ok(())
}

fn print_cond<T: std::fmt::Display + std::fmt::Debug>(val: T, debug: bool) {
    if debug {
        println!("{val:?}");
    } else {
        println!("{val}");
    }
}

fn are_brackets_closed(input: &str, opening: Operator) -> bool {
    input.matches(opening.as_str()).count()
        == input
            .matches(
                opening
                    .get_closing_bracket()
                    .expect("should be a bracket")
                    .as_str(),
            )
            .count()
}
