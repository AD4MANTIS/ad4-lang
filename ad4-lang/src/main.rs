use prelude::*;
use std::{
    collections::HashMap,
    env::args,
    fs::read_to_string,
    io::{Write, stdin, stdout},
    path::PathBuf,
    str::FromStr,
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

    let code_str = args()
        .skip(1) // skip the executable name
        .filter_map(|arg| PathBuf::from_str(&arg).ok())
        .find(|path| path.is_file())
        .map(|path| read_to_string(path).unwrap());

    if let Some(code_str) = code_str {
        execute_code(&format!("{{{code_str}}}"), &mut variables, debug);
        return Ok(());
    }

    let mut input = String::new();
    loop {
        input.clear();

        read_from_console(&mut input);

        if input.trim() == "exit" {
            break;
        }

        execute_code(&input, &mut variables, debug);
    }

    Ok(())
}

fn execute_code(input: &str, variables: &mut HashMap<Variable, Value>, debug: bool) {
    macro_rules! print_err {
        { $expr:expr } => {
            match $expr {
                Ok(x) => x,
                Err(e) => {
                    print_cond(e, debug);
                    return;
                }
            }
        };
    }

    let mut lexer = print_err! { Lexer::build(input) };
    let expr = print_err! { Statement::parse(&mut lexer) };
    let result = print_err! { expr.execute(variables) };

    print_cond(result, debug);
}

fn read_from_console(input: &mut String) {
    loop {
        print!(">> ");
        stdout().flush().unwrap();
        stdin().read_line(input).unwrap();

        if are_brackets_closed(&*input, Operator::OpeningBracket)
            && are_brackets_closed(&*input, Operator::OpeningCurlyBrace)
        {
            break;
        }
    }
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
