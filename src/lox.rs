use std::error::Error;
use std::fs;
use std::io::{self, Write};

use crate::interpreter::{Interpreter, RuntimeError};
use crate::parser::{ParseError, Parser};
use crate::scanner::Scanner;
use crate::token::Token;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

pub fn run_file(path: &str) -> Result<()> {
    let contents = fs::read_to_string(path)?;
    run(&contents)
}

pub fn run_prompt() -> Result<()> {
    let mut interpreter = Interpreter::new();
    loop {
        print!(">> ");
        io::stdout().flush()?;

        let mut line = String::new();
        io::stdin().read_line(&mut line)?;

        if let Err(err) = run_with_interpreter(&mut interpreter, &line) {
            eprintln!("Error: {}", err);
        }

        continue;
    }
}

fn run(source: &str) -> Result<()> {
    let mut interpreter = Interpreter::new();
    run_with_interpreter(&mut interpreter, source)?;

    Ok(())
}

fn run_with_interpreter(interpreter: &mut Interpreter, source: &str) -> Result<()> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(tokens);
    let statements = match parser.parse() {
        Ok(stmts) => stmts,
        Err(err) => {
            let ParseError::UnexpectedToken(ref token, ref _msg) = err;
            show_error_location(source, &token);
            return Err(Box::new(err));
        }
    };

    match interpreter.execute(&statements) {
        Ok(_) => {}
        Err(err) => {
            if let RuntimeError::RuntimeError(ref token, ref _msg) = err {
                show_error_location(source, &token);
            }
            return Err(Box::new(err));
        }
    };
    Ok(())
}

fn show_error_location(source: &str, token: &Token) {
    for (index, line) in source.lines().enumerate() {
        if ((index + 1) as i32) == token.line {
            println!("{:02}: {}", index + 1, line);
            println!("{}{}", " ".repeat((token.column + 3) as usize), '^');
        }
    }
}
