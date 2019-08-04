use std::error::Error;
use std::fs;
use std::io::{self, Write};

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::scanner::Scanner;

pub fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;
    run(&contents)
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
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

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let mut interpreter = Interpreter::new();
    run_with_interpreter(&mut interpreter, source)?;

    Ok(())
}

fn run_with_interpreter(interpreter: &mut Interpreter, source: &str) -> Result<(), Box<dyn Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(tokens);
    let statements = parser.parse()?;

    interpreter.execute(&statements)?;
    Ok(())
}
