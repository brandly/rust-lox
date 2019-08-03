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
    loop {
        print!(">> ");
        io::stdout().flush()?;

        let mut line = String::new();
        io::stdin().read_line(&mut line)?;

        run(&line)?;
    }
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(tokens);
    let statements = parser.parse()?;

    let mut interpreter = Interpreter::new();
    interpreter.execute(&statements);

    Ok(())
}
