use std::env;
use std::error::Error;

use std::fs;
use std::io::{self, Write};

use std::process;

mod scanner;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        eprintln!("Usage: lox [script]");
        process::exit(1);
    } else if args.len() == 2 {
        if let Err(e) = run_file(&args[1]) {
            eprintln!("Application error: {}", e);
            process::exit(1);
        }
    } else {
        if let Err(e) = run_prompt() {
            eprintln!("Application error: {}", e);
            process::exit(1);
        }
    }
}

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(path)?;
    run(&contents)
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    loop {
        print!(">> ");
        io::stdout().flush()?;

        let mut line = String::new();
        io::stdin().read_line(&mut line)?;

        run(&line)?;
    }
}

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    let mut scanner = scanner::Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        println!("TOKEN: {}", token);
    }
    Ok(())
}
