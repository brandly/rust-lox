use std::env;
use std::process;

mod interpreter;
mod lox;
mod parser;
mod scanner;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.as_slice() {
        [_] => {
            if let Err(e) = lox::run_prompt() {
                eprintln!("Application error: {}", e);
                process::exit(1);
            }
        }
        [_, filename] => {
            if let Err(e) = lox::run_file(filename) {
                eprintln!("Application error: {}", e);
                process::exit(1);
            }
        }
        _ => {
            eprintln!("Usage: lox [script]");
            process::exit(1);
        }
    }
}
