use std::env;
use std::process;

mod lox;
mod scanner;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => {
            if let Err(e) = lox::run_prompt() {
                eprintln!("Application error: {}", e);
                process::exit(1);
            }
        }
        2 => {
            if let Err(e) = lox::run_file(&args[1]) {
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
