use std::env;
use std::process;

mod lox;
mod scanner;
mod token;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        eprintln!("Usage: lox [script]");
        process::exit(1);
    } else if args.len() == 2 {
        if let Err(e) = lox::run_file(&args[1]) {
            eprintln!("Application error: {}", e);
            process::exit(1);
        }
    } else {
        if let Err(e) = lox::run_prompt() {
            eprintln!("Application error: {}", e);
            process::exit(1);
        }
    }
}
