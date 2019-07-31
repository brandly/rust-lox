use std::env;
use std::error::Error;
use std::fs;
use std::io::{self, Write};
use std::iter;
use std::process;
use std::str::Chars;
use std::fmt;

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
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        println!("TOKEN: {}", token);
    }
    Ok(())
}

struct Scanner<'a> {
    source: iter::Peekable<Chars<'a>>,
    line: i32,
}
impl<'a> Scanner<'a> {
    fn new(source: &str) -> Scanner {
        Scanner {
            source: source.chars().peekable(),
            line: 0,
        }
    }

    fn scan_tokens(&mut self) -> Result<Vec<Token>, Box<dyn Error>> {
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            let c = self.source.next();
            match c {
                Some(c) => {
                    if c.is_whitespace() {
                        continue;;
                    }
                    tokens.push(self.scan_token(c)?);
                    ();
                },
                None => {
                    tokens.push(Token {
                        line: self.line,
                        lexeme: String::from(""),
                        type_: TokenType::EOF,
                    });
                    break;
                }
            }
        }

        Ok(tokens)
    }

    fn scan_token(&mut self, c: char) -> Result<Token, Box<dyn Error>> {
        match c {
            '(' => Ok(Token::basic(TokenType::LeftParen, self.line)),
            ')' => Ok(Token::basic(TokenType::RightParen, self.line)),
            '{' => Ok(Token::basic(TokenType::LeftBrace, self.line)),
            '}' => Ok(Token::basic(TokenType::RightBrace, self.line)),
            ',' => Ok(Token::basic(TokenType::Comma, self.line)),
            '.' => Ok(Token::basic(TokenType::Dot, self.line)),
            '-' => Ok(Token::basic(TokenType::Minus, self.line)),
            '+' => Ok(Token::basic(TokenType::Plus, self.line)),
            ';' => Ok(Token::basic(TokenType::Semicolon, self.line)),
            '*' => Ok(Token::basic(TokenType::Star, self.line)),
            _ => panic!("don't know that char: {}", c),
        }
    }
}

struct Token {
    type_: TokenType,
    lexeme: String,
    // literal ?
    line: i32, // TODO: toString, need to impl trait
               // https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
}
impl Token {
    fn basic(type_: TokenType, line: i32) -> Token {
        Token {
            type_,
            lexeme: String::from(""),
            line,
        }
    }
}
impl fmt::Display for Token {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {}", self.type_, self.lexeme)
    }
}

#[derive(Debug)]
enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}
