use crate::token::{Token, TokenType};
use std::iter;
use std::str::Chars;
use std::fmt;
use std::error;

#[derive(Debug)]
pub enum ScanError {
    UnexpectedChar(char, i32)
}
impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ScanError::UnexpectedChar(c, line) => {
                write!(f, "Unexpected character {} at line {}", c, line)
            }
        }
    }
}
impl error::Error for ScanError {
    fn description(&self) -> &str {
        match *self {
            ScanError::UnexpectedChar(_, _) =>
                "Unexpected character"
        }
    }
}

pub struct Scanner<'a> {
    source: iter::Peekable<Chars<'a>>,
    line: i32,
}
impl<'a> Scanner<'a> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.chars().peekable(),
            line: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ScanError> {
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            let c = self.source.next();
            match c {
                Some(c) => {
                    if c.is_whitespace() {
                        continue;
                    }
                    tokens.push(self.scan_token(c)?);
                    ();
                }
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

    fn scan_token(&self, c: char) -> Result<Token, ScanError> {
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
            c => Err(ScanError::UnexpectedChar(c, self.line)),
        }
    }
}
