use crate::token::{Token, TokenType as TT};
use std::error;
use std::fmt;
use std::iter;
use std::str::Chars;

#[derive(Debug)]
pub enum ScanError {
    UnexpectedChar(char, i32),
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
            ScanError::UnexpectedChar(_, _) => "Unexpected character",
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
                        if c == '\n' {
                            self.line += 1;
                        }
                        continue;
                    }
                    tokens.push(self.scan_token(c)?);
                    ();
                }
                None => {
                    tokens.push(Token {
                        line: self.line,
                        lexeme: String::from(""),
                        type_: TT::EOF,
                    });
                    break;
                }
            }
        }

        Ok(tokens)
    }

    fn scan_token(&mut self, c: char) -> Result<Token, ScanError> {
        let line = self.line;
        let basic = |type_| Ok(Token::basic(type_, line));
        match c {
            '(' => basic(TT::LeftParen),
            ')' => basic(TT::RightParen),
            '{' => basic(TT::LeftBrace),
            '}' => basic(TT::RightBrace),
            ',' => basic(TT::Comma),
            '.' => basic(TT::Dot),
            '-' => basic(TT::Minus),
            '+' => basic(TT::Plus),
            ';' => basic(TT::Semicolon),
            '*' => basic(TT::Star),
            '!' => basic(if self.match_('=') {
                TT::BangEqual
            } else {
                TT::Bang
            }),
            '=' => basic(if self.match_('=') {
                TT::EqualEqual
            } else {
                TT::Equal
            }),
            '<' => basic(if self.match_('=') {
                TT::LessEqual
            } else {
                TT::Less
            }),
            '>' => basic(if self.match_('=') {
                TT::GreaterEqual
            } else {
                TT::Greater
            }),
            '/' => {
                if self.match_('/') {
                    let mut comment = String::new();
                    while self.source.peek() != Some(&'\n') && self.source.peek() != None {
                        comment.push(self.source.next().unwrap());
                    }
                    basic(TT::Comment(comment))
                } else {
                    basic(TT::Slash)
                }
            }
            c => Err(ScanError::UnexpectedChar(c, self.line)),
        }
    }

    fn match_(&mut self, c: char) -> bool {
        if self.source.peek() == Some(&c) {
            self.source.next();
            true
        } else {
            false
        }
    }
}
