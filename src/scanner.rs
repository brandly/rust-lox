use crate::token::{Token, TokenType as TT};
use std::error;
use std::fmt;
use std::iter;
use std::str::Chars;

#[derive(Debug)]
pub enum ScanError {
    UnexpectedChar(char, i32),
    UnterminatedString(i32),
}
impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ScanError::UnexpectedChar(c, line) => {
                write!(f, "Unexpected character {} at line {}", c, line)
            }
            ScanError::UnterminatedString(line) => {
                write!(f, "Unterminated string at line {}", line)
            }
        }
    }
}
impl error::Error for ScanError {
    fn description(&self) -> &str {
        match *self {
            ScanError::UnexpectedChar(_, _) => "Unexpected character",
            ScanError::UnterminatedString(_) => "Unterminated string",
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
            '"' => self.string().and_then(basic),
            c => {
                if c.is_ascii_digit() {
                    basic(TT::Number(self.digit(c)))
                } else if is_alpha(c) {
                    self.identifier(c)
                } else {
                    Err(ScanError::UnexpectedChar(c, self.line))
                }
            }
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

    fn string(&mut self) -> Result<TT, ScanError> {
        let mut string = String::new();

        loop {
            match self.source.next() {
                None => return Err(ScanError::UnterminatedString(self.line)),
                Some('\n') => {
                    self.line += 1;
                    string.push('\n');
                }
                Some('"') => {
                    return Ok(TT::String(string));
                }
                Some(c) => string.push(c),
            }
        }
    }

    fn digit(&mut self, start: char) -> f64 {
        let mut out = String::new();
        out.push(start);

        while self.source.peek().map(|c| c.is_ascii_digit()) == Some(true) {
            out.push(self.source.next().unwrap());
        }

        if self.source.peek() == Some(&'.') {
            // no `peekNext` so we've gotta consume the period...
            out.push(self.source.next().unwrap());
            while self.source.peek().map(|c| c.is_ascii_digit()) == Some(true) {
                out.push(self.source.next().unwrap());
            }
        }

        out.parse().unwrap()
    }

    fn identifier(&mut self, start: char) -> Result<Token, ScanError> {
        let mut id = start.to_string();

        loop {
            match self.source.peek() {
                Some(c) => {
                    if is_alphanumeric(*c) {
                        id.push(self.source.next().unwrap());
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
        Ok(Token::basic(TT::Identifier(id), self.line))
    }
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || c.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_token(type_: TT) -> Token {
        Token::basic(type_, 0)
    }

    fn to_tokens(types: Vec<TT>) -> Vec<Token> {
        types.into_iter().map(to_token).collect()
    }

    #[test]
    fn test_string() {
        let mut scanner = Scanner::new("(\"hello\");");

        assert_eq!(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![
                TT::LeftParen,
                TT::String("hello".to_string()),
                TT::RightParen,
                TT::Semicolon,
                TT::EOF,
            ])
        );
    }

    #[test]
    fn test_basic_number() {
        let mut scanner = Scanner::new("420");

        assert_eq!(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![TT::Number(420.), TT::EOF,])
        );
    }

    #[test]
    fn test_number_with_decimal() {
        let mut scanner = Scanner::new("4.20");

        assert_eq!(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![TT::Number(4.20), TT::EOF,])
        );
    }

    #[test]
    fn test_id() {
        let mut scanner = Scanner::new("abc = 123");

        assert_eq!(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![
                TT::Identifier("abc".to_string()),
                TT::Equal,
                TT::Number(123.),
                TT::EOF,
            ])
        );
    }
}
