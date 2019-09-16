use crate::token::{Token, TokenType as TT};
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::iter;
use std::str::Chars;

type Result<T> = std::result::Result<T, ScanError>;

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
    column: i32,
    keywords: HashMap<&'static str, TT>,
}
impl<'a> Scanner<'a> {
    pub fn new(source: &str) -> Scanner {
        let keywords = [
            ("and", TT::And),
            ("class", TT::Class),
            ("else", TT::Else),
            ("false", TT::False),
            ("for", TT::For),
            ("fun", TT::Fun),
            ("if", TT::If),
            ("nil", TT::Nil),
            ("or", TT::Or),
            ("print", TT::Print),
            ("return", TT::Return),
            ("super", TT::Super),
            ("this", TT::This),
            ("true", TT::True),
            ("var", TT::Var),
            ("while", TT::While),
        ]
        .iter()
        .cloned()
        .collect();

        Scanner {
            source: source.chars().peekable(),
            line: 0,
            column: 0,
            keywords,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            let c = self.next();
            match c {
                Some(c) => {
                    if c.is_whitespace() {
                        if c == '\n' {
                            self.line += 1;
                            self.column = 0;
                        }
                        continue;
                    }
                    tokens.push(self.scan_token(c)?);
                }
                None => {
                    tokens.push(Token::new(TT::EOF, self.line, self.column));
                    break;
                }
            }
        }

        Ok(tokens)
    }

    fn scan_token(&mut self, c: char) -> Result<Token> {
        let line = self.line;
        let column = self.column;
        let basic = |type_| Ok(Token::new(type_, line, column));
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
                        comment.push(self.next().unwrap());
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
            self.next();
            true
        } else {
            false
        }
    }

    fn string(&mut self) -> Result<TT> {
        let mut string = String::new();

        loop {
            match self.next() {
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
            out.push(self.next().unwrap());
        }

        if self.source.peek() == Some(&'.') {
            // TODO: no `peekNext` so we've gotta consume the period...
            out.push(self.next().unwrap());
            while self.source.peek().map(|c| c.is_ascii_digit()) == Some(true) {
                out.push(self.next().unwrap());
            }
        }

        out.parse().unwrap()
    }

    fn identifier(&mut self, start: char) -> Result<Token> {
        let mut id = start.to_string();

        loop {
            match self.source.peek() {
                Some(c) => {
                    if is_alphanumeric(*c) {
                        id.push(self.next().unwrap());
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }

        // TODO: `&*` seems v hacky
        match self.keywords.get(&*id) {
            Some(type_) => Ok(Token::new(type_.clone(), self.line, self.column)),
            None => Ok(Token::new(TT::Identifier(id), self.line, self.column)),
        }
    }

    fn next(&mut self) -> Option<char> {
        self.column += 1;
        self.source.next()
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

    fn to_token((column, type_): (usize, TT)) -> Token {
        Token::new(type_, 0, column as i32)
    }

    fn to_tokens(types: Vec<TT>) -> Vec<Token> {
        types.into_iter().enumerate().map(to_token).collect()
    }

    fn assert_same_tokens(a: Vec<Token>, b: Vec<Token>) {
        assert_eq!(clean_tokens(a), clean_tokens(b));
    }

    fn clean_tokens(tokens: Vec<Token>) -> Vec<Token> {
        tokens.into_iter().map(clean_token).collect()
    }
    fn clean_token(t: Token) -> Token {
        let Token {
            type_,
            line: _,
            column: _,
        } = t;
        Token::new(type_, 0, 0)
    }

    #[test]
    fn test_string() {
        let mut scanner = Scanner::new("(\"hello\");");

        assert_same_tokens(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![
                TT::LeftParen,
                TT::String("hello".to_string()),
                TT::RightParen,
                TT::Semicolon,
                TT::EOF,
            ]),
        );
    }

    #[test]
    fn test_basic_number() {
        let mut scanner = Scanner::new("420");

        assert_same_tokens(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![TT::Number(420.), TT::EOF]),
        );
    }

    #[test]
    fn test_number_with_decimal() {
        let mut scanner = Scanner::new("4.20");

        assert_same_tokens(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![TT::Number(4.20), TT::EOF]),
        );
    }

    #[test]
    fn test_id() {
        let mut scanner = Scanner::new("abc = 123");

        assert_same_tokens(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![
                TT::Identifier("abc".to_string()),
                TT::Equal,
                TT::Number(123.),
                TT::EOF,
            ]),
        );
    }

    #[test]
    fn test_keyword() {
        let mut scanner = Scanner::new("class Dog {}");

        assert_same_tokens(
            scanner.scan_tokens().unwrap(),
            to_tokens(vec![
                TT::Class,
                TT::Identifier("Dog".to_string()),
                TT::LeftBrace,
                TT::RightBrace,
                TT::EOF,
            ]),
        );
    }
}
