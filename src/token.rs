use std::fmt;

pub struct Token {
    pub type_: TokenType,
    pub lexeme: String,
    // literal ?
    pub line: i32, // TODO: toString, need to impl trait
                   // https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
}
impl Token {
    pub fn basic(type_: TokenType, line: i32) -> Token {
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
pub enum TokenType {
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
