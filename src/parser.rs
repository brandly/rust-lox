use crate::token::{Token, TokenType as TT};
use std::error;
use std::fmt;

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser {
    pub tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt> {
        // TODO: synchronize
        if let Some(_) = self.match_(vec![TT::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }
    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self
            .consume_identifier("Expected variable name.".to_string())?
            .clone();

        let mut initial = None;
        if let Some(_) = self.match_(vec![TT::Equal]) {
            initial = Some(self.expression()?);
        }

        self.consume(
            TT::Semicolon,
            "Expected semicolon after variable declaration.".to_string(),
        )?;

        Ok(Stmt::VarDec(name, initial))
    }
    fn statement(&mut self) -> Result<Stmt> {
        if let Some(_) = self.match_(vec![TT::Print]) {
            return self.print_statement();
        }
        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(TT::Semicolon, "Expected ';' after value.".to_string())?;
        Ok(Stmt::Print(value))
    }
    fn expression_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(TT::Semicolon, "Expected ';' after expression.".to_string())?;
        Ok(Stmt::Expression(value))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.match_(vec![TT::BangEqual, TT::EqualEqual]) {
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn match_(&mut self, types: Vec<TT>) -> Option<Token> {
        for type_ in types {
            if self.check(&type_) {
                // TODO: cloning my way out of understanding ownership
                return self.advance().map(|t| t.clone());
            }
        }
        None
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.addition()?;

        while let Some(operator) =
            self.match_(vec![TT::Greater, TT::GreaterEqual, TT::Less, TT::LessEqual])
        {
            let right = self.addition()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr> {
        let mut expr = self.multiplication()?;

        while let Some(operator) = self.match_(vec![TT::Minus, TT::Plus]) {
            let right = self.multiplication()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.match_(vec![TT::Slash, TT::Star]) {
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some(operator) = self.match_(vec![TT::Bang, TT::Minus]) {
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }

        return self.primary();
    }

    fn primary(&mut self) -> Result<Expr> {
        let token = self.advance().expect("advance to next token in primary");

        let expr = match token.type_ {
            TT::False => Expr::Literal(Value::Bool(false)),
            TT::True => Expr::Literal(Value::Bool(true)),
            TT::Nil => Expr::Literal(Value::Nil),
            TT::Number(num) => Expr::Literal(Value::Number(num)),
            // TODO: learn about this `ref`
            TT::String(ref str) => Expr::Literal(Value::String(str.clone())),
            TT::Identifier(ref str) => Expr::Variable(token.clone(), str.clone()),
            TT::LeftParen => {
                let expr = self.expression()?;
                self.consume(TT::RightParen, "Expected ')' after expression.".to_string())?;
                Expr::Grouping(Box::new(expr))
            }
            // Expected expression
            _ => {
                return Err(ParseError::UnexpectedToken(
                    token.clone(),
                    "Expected false, true, nil, a number, a string, or a left paren".to_string(),
                ))
            }
        };

        Ok(expr)
    }

    fn consume(&mut self, type_: TT, msg: String) -> Result<&Token> {
        let token = self.advance().expect(&msg);
        if token.type_ == type_ {
            return Ok(token);
        } else {
            return Err(ParseError::UnexpectedToken(token.clone(), msg));
        }
    }
    fn consume_identifier(&mut self, msg: String) -> Result<&Token> {
        if let TT::Identifier(_) = self.peek().type_ {
            Ok(self.advance().unwrap())
        } else {
            Err(ParseError::UnexpectedToken(self.peek().clone(), msg))
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        let current = self.current;
        self.current += 1;
        self.tokens.get(current)
    }

    fn check(&mut self, type_: &TT) -> bool {
        self.peek().type_ == *type_
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.peek().type_ == TT::EOF
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_token(type_: TT) -> Token {
        Token::new(type_, 0)
    }

    fn to_tokens(types: Vec<TT>) -> Vec<Token> {
        types.into_iter().map(to_token).collect()
    }

    #[test]
    fn test_expression() {
        let mut parser = Parser::new(to_tokens(vec![
            TT::Number(3.),
            TT::Plus,
            TT::Number(2.),
            TT::EOF,
        ]));

        assert_eq!(
            parser.expression().unwrap(),
            Expr::Binary(
                Box::new(Expr::Literal(Value::Number(3.))),
                to_token(TT::Plus),
                Box::new(Expr::Literal(Value::Number(2.)))
            )
        );
    }

    #[test]
    fn test_expression_stmt() {
        let mut parser = Parser::new(to_tokens(vec![
            TT::Number(3.),
            TT::Plus,
            TT::Number(2.),
            TT::Semicolon,
            TT::EOF,
        ]));

        assert_eq!(
            parser.parse().unwrap(),
            vec![Stmt::Expression(Expr::Binary(
                Box::new(Expr::Literal(Value::Number(3.))),
                to_token(TT::Plus),
                Box::new(Expr::Literal(Value::Number(2.)))
            ))]
        );
    }

    #[test]
    fn test_print_stmt() {
        let mut parser = Parser::new(to_tokens(vec![
            TT::Print,
            TT::Minus,
            TT::Number(3.),
            TT::Semicolon,
            TT::EOF,
        ]));

        assert_eq!(
            parser.parse().unwrap(),
            vec![Stmt::Print(Expr::Unary(
                to_token(TT::Minus),
                Box::new(Expr::Literal(Value::Number(3.)))
            ))]
        );
    }
}

#[derive(Debug)]
pub enum ParseError {
    // ParseError,
    UnexpectedToken(Token, String),
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            // ParseError::ParseError => write!(f, "parse error"),
            ParseError::UnexpectedToken(ref token, ref msg) => {
                write!(f, "Unexpected token: {}. {}", token, msg)
            }
        }
    }
}
impl error::Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            // ParseError::ParseError => "ParseError",
            ParseError::UnexpectedToken(_, _) => "UnexpectedToken",
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    VarDec(Token, Option<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Value),
    Unary(Token, Box<Expr>),
    Variable(Token, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}
