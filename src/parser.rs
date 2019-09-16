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
        if let Some(_) = self.match_(vec![TT::For]) {
            return self.for_statement();
        } else if let Some(_) = self.match_(vec![TT::If]) {
            return self.if_statement();
        } else if let Some(_) = self.match_(vec![TT::Print]) {
            return self.print_statement();
        } else if let Some(_) = self.match_(vec![TT::While]) {
            return self.while_statement();
        } else if let Some(_) = self.match_(vec![TT::LeftBrace]) {
            return Ok(Stmt::Block(self.block()?));
        }
        self.expression_statement()
    }
    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = vec![];

        while !self.check(&TT::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TT::RightBrace, "Expected '}' after block.".to_string())?;
        Ok(statements)
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(TT::LeftParen, "Expected '(' after 'for'.".to_string())?;

        let mut initializer;
        if let Some(_) = self.match_(vec![TT::Semicolon]) {
            initializer = None;
        } else if let Some(_) = self.match_(vec![TT::Var]) {
            initializer = Some(self.var_declaration()?);
        } else {
            initializer = Some(self.expression_statement()?);
        }

        let mut condition = None;
        if !self.check(&TT::Semicolon) {
            condition = Some(self.expression()?);
        }
        self.consume(
            TT::Semicolon,
            "Expected ';' after loop condition.".to_string(),
        )?;

        let mut increment = None;
        if !self.check(&TT::RightParen) {
            increment = Some(self.expression()?);
        }
        self.consume(
            TT::RightParen,
            "Expected ')' after `for` clauses.".to_string(),
        )?;

        let mut body = self.statement()?;
        if let Some(increment_val) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment_val)])
        }
        body = Stmt::While(
            condition.unwrap_or(Expr::Literal(Value::Bool(true))),
            Box::new(body),
        );

        if let Some(initializer_val) = initializer {
            body = Stmt::Block(vec![initializer_val, body])
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(TT::LeftParen, "Expected '(' after 'if'.".to_string())?;
        let condition = self.expression()?;
        self.consume(
            TT::RightParen,
            "Expected '(' after if condition.".to_string(),
        )?;

        let then_branch = self.statement()?;
        if let Some(_) = self.match_(vec![TT::Else]) {
            let else_branch = self.statement()?;
            Ok(Stmt::If(
                condition,
                Box::new(then_branch),
                Some(Box::new(else_branch)),
            ))
        } else {
            Ok(Stmt::If(condition, Box::new(then_branch), None))
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(TT::Semicolon, "Expected ';' after value.".to_string())?;
        Ok(Stmt::Print(value))
    }
    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(TT::LeftParen, "Expected '(' after 'while'.".to_string())?;
        let condition = self.expression()?;
        self.consume(
            TT::RightParen,
            "Expected ')' after while condition.".to_string(),
        )?;
        let body = self.statement()?;
        Ok(Stmt::While(condition, Box::new(body)))
    }
    fn expression_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(TT::Semicolon, "Expected ';' after expression.".to_string())?;
        Ok(Stmt::Expression(value))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;

        if let Some(equal) = self.match_(vec![TT::Equal]) {
            let value = self.assignment()?;

            return match expr {
                Expr::Variable(token, _) => Ok(Expr::Assign(token, Box::new(value))),
                _ => Err(ParseError::UnexpectedToken(
                    equal,
                    "Expected Variable before =.".to_string(),
                )),
            };
        }

        Ok(expr)
    }
    fn or(&mut self) -> Result<Expr> {
        let mut expr = self.and()?;
        while let Some(operator) = self.match_(vec![TT::Or]) {
            let right = self.and()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }
    fn and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        while let Some(operator) = self.match_(vec![TT::And]) {
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
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

        self.primary()
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
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken(token.clone(), msg))
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
    Block(Vec<Stmt>),
    Expression(Expr),
    Print(Expr),
    VarDec(Token, Option<Expr>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Value),
    Logical(Box<Expr>, Token, Box<Expr>),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn to_token((column, type_): (usize, TT)) -> Token {
        Token::new(type_, 0, column as i32)
    }

    fn to_tokens(types: Vec<TT>) -> Vec<Token> {
        types.into_iter().enumerate().map(to_token).collect()
    }

    #[test]
    fn expression() {
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
                to_token((1, TT::Plus)),
                Box::new(Expr::Literal(Value::Number(2.)))
            )
        );
    }

    #[test]
    fn expression_stmt() {
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
                to_token((1, TT::Plus)),
                Box::new(Expr::Literal(Value::Number(2.)))
            ))]
        );
    }

    #[test]
    fn print_stmt() {
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
                to_token((1, TT::Minus)),
                Box::new(Expr::Literal(Value::Number(3.)))
            ))]
        );
    }

    #[test]
    fn block() {
        let mut parser = Parser::new(to_tokens(vec![
            TT::LeftBrace,
            TT::Minus,
            TT::Number(3.),
            TT::Semicolon,
            TT::RightBrace,
            TT::EOF,
        ]));

        assert_eq!(
            parser.parse().unwrap(),
            vec![Stmt::Block(vec![Stmt::Expression(Expr::Unary(
                to_token((1, TT::Minus)),
                Box::new(Expr::Literal(Value::Number(3.)))
            ))])]
        );
    }

    #[test]
    fn for_loop() {
        let mut parser = Parser::new(to_tokens(vec![
            TT::For,
            TT::LeftParen,
            TT::Var,
            TT::Identifier("i".to_string()),
            TT::Equal,
            TT::Number(0.0),
            TT::Semicolon,
            TT::Identifier("i".to_string()),
            TT::Less,
            TT::Number(5.0),
            TT::Semicolon,
            TT::Identifier("i".to_string()),
            TT::Equal,
            TT::Identifier("i".to_string()),
            TT::Plus,
            TT::Number(1.0),
            TT::RightParen,
            TT::LeftBrace,
            TT::RightBrace,
            TT::EOF,
        ]));

        assert_eq!(
            parser.parse().unwrap(),
            vec![Stmt::Block(vec![
                Stmt::VarDec(
                    Token {
                        type_: TT::Identifier("i".to_string()),
                        column: 3,
                        line: 0
                    },
                    Some(Expr::Literal(Value::Number(0.0)))
                ),
                Stmt::While(
                    Expr::Binary(
                        Box::new(Expr::Variable(
                            Token {
                                type_: TT::Identifier("i".to_string()),
                                column: 7,
                                line: 0
                            },
                            "i".to_string()
                        )),
                        Token {
                            type_: TT::Less,
                            column: 8,
                            line: 0
                        },
                        Box::new(Expr::Literal(Value::Number(5.0)))
                    ),
                    Box::new(Stmt::Block(vec![
                        Stmt::Block(vec![]),
                        Stmt::Expression(Expr::Assign(
                            Token {
                                type_: TT::Identifier("i".to_string()),
                                column: 11,
                                line: 0
                            },
                            Box::new(Expr::Binary(
                                Box::new(Expr::Variable(
                                    Token {
                                        type_: TT::Identifier("i".to_string()),
                                        column: 13,
                                        line: 0
                                    },
                                    "i".to_string()
                                )),
                                Token {
                                    type_: TT::Plus,
                                    column: 14,
                                    line: 0
                                },
                                Box::new(Expr::Literal(Value::Number(1.0)))
                            ))
                        ))
                    ]))
                )
            ])]
        );
    }
}
