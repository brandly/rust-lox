use crate::expr::{Expr, Value};
use crate::token::{Token, TokenType as TT};

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    ParseError,
}
pub struct Parser {
    pub tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn expression(&mut self) -> Result<Expr> {
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
            if self.check(type_) {
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
        // TODO: better error
        let token = self.advance().ok_or(ParseError::ParseError)?;

        let expr = match token.type_ {
            TT::False => Expr::Literal(Value::Bool(false)),
            TT::True => Expr::Literal(Value::Bool(true)),
            TT::Nil => Expr::Literal(Value::Nil),
            TT::Number(num) => Expr::Literal(Value::Number(num)),
            // TODO: learn about this `ref`
            TT::String(ref str) => Expr::Literal(Value::String(str.clone())),
            TT::LeftParen => {
                let expr = self.expression()?;
                self.consume(TT::RightParen, "Expect ')' after expression.".to_string())?;
                Expr::Grouping(Box::new(expr))
            }
            _ => return Err(ParseError::ParseError),
        };

        Ok(expr)
    }

    fn consume(&mut self, type_: TT, msg: String) -> Result<()> {
        if let Some(token) = self.advance() {
            if token.type_ == type_ {
                return Ok(());
            } else {
                eprintln!("{}", msg);
                return Err(ParseError::ParseError);
            }
        }

        // Unexpected end of tokens
        Err(ParseError::ParseError)
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.is_at_end() {
            None
        } else {
            let current = self.current;
            self.current += 1;

            Some(&self.tokens[current])
        }
    }

    fn check(&mut self, type_: TT) -> bool {
        self.peek()
            .map(|token| token.type_ == type_)
            .unwrap_or(false)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn is_at_end(&self) -> bool {
        match self.peek() {
            Some(token) => token.type_ == TT::EOF,
            None => panic!("Parse ran out of tokens unexpectedly. Maybe missing an EOF."),
        }
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
}
