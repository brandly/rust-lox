use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Value),
    Unary(Token, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

fn show(expr: Expr) -> String {
    match expr {
        Expr::Binary(left, op, right) => format!("({:?} {:?} {:?})", op, left, right),
        Expr::Grouping(expr) => format!("(group {:?})", expr),
        Expr::Literal(value) => format!("(literal {:?})", value),
        Expr::Unary(op, right) => format!("({:?} {:?})", op, right),
    }
}
