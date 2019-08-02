use crate::token::{Token, TokenType as TT};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Value),
    Unary(Token, Box<Expr>),
}

impl Expr {
    // TODO: Result
    // TODO: move this to like a Resolver class?
    pub fn eval(&self) -> Value {
        match &self {
            Expr::Binary(left, op, right) => {
                let left = left.eval();
                let right = right.eval();

                match (&left, &op.type_, &right) {
                    // Math
                    (Value::Number(left), TT::Minus, Value::Number(right)) => {
                        Value::Number(left - right)
                    }
                    (Value::Number(left), TT::Slash, Value::Number(right)) => {
                        Value::Number(left / right)
                    }
                    (Value::Number(left), TT::Star, Value::Number(right)) => {
                        Value::Number(left * right)
                    }
                    (Value::Number(left), TT::Plus, Value::Number(right)) => {
                        Value::Number(left + right)
                    }
                    // Concat string
                    (Value::String(left), TT::Plus, Value::String(right)) => {
                        Value::String(left.to_string() + right)
                    }
                    // Compare
                    (Value::Number(left), TT::Greater, Value::Number(right)) => {
                        Value::Bool(left > right)
                    }
                    (Value::Number(left), TT::GreaterEqual, Value::Number(right)) => {
                        Value::Bool(left >= right)
                    }
                    (Value::Number(left), TT::Less, Value::Number(right)) => {
                        Value::Bool(left < right)
                    }
                    (Value::Number(left), TT::LessEqual, Value::Number(right)) => {
                        Value::Bool(left <= right)
                    }
                    // Equality
                    // TODO: can you express "these two enums are the _same_ constructor?"
                    (Value::Number(left), TT::EqualEqual, Value::Number(right)) => {
                        Value::Bool(left == right)
                    }
                    (Value::String(left), TT::EqualEqual, Value::String(right)) => {
                        Value::Bool(left == right)
                    }
                    (Value::Bool(left), TT::EqualEqual, Value::Bool(right)) => {
                        Value::Bool(left == right)
                    }
                    (Value::Nil, TT::EqualEqual, Value::Nil) => Value::Bool(true),
                    // Inequality
                    (Value::Number(left), TT::BangEqual, Value::Number(right)) => {
                        Value::Bool(left != right)
                    }
                    (Value::String(left), TT::BangEqual, Value::String(right)) => {
                        Value::Bool(left != right)
                    }
                    (Value::Bool(left), TT::BangEqual, Value::Bool(right)) => {
                        Value::Bool(left != right)
                    }
                    (Value::Nil, TT::BangEqual, Value::Nil) => Value::Bool(false),
                    (_, _, _) => panic!("Can't eval Binary {:?} {:?} {:?}", left, op, right),
                }
            }
            Expr::Grouping(expr) => expr.eval(),
            Expr::Literal(ref value) => value.clone(),
            Expr::Unary(op, expr) => {
                let val = expr.eval();

                match (&op.type_, val) {
                    (TT::Minus, Value::Number(num)) => Value::Number(-num),
                    // TODO: isTruthy?
                    (TT::Bang, Value::Bool(boolean)) => Value::Bool(!boolean),

                    (_, _) => panic!("Can't eval Unary {:?} {:?}", op, expr),
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
