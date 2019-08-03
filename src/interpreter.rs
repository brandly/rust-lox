use crate::expr::{Expr, Value};
use crate::stmt::Stmt;
use crate::token::TokenType as TT;

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }
    // TODO: Result
    pub fn execute(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.exec_stmt(stmt);
        }
    }

    fn exec_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.eval(expr);
                ()
            }
            Stmt::Print(expr) => {
                println!("{:?}", self.eval(expr));
                ()
            }
        }
    }

    fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Binary(left, op, right) => {
                let left = self.eval(left);
                let right = self.eval(right);

                match (&left, &op.type_, &right) {
                    // Math
                    (Value::Number(left), TT::Minus, Value::Number(right)) => {
                        Value::Number(left - right)
                    }
                    // TODO: divide by zero runtime error
                    // (Value::Number(left), TT::Slash, Value::Number(0)) => {
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
                    (_, _, _) => panic!("Mismatched Binary types: {:?} {:?} {:?}", left, op, right),
                }
            }
            Expr::Grouping(expr) => self.eval(expr),
            Expr::Literal(ref value) => value.clone(),
            Expr::Unary(op, expr) => {
                let val = self.eval(expr);

                match (&op.type_, val) {
                    (TT::Minus, Value::Number(num)) => Value::Number(-num),
                    // TODO: isTruthy?
                    (TT::Bang, Value::Bool(boolean)) => Value::Bool(!boolean),

                    (_, _) => panic!("Mismatched Unary types: {:?} {:?}", op, expr),
                }
            }
        }
    }
}
