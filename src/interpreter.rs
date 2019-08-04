use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::parser::{Expr, Stmt, Value};
use crate::token::Token;
use crate::token::TokenType as TT;

type Result<T> = std::result::Result<T, RuntimeError>;

struct Environment {
    values: HashMap<String, Value>,
}
impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: String) -> Option<Value> {
        self.values.get(&name).cloned()
    }
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Environment {
                values: HashMap::new(),
            },
        }
    }

    pub fn execute(&mut self, stmts: &[Stmt]) -> Result<()> {
        for stmt in stmts {
            self.exec_stmt(stmt)?;
        }
        Ok(())
    }

    fn exec_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expression(expr) => {
                self.eval(expr)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                println!("{:?}", self.eval(expr)?);
                Ok(())
            }
            Stmt::VarDec(token, maybe_expr) => match (token.type_.clone(), maybe_expr) {
                (TT::Identifier(ref name), Some(expr)) => {
                    let val = self.eval(&expr)?;
                    self.env.define(name.clone(), val);
                    Ok(())
                }
                (TT::Identifier(ref name), None) => {
                    self.env.define(name.clone(), Value::Nil);
                    Ok(())
                }
                _ => Err(RuntimeError::RuntimeError(
                    token.clone(),
                    "Expected Identifier in VarDec".to_string(),
                )),
            },
        }
    }

    fn eval(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Binary(left, op, right) => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;

                match (&left, &op.type_, &right) {
                    // Math
                    (Value::Number(left), TT::Minus, Value::Number(right)) => {
                        Ok(Value::Number(left - right))
                    }
                    (Value::Number(left), TT::Slash, Value::Number(right)) => {
                        if *right == (0 as f64) {
                            Err(RuntimeError::RuntimeError(
                                op.clone(),
                                "Cannot divide by zero".to_string(),
                            ))
                        } else {
                            Ok(Value::Number(left / right))
                        }
                    }
                    (Value::Number(left), TT::Star, Value::Number(right)) => {
                        Ok(Value::Number(left * right))
                    }
                    (Value::Number(left), TT::Plus, Value::Number(right)) => {
                        Ok(Value::Number(left + right))
                    }
                    // Concat string
                    (Value::String(left), TT::Plus, Value::String(right)) => {
                        Ok(Value::String(left.to_string() + right))
                    }
                    // Compare
                    (Value::Number(left), TT::Greater, Value::Number(right)) => {
                        Ok(Value::Bool(left > right))
                    }
                    (Value::Number(left), TT::GreaterEqual, Value::Number(right)) => {
                        Ok(Value::Bool(left >= right))
                    }
                    (Value::Number(left), TT::Less, Value::Number(right)) => {
                        Ok(Value::Bool(left < right))
                    }
                    (Value::Number(left), TT::LessEqual, Value::Number(right)) => {
                        Ok(Value::Bool(left <= right))
                    }
                    // Equality
                    // TODO: can you express "these two enums are the _same_ constructor?"
                    (Value::Number(left), TT::EqualEqual, Value::Number(right)) => {
                        Ok(Value::Bool(left == right))
                    }
                    (Value::String(left), TT::EqualEqual, Value::String(right)) => {
                        Ok(Value::Bool(left == right))
                    }
                    (Value::Bool(left), TT::EqualEqual, Value::Bool(right)) => {
                        Ok(Value::Bool(left == right))
                    }
                    (Value::Nil, TT::EqualEqual, Value::Nil) => Ok(Value::Bool(true)),
                    // Inequality
                    (Value::Number(left), TT::BangEqual, Value::Number(right)) => {
                        Ok(Value::Bool(left != right))
                    }
                    (Value::String(left), TT::BangEqual, Value::String(right)) => {
                        Ok(Value::Bool(left != right))
                    }
                    (Value::Bool(left), TT::BangEqual, Value::Bool(right)) => {
                        Ok(Value::Bool(left != right))
                    }
                    (Value::Nil, TT::BangEqual, Value::Nil) => Ok(Value::Bool(false)),
                    (_, _, _) => Err(RuntimeError::RuntimeError(
                        op.clone(),
                        format!("Mismatched Binary types: {:?} and {:?}", left, right),
                    )),
                }
            }
            Expr::Grouping(expr) => self.eval(expr),
            Expr::Literal(ref value) => Ok(value.clone()),
            Expr::Unary(op, expr) => {
                let val = self.eval(expr)?;

                match (&op.type_, val) {
                    (TT::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
                    // TODO: isTruthy?
                    (TT::Bang, Value::Bool(boolean)) => Ok(Value::Bool(!boolean)),

                    (_, _) => panic!("Mismatched Unary types: {:?} {:?}", op, expr),
                }
            }
            Expr::Variable(token, name) => {
                self.env.get(name.clone()).ok_or(RuntimeError::RuntimeError(
                    token.clone(),
                    format!("Undefined variable '{:?}'", name),
                ))
            }
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    RuntimeError(Token, String),
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::RuntimeError(token, msg) => {
                write!(f, "RuntimeError at token {}: {}", token, msg)
            }
        }
    }
}
impl error::Error for RuntimeError {
    fn description(&self) -> &str {
        match *self {
            RuntimeError::RuntimeError(_, _) => "RuntimeError",
        }
    }
}
