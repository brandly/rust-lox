use core::cell::RefCell;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::mem;
use std::rc::Rc;

use crate::parser::{Expr, Stmt, Value};
use crate::token::Token;
use crate::token::TokenType as TT;

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug)]
pub enum AssignError {
    NotDeclared,
}

struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}
impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }
    pub fn enclosing(env: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(env),
        }
    }

    pub fn define(&mut self, name: &str, value: &Value) {
        self.values.insert(name.to_string(), value.clone());
    }

    pub fn assign(&mut self, name: &str, value: &Value) -> std::result::Result<(), AssignError> {
        if self.is_defined(name) {
            self.values.insert(name.to_string(), value.clone());
            Ok(())
        } else {
            Err(AssignError::NotDeclared)
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if self.values.contains_key(name) {
            self.values.get(name).cloned()
        } else {
            match &self.enclosing {
                Some(env) => env.borrow().get(name),
                None => None,
            }
        }
    }

    pub fn is_defined(&self, name: &str) -> bool {
        if self.values.contains_key(name) {
            true
        } else {
            match &self.enclosing {
                Some(env) => env.borrow().is_defined(name),
                None => false,
            }
        }
    }
}

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: Rc::new(RefCell::new(Environment::new())),
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
            Stmt::Block(stmts) => {
                let new_env = Environment::enclosing(Rc::clone(&self.env));
                let old_env = mem::replace(&mut self.env, Rc::new(RefCell::new(new_env)));

                for stmt in stmts {
                    self.exec_stmt(stmt)?;
                }

                self.env = old_env;
                Ok(())
            }
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
                    self.env.borrow_mut().define(name, &val);
                    Ok(())
                }
                (TT::Identifier(ref name), None) => {
                    self.env.borrow_mut().define(name, &Value::Nil);
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
                self.env
                    .borrow_mut()
                    .get(name)
                    .ok_or(RuntimeError::RuntimeError(
                        token.clone(),
                        format!("Undefined variable '{:?}'", name),
                    ))
            }
            Expr::Assign(token, expr) => {
                match &token.type_ {
                    TT::Identifier(name) => {
                        let val = self.eval(expr)?;
                        self.env
                            .borrow_mut()
                            .assign(name, &val)
                            .map(|_| val)
                            .map_err(|e| match e {
                                AssignError::NotDeclared => RuntimeError::RuntimeError(
                                    token.clone(),
                                    format!("Name '{}' has not been declared.", name),
                                ),
                            })
                    }
                    // TODO: types should be used better to prevent this
                    _ => Err(RuntimeError::RuntimeError(
                        token.clone(),
                        "Expected Identifier in Assign".to_string(),
                    )),
                }
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
