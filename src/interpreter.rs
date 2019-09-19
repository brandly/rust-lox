use core::cell::RefCell;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::mem;
use std::rc::Rc;

use crate::lox_callable::Clock;
use crate::lox_callable::LoxCallable;
use crate::parser::{Expr, Stmt, Value};
use crate::token::Token;
use crate::token::TokenType as TT;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug)]
pub enum AssignError {
    NotDeclared,
}

#[derive(Debug)]
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
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value.clone());
            Ok(())
        } else if let Some(env) = &self.enclosing {
            env.borrow_mut().assign(name, value)
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
}

pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut globals = Environment::new();
        globals.define("clock", &Value::Callable(Rc::new(Box::new(Clock))));
        Interpreter {
            env: Rc::new(RefCell::new(globals)),
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
            Stmt::Block(ref stmts) => {
                let new_env = Rc::new(RefCell::new(Environment::enclosing(Rc::clone(&self.env))));
                self.exec_block(stmts, new_env)?;
                Ok(())
            }
            Stmt::Expression(expr) => {
                self.eval(expr)?;
                Ok(())
            }
            Stmt::Function(name, parameters, body) => {
                let func = LoxFunction::new(name.clone(), parameters.clone(), body.to_vec(), Rc::clone(&self.env));
                match &name.type_ {
                    TT::Identifier(name_str) => {
                        self.env
                            .borrow_mut()
                            .define(&name_str, &Value::Callable(Rc::new(Box::new(func))));
                        Ok(())
                    }
                    _ => panic!("Expected identifier but got {:?}", name),
                }
            }
            Stmt::Print(expr) => {
                println!("{:?}", self.eval(expr)?);
                Ok(())
            }
            Stmt::Return(_return, stmt) => {
                let mut output = Value::Nil;
                if let Some(value) = stmt {
                    output = self.eval(value)?;
                }
                Err(RuntimeError::Return(output))
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
            Stmt::If(cond, then_stmt, else_stmt) => {
                if is_truthy(&self.eval(cond)?) {
                    self.exec_stmt(then_stmt)
                } else if let Some(stmt) = else_stmt {
                    self.exec_stmt(stmt)
                } else {
                    Ok(())
                }
            }
            Stmt::While(cond, stmt) => {
                while is_truthy(&self.eval(cond)?) {
                    self.exec_stmt(stmt)?;
                }

                Ok(())
            }
        }
    }

    fn exec_block(&mut self, stmts: &Vec<Stmt>, new_env: Rc<RefCell<Environment>>) -> Result<()> {
        let old_env = mem::replace(&mut self.env, new_env);

        for stmt in stmts {
            match self.exec_stmt(&stmt) {
                Ok(_) => continue,
                Err(err) => {
                    self.env = old_env;
                    return Err(err);
                }
            }
        }

        self.env = old_env;
        Ok(())
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
            Expr::Call(callee_expr, paren, arguments_expr) => {
                let callee = self.eval(callee_expr)?;

                let mut arguments = Vec::new();
                for arg in arguments_expr {
                    arguments.push(self.eval(arg)?);
                }

                match callee {
                    Value::Callable(callable) => {
                        if callable.arity() == arguments.len() {
                            callable.call(self, &arguments)
                        } else {
                            Err(RuntimeError::RuntimeError(
                                paren.clone(),
                                format!(
                                    "Expected {:?} argument(s) but got {:?}",
                                    callable.arity(),
                                    arguments.len()
                                ),
                            ))
                        }
                    }
                    val => Err(RuntimeError::RuntimeError(
                        paren.clone(),
                        format!("Can only call functions and classes. Found {:?}", val),
                    )),
                }
            }
            Expr::Grouping(expr) => self.eval(expr),
            Expr::Literal(ref value) => Ok(value.clone()),
            Expr::Logical(left_expr, op, right_expr) => {
                let left = self.eval(left_expr)?;

                if op.type_ == TT::Or {
                    if is_truthy(&left) {
                        return Ok(left);
                    }
                } else {
                    if !is_truthy(&left) {
                        return Ok(left);
                    }
                }

                Ok(self.eval(right_expr)?)
            }
            Expr::Unary(op, expr) => {
                let val = self.eval(expr)?;

                match (&op.type_, val) {
                    (TT::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
                    (TT::Bang, val) => Ok(Value::Bool(!is_truthy(&val))),
                    (_, _) => Err(RuntimeError::RuntimeError(
                        op.clone(),
                        format!("Mismatched Unary types: {:?} and {:?}", op, expr),
                    )),
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

// ruby-esque
fn is_truthy(val: &Value) -> bool {
    match &val {
        Value::Nil => false,
        Value::Bool(boolean) => *boolean,
        _ => true,
    }
}

#[derive(Debug)]
struct LoxFunction {
    name: Token,
    parameters: Vec<Token>,
    body: Vec<Stmt>,
    env: Rc<RefCell<Environment>>
}

impl LoxFunction {
    pub fn new(name: Token, parameters: Vec<Token>, body: Vec<Stmt>, env: Rc<RefCell<Environment>>) -> Self {
        LoxFunction {
            name,
            parameters,
            body,
            env,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value> {
        let mut env = Environment::enclosing(Rc::clone(&self.env));
        for (param, arg) in self.parameters.clone().into_iter().zip(args) {
            match &param.type_ {
                TT::Identifier(name) => env.define(name, arg),
                token => panic!("Expected Identifier but got {:?}", token),
            }
        }

        match interpreter.exec_block(&self.body, Rc::new(RefCell::new(env))) {
            Err(RuntimeError::Return(val)) => Ok(val),
            Err(err) => Err(err),
            Ok(_) => Ok(Value::Nil),
        }
    }
    fn arity(&self) -> usize {
        self.parameters.len()
    }
    fn name(&self) -> &str {
        // &self.name.to_string().clone()
        "uh"
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    RuntimeError(Token, String),
    Return(Value),
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::RuntimeError(token, msg) => {
                write!(f, "RuntimeError at token {}: {}", token, msg)
            }
            RuntimeError::Return(val) => write!(f, "Return value: {:?}", val),
        }
    }
}
impl error::Error for RuntimeError {
    fn description(&self) -> &str {
        match *self {
            RuntimeError::RuntimeError(_, _) => "RuntimeError",
            RuntimeError::Return(_) => "Return",
        }
    }
}
