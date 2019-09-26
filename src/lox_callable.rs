use crate::interpreter::{Interpreter, Result};
use crate::parser::Value;
use std::fmt;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

pub trait LoxCallable: fmt::Debug {
    fn call(&self, interpreter: &mut Interpreter, args: &[Value]) -> Result<Value>;
    fn arity(&self) -> usize;
    fn name(&self) -> &str;
}

impl fmt::Display for dyn LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}
impl PartialEq for dyn LoxCallable {
    fn eq(&self, other: &Self) -> bool {
        self.arity() == other.arity() && self.name() == other.name()
    }
}

#[derive(Debug)]
pub struct Clock;

impl LoxCallable for Clock {
    fn call(&self, _: &mut Interpreter, _: &[Value]) -> Result<Value> {
        let now = SystemTime::now();
        let since_the_epoch = now
            .duration_since(UNIX_EPOCH)
            .unwrap_or(Duration::from_secs(0));
        let secs = since_the_epoch.as_millis() as f64 / 1000.0;

        Ok(Value::Number(secs))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &str {
        "clock"
    }
}
