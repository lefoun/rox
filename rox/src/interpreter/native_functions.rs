use super::error::RuntimeError;
use super::interpreter::{Callable, Value};

#[derive(Debug, Clone)]
pub struct Clock;

impl Callable for Clock {
    fn call(&self, _: &mut crate::Interpreter, _: Vec<Value>) -> Result<Value, RuntimeError> {
        Ok(Value::Number(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_millis() as f64,
        ))
    }

    fn arity(&self) -> usize {
        0
    }

    fn to_string(&self) -> String {
        format!("<function {}>", "clock")
    }
}
