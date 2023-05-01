use super::error::RuntimeError;
use super::interpreter::{Callable, Value};

#[derive(Debug, Clone)]
pub struct Clock;

impl Callable for Clock {
    fn call(&mut self, _: &mut crate::Interpreter, _: Vec<Value>) -> Result<Value, RuntimeError> {
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
        format!("<builtin function {}>", "clock")
    }
}

#[derive(Debug, Clone)]
pub struct Print;

impl Callable for Print {
    fn call(
        &mut self,
        _: &mut super::interpreter::Interpreter,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if self.arity() < args.len() {
            return Err(RuntimeError::TooManyArguments {
                fun: "Print".to_string(),
            });
        }
        for val in args.into_iter() {
            print!("{val}");
        }
        Ok(Value::Null)
    }

    fn to_string(&self) -> String {
        format!("<builtin function {}>", "print")
    }

    fn arity(&self) -> usize {
        128
    }
}

#[derive(Debug, Clone)]
pub struct PrintLn;

impl Callable for PrintLn {
    fn call(
        &mut self,
        interpreter: &mut super::interpreter::Interpreter,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut args = args;
        let mut print = Print;

        args.push(Value::Str("\n".to_string()));
        print.call(interpreter, args)
    }

    fn arity(&self) -> usize {
        127
    }

    fn to_string(&self) -> String {
        format!("<builtin function {}>", "println")
    }
}
