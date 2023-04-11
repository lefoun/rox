use super::interpreter::Value;
use std::collections::HashMap;

pub struct Environment {
    env: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.env.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.env.get(name).cloned()
    }
}
