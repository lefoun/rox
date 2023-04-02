use crate::interepreter::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    env: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            env: HashMap::new(),
            parent,
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.env.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) -> Option<Value> {
        if self.env.get(&name).is_some() {
            self.define(name, value.clone());
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().assign(name, value)
        } else {
            None
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.env.get(name) {
            return Some(value.clone());
        } else if let Some(parent) = &self.parent {
            let borrowd_parent = parent.borrow();
            return borrowd_parent.get(name);
        }
        None
    }

    pub fn set_parent(&mut self, parent: Rc<RefCell<Environment>>) {
        self.parent = Some(parent);
    }
}
