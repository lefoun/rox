use super::interpreter::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub struct Environment {
    env: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            env: Rc::new(RefCell::new(HashMap::new())),
            parent,
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.env.borrow_mut().insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) {
        if self.env.borrow().get(&name).is_some() {
            self.env.borrow_mut().insert(name, value);
        } else if let Some(ref mut parent) = self.parent {
            parent.borrow_mut().assign(name, value);
        }
    }

    pub fn assign_at(&mut self, name: &str, value: Value, dist: usize) {
        self.ancestor(dist)
            .borrow_mut()
            .insert(name.to_owned(), value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(var) = self.env.borrow().get(name) {
            return Some(var.clone());
        } else if let Some(ref parent) = self.parent {
            return parent.borrow().get(name);
        }
        None
    }

    pub fn debug(&mut self) {
        dbg!(&self.env);
        if let Some(ref parent) = self.parent {
            parent.borrow_mut().debug();
        }
    }

    pub fn get_at(&mut self, name: &str, dist: usize) -> Option<Value> {
        // self.debug();
        self.ancestor(dist).borrow().get(name).cloned()
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        if let Some(ref parent) = self.parent {
            parent.borrow().get_global(name)
        } else {
            self.env.borrow().get(name).cloned()
        }
    }

    pub fn ancestor(&mut self, dist: usize) -> Rc<RefCell<HashMap<String, Value>>> {
        if dist == 0 {
            Rc::clone(&self.env)
        } else {
            self.parent
                .as_mut()
                .expect("Expect to have a parent if dist > 0")
                .borrow_mut()
                .ancestor(dist - 1)
        }
    }
}
