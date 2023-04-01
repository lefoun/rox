use super::environment::Environment;
use super::error::RuntimeError::{self, *};
use crate::parser::exprs::{expr_type, Expr, ExprVisitor};
use crate::parser::stmts::{stmt_type, Stmt, StmtVisitor};
use crate::scanner::scanner::TokenType::{self, *};
use std::collections::VecDeque;

#[derive(Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Null,
    Number(f64),
    Callable(RoxCallable),
    Str(String),
    Uninitialized,
}

impl Clone for RoxCallable {
    fn clone(&self) -> Self {
        Self(self.0.clone_box())
    }
}
struct RoxCallable(Box<dyn Callable>);
// hack to make RoxCallable comparable
impl PartialEq for RoxCallable {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_string() == other.0.to_string()
    }
}

impl RoxCallable {
    pub fn new(callable: Box<dyn Callable>) -> Self {
        Self(callable)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Null => write!(f, "Null"),
            Self::Number(num) => write!(f, "{num}"),
            Self::Str(str) => write!(f, "{str}"),
            Self::Callable(callable) => write!(f, "{}", callable.0.to_string()),
            Self::Uninitialized => write!(f, "Uninitialized"),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Callee {
    identifier: String,
}

impl Callee {
    pub fn new(identifier: String) -> Self {
        Self { identifier }
    }
}

pub struct Interpreter {
    env: VecDeque<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = VecDeque::new();
        let mut global_env = Environment::new();
        let time_function = super::native_functions::Clock;
        global_env.define(
            String::from("clock"),
            Value::Callable(RoxCallable::new(Box::new(time_function))),
        );
        env.push_front(global_env);
        Self { env }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<<Interpreter as StmtVisitor>::Value, RuntimeError> {
        StmtVisitor::accept(self, stmt)
    }

    fn assign_var(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        for env in &mut self.env {
            if env.get(&name).is_some() {
                env.define(name, value);
                return Ok(());
            }
        }
        Err(RuntimeError::UndefinedVariable { ident: name })
    }

    fn define_var(&mut self, name: String, value: Value) {
        self.env.front_mut().unwrap().define(name, value);
    }

    fn get_var(&mut self, name: &str) -> Result<Value, RuntimeError> {
        for env in &self.env {
            if env.get(name).is_some() {
                return Ok(env.get(name).unwrap());
            }
        }
        Err(RuntimeError::UndefinedVariable {
            ident: name.to_owned(),
        })
    }

    fn add_new_environment(&mut self) {
        self.env.push_front(Environment::new());
    }

    fn pop_environment(&mut self) {
        self.env.pop_front();
    }

    fn token_type_matches(ty: &TokenType, typs: &[TokenType]) -> bool {
        typs.contains(ty)
    }

    fn binary_op_numbers(lhs: f64, rhs: f64, op: &TokenType) -> Result<Value, RuntimeError> {
        match op {
            Minus => Ok(Value::Number(lhs - rhs)),
            Star => Ok(Value::Number(lhs * rhs)),
            Plus => Ok(Value::Number(lhs + rhs)),
            Percent => Ok(Value::Number(lhs % rhs)),
            Slash => {
                if rhs == 0.0 {
                    Ok(Value::Number(0.0))
                } else {
                    Ok(Value::Number(lhs / rhs))
                }
            }
            _ => Self::eq_order_op(lhs, rhs, op),
        }
    }

    fn eq_order_op<T: PartialOrd + std::fmt::Display>(
        lhs: T,
        rhs: T,
        op: &TokenType,
    ) -> Result<Value, RuntimeError> {
        match op {
            Greater => Ok(Value::Bool(lhs > rhs)),
            GreaterEqual => Ok(Value::Bool(lhs >= rhs)),
            Less => Ok(Value::Bool(lhs < rhs)),
            LessEqual => Ok(Value::Bool(lhs <= rhs)),
            _ => Self::eq_op(lhs, rhs, op),
        }
    }

    fn eq_op<T: PartialEq + std::fmt::Display>(
        lhs: T,
        rhs: T,
        op: &TokenType,
    ) -> Result<Value, RuntimeError> {
        let v = match op {
            DoubleEqual => lhs == rhs,
            BangEqual => lhs != rhs,
            _ => {
                return Err(RuntimeError::InvalidOperands {
                    lhs: lhs.to_string(),
                    rhs: rhs.to_string(),
                    expr_type: "binary".to_owned(),
                    msg: String::new(),
                });
            }
        };
        Ok(Value::Bool(v))
    }
}

impl StmtVisitor for Interpreter {
    type Value = Option<Value>;
    type Error = RuntimeError;

    fn accept(&mut self, stmt: Stmt) -> Result<Self::Value, Self::Error> {
        match stmt {
            Stmt::ExprStmt(s) => self.visit_expr_stmt(s),
            Stmt::Print(s) => self.visit_print_stmt(s),
            Stmt::VarDecl(s) => self.visit_var_stmt(s),
            Stmt::Block(s) => self.visit_block(s),
            Stmt::IfStmt(s) => self.visit_if_stmt(s),
            Stmt::WhileLoop(s) => self.visit_while_loop(s),
            Stmt::FunctionDecl(s) => self.visit_function_decl(s),
            Stmt::ReturnStmt(s) => self.visit_return_stmt(s),
        }
    }

    fn visit_expr_stmt(&mut self, stmt: stmt_type::ExprStmt) -> Result<Self::Value, Self::Error> {
        ExprVisitor::accept(self, stmt.value().clone())?;
        Ok(None)
    }

    fn visit_print_stmt(&mut self, stmt: stmt_type::Print) -> Result<Self::Value, Self::Error> {
        let v = ExprVisitor::accept(self, stmt.value().clone())?;
        println!("{v}");
        Ok(None)
    }

    fn visit_var_stmt(&mut self, stmt: stmt_type::VarDecl) -> Result<Self::Value, Self::Error> {
        let mut value = Value::Uninitialized;

        if let Some(expr) = stmt.ty() {
            value = <Self as ExprVisitor>::accept(self, expr.to_owned())?;
        }
        self.define_var(stmt.name().to_owned(), value);
        Ok(None)
    }

    fn visit_block(&mut self, stmt: stmt_type::Block) -> Result<Self::Value, Self::Error> {
        self.add_new_environment();
        let mut ret_value = None;

        for stmt in stmt.stmts() {
            if let Some(v) = self.execute(stmt.to_owned())? {
                ret_value = Some(v);
                break;
            }
        }

        self.pop_environment();
        Ok(ret_value)
    }

    fn visit_if_stmt(&mut self, stmt: stmt_type::IfStmt) -> Result<Self::Value, Self::Error> {
        match <Self as ExprVisitor>::accept(self, stmt.condition().to_owned()) {
            Ok(Value::Bool(val)) => match val {
                true => self.execute(stmt.then_branch().to_owned()),
                false => self.execute(stmt.else_branch().unwrap().to_owned()),
            },
            Ok(_) => Err(RuntimeError::ExpectedBooleanCondition),
            Err(e) => Err(e),
        }
    }

    fn visit_while_loop(&mut self, stmt: stmt_type::WhileLoop) -> Result<Self::Value, Self::Error> {
        loop {
            let condition = <Self as ExprVisitor>::accept(self, stmt.condition().to_owned())?;
            match condition {
                Value::Bool(true) => {
                    if let Some(v) = self.execute(Stmt::Block(stmt.body().to_owned()))? {
                        return Ok(Some(v));
                    }
                }
                Value::Bool(false) | Value::Null => return Ok(None),
                _ => return Err(RuntimeError::ExpectedBooleanCondition),
            }
        }
    }
    fn visit_function_decl(
        &mut self,
        stmt: stmt_type::FunctionDecl,
    ) -> Result<Self::Value, Self::Error> {
        let name = stmt.name().to_string();
        let function = LoxFunction::new(stmt);
        self.define_var(name, Value::Callable(RoxCallable::new(Box::new(function))));
        Ok(None)
    }

    fn visit_return_stmt(
        &mut self,
        stmt: stmt_type::ReturnStmt,
    ) -> Result<Self::Value, Self::Error> {
        let ret_value = match stmt.value() {
            Some(expr_value) => ExprVisitor::accept(self, expr_value.clone())?,
            None => Value::Null,
        };
        Ok(Some(ret_value))
    }
}

impl ExprVisitor for Interpreter {
    type Value = Value;
    type Error = RuntimeError;

    fn accept(&mut self, expr: Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Binary(expr) => self.visit_binary(expr),
            Expr::Grouping(expr) => self.visit_grouping(expr),
            Expr::Unary(expr) => self.visit_unary(expr),
            Expr::Literal(expr) => self.visit_literal(expr),
            Expr::Variable(expr) => self.visit_variable(expr),
            Expr::Assignment(expr) => self.visit_assignment(expr),
            Expr::Call(expr) => self.visit_call(expr),
        }
    }

    fn visit_literal(&mut self, expr: expr_type::Literal) -> Result<Self::Value, Self::Error> {
        Ok(match expr.ty() {
            Null => Value::Null,
            Number(num) => Value::Number(num),
            RoxString(str) => Value::Str(str),
            True => Value::Bool(true),
            False => Value::Bool(false),
            _ => {
                return Err(ExpectedLiteral {
                    token: expr.lexeme().to_owned(),
                })
            }
        })
    }

    fn visit_binary(&mut self, expr: expr_type::Binary) -> Result<Self::Value, Self::Error> {
        let left = ExprVisitor::accept(self, expr.lhs().clone())?;
        let op = expr.op_ty();
        if Self::token_type_matches(&op, &[Or]) {
            match left {
                Value::Bool(true) => return Ok(Value::Bool(true)),
                Value::Null | Value::Bool(false) => {
                    return ExprVisitor::accept(self, expr.rhs().clone())
                }
                _ => {
                    return Err(InvalidOperand {
                        operand: left.to_string(),
                        expr_type: expr.operator_lexeme().to_owned(),
                        msg: "".to_string(),
                    })
                }
            }
        }
        if Self::token_type_matches(&op, &[And]) {
            match left {
                Value::Bool(true) => {
                    let right = ExprVisitor::accept(self, expr.rhs().clone())?;
                    match right {
                        Value::Bool(true) => return Ok(Value::Bool(true)),
                        Value::Bool(false) | Value::Null => return Ok(Value::Bool(false)),
                        _ => {
                            return Err(InvalidOperand {
                                operand: right.to_string(),
                                expr_type: expr.operator_lexeme().to_owned(),
                                msg: "".to_string(),
                            })
                        }
                    }
                }
                Value::Null | Value::Bool(false) => {
                    return ExprVisitor::accept(self, expr.rhs().clone())
                }
                _ => {
                    return Err(InvalidOperand {
                        operand: left.to_string(),
                        expr_type: expr.operator_lexeme().to_owned(),
                        msg: "".to_string(),
                    })
                }
            }
        }
        let right = ExprVisitor::accept(self, expr.rhs().clone())?;

        if !Self::token_type_matches(
            &op,
            &[
                Minus,
                Star,
                Percent,
                Slash,
                Plus,
                DoubleEqual,
                BangEqual,
                Less,
                LessEqual,
                Greater,
                GreaterEqual,
            ],
        ) {
            return Err(UnsupportedBinaryOperator {
                op: expr.operator_lexeme().to_owned(),
            });
        }

        Ok(match left {
            Value::Number(lhs) => match right {
                Value::Number(rhs) => Self::binary_op_numbers(lhs, rhs, &op)?,
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        expr_type: expr.operator_lexeme().to_owned(),
                        msg: "".to_string(),
                    })
                }
            },
            Value::Str(lhs) => match right {
                Value::Str(rhs) => match op {
                    Plus => Value::Str(format!("{lhs}{rhs}")),
                    DoubleEqual | BangEqual | Less | LessEqual | Greater | GreaterEqual => {
                        Self::eq_order_op(lhs, rhs, &op)?
                    }
                    _ => {
                        return Err(InvalidOperands {
                            expr_type: format!("Binary expression {}", expr.operator_lexeme()),
                            rhs,
                            lhs,
                            msg: format!(
                                "Cannot perform operation {} on strings",
                                expr.operator_lexeme()
                            ),
                        })
                    }
                },
                _ => {
                    return Err(InvalidOperand {
                        expr_type: expr.operator_lexeme().to_owned(),
                        operand: lhs,
                        msg: format!("Invalid types for {} expression", expr.operator_lexeme()),
                    })
                }
            },
            Value::Bool(lhs) => match right {
                Value::Bool(rhs) => match op {
                    DoubleEqual | BangEqual => Self::eq_op(lhs, rhs, &op)?,
                    _ => {
                        return Err(UnsupportedBinaryOperator {
                            op: expr.operator_lexeme().to_owned(),
                        })
                    }
                },
                Value::Null => match op {
                    DoubleEqual | BangEqual => Self::eq_op(lhs, false, &op)?,
                    _ => {
                        return Err(UnsupportedBinaryOperator {
                            op: expr.operator_lexeme().to_owned(),
                        })
                    }
                },
                other => {
                    return Err(InvalidOperands {
                        lhs: lhs.to_string(),
                        rhs: other.to_string(),
                        expr_type: expr.operator_lexeme().to_owned(),
                        msg: "".to_string(),
                    })
                }
            },
            Value::Null => match right {
                Value::Bool(rhs) => Self::eq_op(false, rhs, &op)?,
                Value::Null => Self::eq_op(false, false, &op)?,
                other => {
                    return Err(InvalidOperands {
                        lhs: Value::Null.to_string(),
                        rhs: other.to_string(),
                        expr_type: expr.operator_lexeme().to_owned(),
                        msg: "".to_string(),
                    })
                }
            },
            other => {
                return Err(InvalidOperand {
                    operand: other.to_string(),
                    expr_type: expr.operator_lexeme().to_owned(),
                    msg: "".to_string(),
                })
            }
        })
    }

    fn visit_grouping(&mut self, expr: expr_type::Grouping) -> Result<Self::Value, Self::Error> {
        ExprVisitor::accept(self, expr.ty().clone())
    }

    fn visit_unary(&mut self, expr: expr_type::Unary) -> Result<Self::Value, Self::Error> {
        let v = ExprVisitor::accept(self, expr.expr().clone())?;
        Ok(match expr.operator_ty() {
            Bang => match v {
                Value::Bool(value) => Value::Bool(!value),
                Value::Null => Value::Bool(true),
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        expr_type: expr.operator_lexeme().to_owned(),
                        msg: "".to_string(),
                    })
                }
            },
            Minus => match v {
                Value::Number(num) => Value::Number(-num),
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        expr_type: expr.operator_lexeme().to_owned(),
                        msg: "".to_string(),
                    })
                }
            },
            _ => {
                return Err(UnexpectedUnaryOperator {
                    op: expr.operator_lexeme().to_owned(),
                })
            }
        })
    }

    fn visit_variable(&mut self, expr: expr_type::Variable) -> Result<Self::Value, Self::Error> {
        let var = self.get_var(expr.name())?;
        if var == Value::Uninitialized {
            return Err(UninitializedVariable {
                ident: expr.name().to_owned(),
            });
        } else {
            Ok(var)
        }
    }

    fn visit_assignment(
        &mut self,
        expr: expr_type::Assignment,
    ) -> Result<Self::Value, Self::Error> {
        let name = expr.name().to_owned();
        let value = ExprVisitor::accept(self, expr.ty().clone())?;
        self.assign_var(name, value.clone())?;
        Ok(value)
    }

    fn visit_call(&mut self, expr: expr_type::Call) -> Result<Self::Value, Self::Error> {
        let callee = match <Self as ExprVisitor>::accept(self, expr.callee().to_owned())? {
            Value::Callable(callable) => callable.0,
            other => {
                return Err(InvalidOperand {
                    operand: other.to_string(),
                    expr_type: "Call".to_owned(),
                    msg: "Can only call functions and classes".to_string(),
                })
            }
        };
        let args = expr
            .args()
            .iter()
            .cloned()
            .map(|arg| ExprVisitor::accept(self, arg))
            .collect::<Result<Vec<Self::Value>, Self::Error>>()?;

        let function = callee.call(self, args)?;
        Ok(function)
    }
}

// hack to make a trait object callable
pub trait ClonableCallable {
    fn clone_box(&self) -> Box<dyn Callable>;
}

impl<T> ClonableCallable for T
where
    T: 'static + Callable + Clone,
{
    fn clone_box(&self) -> Box<dyn Callable> {
        Box::new(self.clone())
    }
}

pub trait Callable: ClonableCallable {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError>;
    fn arity(&self) -> usize;
    fn to_string(&self) -> String;
}

#[derive(Clone, Debug)]
struct LoxFunction {
    declaration: stmt_type::FunctionDecl,
}

impl LoxFunction {
    pub fn new(declaration: stmt_type::FunctionDecl) -> Self {
        Self { declaration }
    }
}

impl Callable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut ret_val = Value::Null;
        if args.len() != self.arity() {
            return Err(MissingPositionalArguments {
                args: self
                    .declaration
                    .params()
                    .iter()
                    .skip(args.len())
                    .map(|p| format!("'{}'", p.lexeme()))
                    .collect(),
            });
        }
        let mut local_env = Environment::new();
        self.declaration
            .params()
            .iter()
            .zip(args.iter())
            .for_each(|(param, arg)| {
                local_env.define(param.lexeme().to_owned(), arg.clone());
            });
        interpreter.env.push_front(local_env);
        for stmt in self.declaration.body().iter() {
            match interpreter.execute(stmt.to_owned())? {
                None => (),
                Some(v) => {
                    ret_val = v;
                    break;
                }
            };
        }
        interpreter.env.pop_front();
        Ok(ret_val)
    }

    fn arity(&self) -> usize {
        self.declaration.params().len()
    }

    fn to_string(&self) -> String {
        format!("<function {}>", self.declaration.name())
    }
}