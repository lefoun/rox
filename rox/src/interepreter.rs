use crate::environment::Environment;
use crate::error::RuntimeError::{self, *};
use crate::exprs::{expr_type, Expr, ExprVisitor};
use crate::scanner::TokenType::{self, *};
use crate::stmts::{stmt_type, Stmt, StmtVisitor};
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Null,
    Number(f64),
    Object(Object),
    Str(String),
    Uninitialized,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Null => write!(f, "Null"),
            Self::Number(num) => write!(f, "{num}"),
            Self::Str(str) => write!(f, "{str}"),
            Self::Object(obj) => write!(f, "{}", obj.identifier),
            Self::Uninitialized => write!(f, "Uninitialized"),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Object {
    identifier: String,
    ty: String,
    parent: String,
    fn_members: Vec<String>,
    var_members: Vec<String>,
}

pub struct Interpreter {
    env: VecDeque<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = VecDeque::new();
        env.push_front(Environment::new());
        Self { env }
    }

    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), RuntimeError> {
        StmtVisitor::accept(self, stmt)?;
        Ok(())
    }

    fn assign_var(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        for env in &mut self.env {
            if env.get(&name).is_some() {
                env.define(name, value.clone());
                return Ok(());
            }
        }
        Err(RuntimeError::UndefinedVariable { ident: name })
    }

    fn define_var(&mut self, name: String, value: Value) {
        self.env.front_mut().unwrap().define(name, value.clone());
    }

    fn get_var(&mut self, name: &str) -> Result<Value, RuntimeError> {
        for env in &self.env {
            if env.get(&name).is_some() {
                return Ok(env.get(&name).unwrap().clone());
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
        typs.contains(&ty)
    }

    fn binary_op_numbers(lhs: f64, rhs: f64, op: &TokenType) -> Value {
        match op {
            Minus => Value::Number(lhs - rhs),
            Star => Value::Number(lhs * rhs),
            Plus => Value::Number(lhs + rhs),
            Slash => {
                if rhs == 0.0 {
                    Value::Number(0.0)
                } else {
                    Value::Number(lhs / rhs)
                }
            }
            _ => Self::eq_order_op(lhs, rhs, op),
        }
    }

    fn eq_order_op<T: PartialOrd + std::fmt::Display>(lhs: T, rhs: T, op: &TokenType) -> Value {
        match op {
            Greater => Value::Bool(lhs > rhs),
            GreaterEqual => Value::Bool(lhs >= rhs),
            Less => Value::Bool(lhs < rhs),
            LessEqual => Value::Bool(lhs <= rhs),
            _ => Self::eq_op(lhs, rhs, op),
        }
    }

    fn eq_op<T: PartialEq + std::fmt::Display>(lhs: T, rhs: T, op: &TokenType) -> Value {
        let v = match op {
            DoubleEqual => lhs == rhs,
            BangEqual => lhs != rhs,
            _ => unreachable!(),
        };
        Value::Bool(v)
    }
}

impl StmtVisitor for Interpreter {
    type Value = ();
    type Error = RuntimeError;

    fn accept(&mut self, stmt: Stmt) -> Result<Self::Value, Self::Error> {
        match stmt {
            Stmt::ExprStmt(s) => self.visit_expr_stmt(s),
            Stmt::Print(s) => self.visit_print_stmt(s),
            Stmt::VarDecl(s) => self.visit_var_stmt(s),
            Stmt::Block(s) => self.visit_block(s),
            Stmt::IfStmt(s) => self.visit_if_stmt(s),
        }
    }

    fn visit_expr_stmt(
        &mut self,
        stmt: crate::stmts::stmt_type::ExprStmt,
    ) -> Result<Self::Value, Self::Error> {
        ExprVisitor::accept(self, stmt.value().clone())?;
        Ok(())
    }

    fn visit_print_stmt(
        &mut self,
        stmt: crate::stmts::stmt_type::Print,
    ) -> Result<Self::Value, Self::Error> {
        let v = ExprVisitor::accept(self, stmt.value().clone())?;
        println!("{v}");
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: stmt_type::VarDecl) -> Result<Self::Value, Self::Error> {
        let mut value = Value::Uninitialized;

        if let Some(expr) = stmt.ty() {
            value = <Self as ExprVisitor>::accept(self, expr.to_owned())?;
        }
        self.define_var(stmt.name().to_owned(), value);
        Ok(())
    }

    fn visit_block(&mut self, stmt: stmt_type::Block) -> Result<Self::Value, Self::Error> {
        self.add_new_environment();

        for stmt in stmt.stmts() {
            self.execute(stmt.to_owned())?;
        }

        self.pop_environment();
        Ok(())
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
                    token: expr.lexem().to_owned(),
                })
            }
        })
    }

    fn visit_binary(&mut self, expr: expr_type::Binary) -> Result<Self::Value, Self::Error> {
        let left = ExprVisitor::accept(self, expr.lhs().clone())?;
        let op = expr.op_ty();
        let right = ExprVisitor::accept(self, expr.rhs().clone())?;

        if !Self::token_type_matches(
            &op,
            &[
                Minus,
                Star,
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
                op: expr.operator_lexem().to_owned(),
            });
        }

        Ok(match left {
            Value::Number(lhs) => match right {
                Value::Number(rhs) => Self::binary_op_numbers(lhs, rhs, &op),
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        expr_type: expr.operator_lexem().to_owned(),
                    })
                }
            },
            Value::Str(lhs) => match right {
                Value::Str(rhs) => match op {
                    Plus => Value::Str(format!("{lhs}{rhs}")),
                    DoubleEqual | BangEqual | Less | LessEqual | Greater | GreaterEqual => {
                        Self::eq_order_op(lhs, rhs, &op)
                    }
                    _ => {
                        return Err(UnsupportedBinaryOperator {
                            op: expr.operator_lexem().to_owned(),
                        })
                    }
                },
                _ => {
                    return Err(UnsupportedBinaryOperator {
                        op: expr.operator_lexem().to_owned(),
                    })
                }
            },
            Value::Bool(lhs) => match right {
                Value::Bool(rhs) => match op {
                    DoubleEqual | BangEqual => Self::eq_op(lhs, rhs, &op),
                    _ => {
                        return Err(UnsupportedBinaryOperator {
                            op: expr.operator_lexem().to_owned(),
                        })
                    }
                },
                Value::Null => match op {
                    DoubleEqual | BangEqual => Self::eq_op(lhs, false, &op),
                    _ => {
                        return Err(UnsupportedBinaryOperator {
                            op: expr.operator_lexem().to_owned(),
                        })
                    }
                },
                other => {
                    return Err(InvalidOperands {
                        lhs: lhs.to_string(),
                        rhs: other.to_string(),
                        expr_type: expr.operator_lexem().to_owned(),
                    })
                }
            },
            Value::Null => match right {
                Value::Bool(rhs) => Self::eq_op(false, rhs, &op),
                Value::Null => Self::eq_op(false, false, &op),
                other => {
                    return Err(InvalidOperands {
                        lhs: Value::Null.to_string(),
                        rhs: other.to_string(),
                        expr_type: expr.operator_lexem().to_owned(),
                    })
                }
            },
            other => {
                return Err(InvalidOperand {
                    operand: other.to_string(),
                    expr_type: expr.operator_lexem().to_owned(),
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
                        expr_type: expr.operator_lexem().to_owned(),
                    })
                }
            },
            Minus => match v {
                Value::Number(num) => Value::Number(-num),
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        expr_type: expr.operator_lexem().to_owned(),
                    })
                }
            },
            _ => {
                return Err(UnexpectedUnaryOperator {
                    op: expr.operator_lexem().to_owned(),
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
}
