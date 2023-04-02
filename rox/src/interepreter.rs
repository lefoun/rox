use crate::environment::Environment;
use crate::error::RuntimeError::{self, *};
use crate::exprs::{expr_type, Expr, ExprVisitor};
use crate::scanner::TokenType::{self, *};
use crate::stmts::{stmt_type, Stmt, StmtVisitor};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Null,
    Number(f64),
    Object(Object),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Null => write!(f, "Null"),
            Self::Number(num) => write!(f, "{num}"),
            Self::Str(str) => write!(f, "{str}"),
            Self::Object(obj) => write!(f, "{}", obj.identifier),
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
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(None),
        }
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

    fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        if self.env.assign(name.clone(), value).is_some() {
            Ok(())
        } else {
            Err(InvalidIdentifier { ident: name })
        }
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
            Stmt::Print(s) => self.visit_print_stmt(s),
            Stmt::ExprStmt(s) => self.visit_expr_stmt(s),
            Stmt::VarDecl(s) => self.visit_var_stmt(s),
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
        let mut value = Value::Null;

        if let Some(expr) = stmt.ty() {
            value = <Self as ExprVisitor>::accept(self, expr.to_owned())?;
        }
        self.env.define(stmt.name().to_owned(), value);
        Ok(())
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
        match self.env.get(expr.name()) {
            Some(val) => Ok(val.clone()),
            None => Err(InvalidIdentifier {
                ident: expr.name().to_string(),
            }),
        }
    }

    fn visit_assignment(
        &mut self,
        expr: expr_type::Assignment,
    ) -> Result<Self::Value, Self::Error> {
        let name = expr.name().to_owned();
        let value = ExprVisitor::accept(self, expr.ty().clone())?;
        self.assign(name, value.clone())?;
        Ok(value)
    }
}
