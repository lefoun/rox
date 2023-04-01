use crate::error::RuntimeError::{self, *};
use crate::expr::{expr_type, Expr};
use crate::parser::Visitor;
use crate::scanner::TokenType::{self, *};

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

#[derive(Clone, PartialEq, Debug)]
struct Object {
    identifier: String,
    ty: String,
    parent: String,
    fn_members: Vec<String>,
    var_members: Vec<String>,
}

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(&self, expr: Expr) -> Result<String, RuntimeError> {
        match self.accept(expr) {
            Ok(v) => Ok(v.to_string()),
            Err(e) => Err(e),
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

impl Visitor for Interpreter {
    type Value = Value;
    type Error = RuntimeError;

    fn accept(&self, expr: Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Binary(expr) => self.visit_binary(expr),
            Expr::Grouping(expr) => self.visit_grouping(expr),
            Expr::Unary(expr) => self.visit_unary(expr),
            Expr::Literal(expr) => self.visit_literal(expr),
        }
    }

    fn visit_literal(&self, expr: expr_type::Literal) -> Result<Self::Value, Self::Error> {
        Ok(match expr.ty() {
            Null => Value::Null,
            Number(num) => Value::Number(num),
            RoxString(str) => Value::Str(str),
            True => Value::Bool(true),
            False => Value::Bool(false),
            _ => {
                return Err(ExpectedLiteral {
                    token: expr.lexem().to_owned(),
                    line: expr.line(),
                })
            }
        })
    }

    fn visit_binary(&self, expr: expr_type::Binary) -> Result<Self::Value, Self::Error> {
        let left = self.accept(*expr.lhs().clone())?;
        let op = expr.op_ty();
        let right = self.accept(*expr.rhs().clone())?;

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
                op: expr.op_lexem().to_owned(),
                line: expr.line(),
            });
        }

        Ok(match left {
            Value::Number(lhs) => match right {
                Value::Number(rhs) => Self::binary_op_numbers(lhs, rhs, &op),
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        line: expr.line(),
                        expr_type: expr.op_lexem().to_owned(),
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
                            op: expr.op_lexem().to_owned(),
                            line: expr.line(),
                        })
                    }
                },
                _ => {
                    return Err(UnsupportedBinaryOperator {
                        op: expr.op_lexem().to_owned(),
                        line: expr.line(),
                    })
                }
            },
            Value::Bool(lhs) => match right {
                Value::Bool(rhs) => match op {
                    DoubleEqual | BangEqual => Self::eq_op(lhs, rhs, &op),
                    _ => {
                        return Err(UnsupportedBinaryOperator {
                            op: expr.op_lexem().to_owned(),
                            line: expr.line(),
                        })
                    }
                },
                Value::Null => match op {
                    DoubleEqual | BangEqual => Self::eq_op(lhs, false, &op),
                    _ => {
                        return Err(UnsupportedBinaryOperator {
                            op: expr.op_lexem().to_owned(),
                            line: expr.line(),
                        })
                    }
                },
                other => {
                    return Err(InvalidOperands {
                        lhs: lhs.to_string(),
                        rhs: other.to_string(),
                        expr_type: expr.op_lexem().to_owned(),
                        line: expr.line(),
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
                        expr_type: expr.op_lexem().to_owned(),
                        line: expr.line(),
                    })
                }
            },
            other => {
                return Err(InvalidOperand {
                    operand: other.to_string(),
                    line: expr.line(),
                    expr_type: expr.op_lexem().to_owned(),
                })
            }
        })
    }

    fn visit_grouping(&self, expr: expr_type::Grouping) -> Result<Self::Value, Self::Error> {
        self.accept(*expr.ty().clone())
    }

    fn visit_unary(&self, expr: expr_type::Unary) -> Result<Self::Value, Self::Error> {
        let v = self.accept(*expr.ty().clone())?;
        Ok(match expr.op_ty() {
            Bang => match v {
                Value::Bool(value) => Value::Bool(!value),
                Value::Null => Value::Bool(true),
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        line: expr.line(),
                        expr_type: expr.op_lexem().to_owned(),
                    })
                }
            },
            Minus => match v {
                Value::Number(num) => Value::Number(-num),
                other => {
                    return Err(InvalidOperand {
                        operand: other.to_string(),
                        line: expr.line(),
                        expr_type: expr.op_lexem().to_owned(),
                    })
                }
            },
            _ => {
                return Err(UnexpectedUnaryOperator {
                    op: expr.op_lexem().to_owned(),
                    line: expr.line(),
                })
            }
        })
    }
}
