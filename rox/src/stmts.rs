use crate::exprs::Expr;
use stmt_type::*;

pub trait StmtVisitor {
    type Value;
    type Error;

    fn accept(&self, stmt: Stmt) -> Result<Self::Value, Self::Error>;
    fn visit_print_stmt(&self, stmt: stmt_type::Print) -> Result<Self::Value, Self::Error>;
    fn visit_expr_stmt(&self, stmt: stmt_type::ExprStmt) -> Result<Self::Value, Self::Error>;
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(Print),
    ExprStmt(ExprStmt),
}

pub mod stmt_type {
    use super::*;
    #[derive(Debug, Clone)]
    pub struct Print(Box<Expr>);
    impl Print {
        pub fn new(value: Box<Expr>) -> Self {
            Self(value)
        }

        pub fn value(&self) -> &Box<Expr> {
            &self.0
        }
    }

    #[derive(Debug, Clone)]
    pub struct ExprStmt(Box<Expr>);
    impl ExprStmt {
        pub fn new(value: Box<Expr>) -> Self {
            Self(value)
        }

        pub fn value(&self) -> &Box<Expr> {
            &self.0
        }
    }
}
