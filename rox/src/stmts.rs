use crate::exprs::Expr;
use crate::scanner::Token;
use stmt_type::*;

pub trait StmtVisitor {
    type Value;
    type Error;

    fn accept(&mut self, stmt: Stmt) -> Result<Self::Value, Self::Error>;
    fn visit_print_stmt(&mut self, stmt: stmt_type::Print) -> Result<Self::Value, Self::Error>;
    fn visit_expr_stmt(&mut self, stmt: stmt_type::ExprStmt) -> Result<Self::Value, Self::Error>;
    fn visit_var_stmt(&mut self, stmt: stmt_type::VarDecl) -> Result<Self::Value, Self::Error>;
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(Print),
    ExprStmt(ExprStmt),
    VarDecl(VarDecl),
}

impl Stmt {
    pub fn new_print(value: Expr) -> Self {
        Self::Print(stmt_type::Print::new(value))
    }

    pub fn new_expr_stmt(value: Expr) -> Self {
        Self::ExprStmt(stmt_type::ExprStmt::new(value))
    }

    pub fn new_var_decl(name: Token, initializer: Option<Expr>) -> Self {
        Self::VarDecl(stmt_type::VarDecl::new(name, initializer))
    }
}

pub mod stmt_type {
    use super::*;
    #[derive(Debug, Clone)]
    pub struct Print(Expr);
    impl Print {
        pub fn new(value: Expr) -> Self {
            Self(value)
        }

        pub fn value(&self) -> &Expr {
            &self.0
        }
    }

    #[derive(Debug, Clone)]
    pub struct ExprStmt(Expr);
    impl ExprStmt {
        pub fn new(value: Expr) -> Self {
            Self(value)
        }

        pub fn value(&self) -> &Expr {
            &self.0
        }
    }

    #[derive(Clone, Debug)]
    pub struct VarDecl(Token, Option<Expr>);
    impl VarDecl {
        pub fn new(name: Token, initializer: Option<Expr>) -> Self {
            Self(name, initializer)
        }

        pub fn ty(&self) -> Option<&Expr> {
            self.1.as_ref()
        }

        pub fn name(&self) -> &str {
            self.0.lexem()
        }
    }
}
