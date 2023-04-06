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
    fn visit_block(&mut self, stmt: stmt_type::Block) -> Result<Self::Value, Self::Error>;
    fn visit_if_stmt(&mut self, stmt: stmt_type::IfStmt) -> Result<Self::Value, Self::Error>;
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(Print),
    ExprStmt(ExprStmt),
    VarDecl(VarDecl),
    Block(Block),
    IfStmt(IfStmt),
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

    pub fn new_block(stmts: Vec<Stmt>) -> Self {
        Self::Block(stmt_type::Block::new(stmts))
    }

    pub fn new_if_stmt(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Self::IfStmt(stmt_type::IfStmt::new(condition, then_branch, else_branch))
    }
}

pub mod stmt_type {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>);
    impl IfStmt {
        pub fn new(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
            Self(condition, Box::new(then_branch), else_branch.map(Box::new))
        }

        pub fn condition(&self) -> &Expr {
            &self.0
        }

        pub fn then_branch(&self) -> &Stmt {
            &*self.1
        }

        pub fn else_branch(&self) -> Option<&Stmt> {
            if let Some(else_branch) = &self.2 {
                Some(&*else_branch)
            } else {
                None
            }
        }
    }

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

    #[derive(Clone, Debug)]
    pub struct Block(Vec<Stmt>);
    impl Block {
        pub fn new(stmts: Vec<Stmt>) -> Self {
            Self(stmts)
        }

        pub fn stmts(&self) -> &[Stmt] {
            &self.0
        }
    }
}
