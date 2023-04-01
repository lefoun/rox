use crate::scanner::{Token, TokenType};
use expr_type::*;

pub trait ExprVisitor {
    type Value;
    type Error;

    fn accept(&self, expr: Expr) -> Result<Self::Value, Self::Error>;
    fn visit_binary(&self, expr: expr_type::Binary) -> Result<Self::Value, Self::Error>;
    fn visit_grouping(&self, expr: expr_type::Grouping) -> Result<Self::Value, Self::Error>;
    fn visit_literal(&self, expr: expr_type::Literal) -> Result<Self::Value, Self::Error>;
    fn visit_unary(&self, expr: expr_type::Unary) -> Result<Self::Value, Self::Error>;
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
}

pub mod expr_type {
    use super::*;
    #[derive(Debug, Clone)]
    pub struct Binary(Box<Expr>, Token, Box<Expr>);
    impl Binary {
        pub fn new(lhs: Box<Expr>, op: Token, rhs: Box<Expr>) -> Self {
            Self(lhs, op, rhs)
        }

        pub fn lhs(&self) -> &Box<Expr> {
            &self.0
        }

        pub fn rhs(&self) -> &Box<Expr> {
            &self.2
        }

        pub fn op_ty(&self) -> TokenType {
            self.1.token_type()
        }

        pub fn op_lexem(&self) -> &str {
            self.1.lexem()
        }

        pub fn line(&self) -> usize {
            self.1.line()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Grouping(Box<Expr>);
    impl Grouping {
        pub fn new(expr: Box<Expr>) -> Self {
            Self(expr)
        }

        pub fn ty(&self) -> &Box<Expr> {
            &self.0
        }
    }

    #[derive(Clone, Debug)]
    pub struct Literal(Token);
    impl Literal {
        pub fn new(token: Token) -> Self {
            Self(token)
        }

        pub fn ty(&self) -> TokenType {
            self.0.token_type()
        }

        pub fn line(&self) -> usize {
            self.0.line()
        }

        pub fn lexem(&self) -> &str {
            self.0.lexem()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Unary(Token, Box<Expr>);
    impl Unary {
        pub fn new(op: Token, rhs: Box<Expr>) -> Self {
            Self(op, rhs)
        }

        pub fn op_ty(&self) -> TokenType {
            self.0.token_type()
        }

        pub fn ty(&self) -> &Box<Expr> {
            &self.1
        }

        pub fn op_lexem(&self) -> &str {
            self.0.lexem()
        }

        pub fn line(&self) -> usize {
            self.0.line()
        }
    }
}
