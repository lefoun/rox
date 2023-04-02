use crate::scanner::{Token, TokenType};
use expr_type::*;

pub trait ExprVisitor {
    type Value;
    type Error;

    fn accept(&mut self, expr: Expr) -> Result<Self::Value, Self::Error>;
    fn visit_binary(&mut self, expr: expr_type::Binary) -> Result<Self::Value, Self::Error>;
    fn visit_grouping(&mut self, expr: expr_type::Grouping) -> Result<Self::Value, Self::Error>;
    fn visit_literal(&mut self, expr: expr_type::Literal) -> Result<Self::Value, Self::Error>;
    fn visit_unary(&mut self, expr: expr_type::Unary) -> Result<Self::Value, Self::Error>;
    fn visit_variable(&mut self, expr: expr_type::Variable) -> Result<Self::Value, Self::Error>;
    fn visit_assignment(&mut self, expr: expr_type::Assignment)
        -> Result<Self::Value, Self::Error>;
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
    Variable(Variable),
    Assignment(Assignment),
}

impl Expr {
    pub fn new_binary(lhs: Expr, op: Token, rhs: Expr) -> Self {
        Self::Binary(expr_type::Binary::new(lhs, op, rhs))
    }

    pub fn new_literal(tkn: Token) -> Self {
        Self::Literal(expr_type::Literal::new(tkn))
    }

    pub fn new_grouping(expr: Expr) -> Self {
        Self::Grouping(expr_type::Grouping::new(expr))
    }

    pub fn new_unary(op: Token, expr: Expr) -> Self {
        Self::Unary(expr_type::Unary::new(op, expr))
    }

    pub fn new_variable(name: &str) -> Self {
        Self::Variable(expr_type::Variable::new(name))
    }

    pub fn new_assignment(name: &str, expr: Expr) -> Self {
        Self::Assignment(expr_type::Assignment::new(name, expr))
    }
}

pub mod expr_type {
    use super::*;
    #[derive(Debug, Clone)]
    pub struct Binary(Box<Expr>, Token, Box<Expr>);
    impl Binary {
        pub fn new(lhs: Expr, op: Token, rhs: Expr) -> Self {
            Self(Box::new(lhs), op, Box::new(rhs))
        }

        pub fn lhs(&self) -> &Expr {
            &self.0
        }

        pub fn rhs(&self) -> &Expr {
            &self.2
        }

        pub fn op_ty(&self) -> TokenType {
            self.1.token_type()
        }

        pub fn operator_lexem(&self) -> &str {
            self.1.lexem()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Grouping(Box<Expr>);
    impl Grouping {
        pub fn new(expr: Expr) -> Self {
            Self(Box::new(expr))
        }

        pub fn ty(&self) -> &Expr {
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

        pub fn lexem(&self) -> &str {
            self.0.lexem()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Unary(Token, Box<Expr>);
    impl Unary {
        pub fn new(op: Token, rhs: Expr) -> Self {
            Self(op, Box::new(rhs))
        }

        pub fn operator_ty(&self) -> TokenType {
            self.0.token_type()
        }

        pub fn expr(&self) -> &Expr {
            &self.1
        }

        pub fn operator_lexem(&self) -> &str {
            self.0.lexem()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Variable(String);
    impl Variable {
        pub fn new(name: &str) -> Self {
            Self(name.to_owned())
        }

        pub fn name(&self) -> &str {
            &self.0
        }
    }

    #[derive(Debug, Clone)]
    pub struct Assignment(String, Box<Expr>);
    impl Assignment {
        pub fn new(name: &str, expr: Expr) -> Self {
            Self(name.to_owned(), Box::new(expr))
        }

        pub fn ty(&self) -> &Expr {
            &self.1
        }

        pub fn name(&self) -> &str {
            &self.0
        }
    }
}
