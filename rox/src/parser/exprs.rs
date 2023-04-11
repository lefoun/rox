use crate::scanner::scanner::{Token, TokenType};
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
    fn visit_call(&mut self, expr: expr_type::Call) -> Result<Self::Value, Self::Error>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
    Variable(Variable),
    Assignment(Assignment),
    Call(Call),
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

    pub fn new_call(callee: Expr, left_paren: Token, args: Vec<Expr>) -> Self {
        Self::Call(expr_type::Call::new(callee, left_paren, args))
    }
}

pub mod expr_type {
    use super::*;
    #[derive(Debug, Clone, PartialEq)]
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

        pub fn operator_lexeme(&self) -> &str {
            self.1.lexeme()
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Grouping(Box<Expr>);
    impl Grouping {
        pub fn new(expr: Expr) -> Self {
            Self(Box::new(expr))
        }

        pub fn ty(&self) -> &Expr {
            &self.0
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Literal(Token);
    impl Literal {
        pub fn new(token: Token) -> Self {
            Self(token)
        }

        pub fn ty(&self) -> TokenType {
            self.0.token_type()
        }

        pub fn lexeme(&self) -> &str {
            self.0.lexeme()
        }
    }

    #[derive(Debug, Clone, PartialEq)]
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

        pub fn operator_lexeme(&self) -> &str {
            self.0.lexeme()
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Variable(String);
    impl Variable {
        pub fn new(name: &str) -> Self {
            Self(name.to_owned())
        }

        pub fn name(&self) -> &str {
            &self.0
        }
    }

    #[derive(Debug, Clone, PartialEq)]
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

    #[derive(Debug, Clone, PartialEq)]
    pub struct Call(Box<Expr>, Token, Vec<Expr>);
    impl Call {
        pub fn new(callee: Expr, left_paren: Token, args: Vec<Expr>) -> Self {
            Self(Box::new(callee), left_paren, args)
        }

        pub fn callee(&self) -> &Expr {
            &self.0
        }

        pub fn paren(&self) -> &Token {
            &self.1
        }

        pub fn args(&self) -> &Vec<Expr> {
            &self.2
        }
    }
}
