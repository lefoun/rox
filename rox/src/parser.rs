use crate::error::ParseError;
use crate::scanner::{Token, TokenType};
use std::iter::{self, Peekable};
use TokenType::*;

pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Token),
    Unary(Token, Box<Expr>),
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        match self.expression() {
            Ok(expr) => Some(expr),
            Err(e) => {
                eprintln!("{e}");
                None
            }
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while let Some(operator) = self
            .tokens
            .next_if(|token| Self::matches(token, &[Equal, BangEqual]))
        {
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;
        while let Some(operator) = self
            .tokens
            .next_if(|token| Self::matches(token, &[Less, LessEqual, Greater, GreaterEqual]))
        {
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while let Some(op) = self
            .tokens
            .next_if(|token| Self::matches(token, &[Plus, Minus]))
        {
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while let Some(op) = self
            .tokens
            .next_if(|token| Self::matches(token, &[Star, Slash]))
        {
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), op, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = self
            .tokens
            .next_if(|token| Self::matches(token, &[Minus, Bang]))
        {
            let expr = self.unary()?;
            Ok(Expr::Unary(op, Box::new(expr)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.next() {
            match token.token_type() {
                True | False | Number(_) | RoxString(_) | Null => Ok(Expr::Literal(token)),
                LeftParen => {
                    let expr = self.expression()?;
                    match self.tokens.next() {
                        Some(tkn) if tkn.token_type() == RightParen => {
                            Ok(Expr::Grouping(Box::new(expr)))
                        }
                        Some(other) => Err(ParseError::UnexpectedToken {
                            token: other.lexem().to_string(),
                            line: other.line(),
                        }),
                        None => Err(ParseError::UnexpectedEOF),
                    }
                }
                other => Err(ParseError::UnexpectedToken {
                    token: format!("{:?}", other),
                    line: token.line(),
                }),
            }
        } else {
            Err(ParseError::UnexpectedEOF {})
        }
    }

    fn matches(token: &Token, typs: &[TokenType]) -> bool {
        typs.contains(&token.token_type())
    }

    fn synchronisze(&mut self) {
        while let Some(token) = self.tokens.next() {
            match token.token_type() {
                Class | Fun | Var | While | If | Print | Return => (),
                _ => self.synchronisze(),
            }
        }
    }
}

pub trait Visitor {
    type Value;
    fn visit_expr(&self, expr: &Expr) -> Self::Value;
}

pub struct AstPrinter;

impl Visitor for AstPrinter {
    type Value = String;
    fn visit_expr(&self, expr: &Expr) -> Self::Value {
        match &expr {
            Expr::Binary(lhs, op, rhs) => {
                self.parenthesize(op.lexem(), iter::once(lhs).chain(iter::once(rhs)))
            }
            Expr::Grouping(exp) => self.parenthesize("group", iter::once(exp)),
            Expr::Literal(token) => token.lexem().to_owned(),
            Expr::Unary(token, exp) => self.parenthesize(token.lexem(), iter::once(exp)),
        }
    }
}

impl AstPrinter {
    fn parenthesize<'a, T: Iterator<Item = &'a Box<Expr>>>(&self, name: &str, exprs: T) -> String {
        let mut s = String::new();
        s.push('(');
        s.push(' ');
        s.push_str(name);
        for expr in exprs {
            s.push(' ');
            s.push_str(&self.visit_expr(expr));
        }
        s.push(')');
        s
    }
}

impl<T: Iterator<Item = Token>> Visitor for Parser<T> {
    type Value = Token;
    fn visit_expr(&self, expr: &Expr) -> Self::Value {
        match &expr {
            Expr::Binary(lhs, op, rhs) => (),
            Expr::Grouping(e) => (),
            Expr::Literal(t) => (),
            Expr::Unary(t, op) => (),
        };
        Token::new(TokenType::And, 2, String::from("lol"))
    }
}

mod test {
    use super::{AstPrinter, Expr, TokenType};
    use crate::{parser::Visitor, scanner::Token};

    #[test]
    fn it_prints() {
        use Expr::*;
        use TokenType::*;
        let printer = AstPrinter;
        let expr = Binary(
            Box::new(Unary(
                Token::new(Minus, 0, String::from("-")),
                Box::new(Literal(Token::new(Number(43.2), 0, String::from("43.2")))),
            )),
            Token::new(Star, 0, String::from("*")),
            Box::new(Grouping(Box::new(Literal(Token::new(
                Number(42.0),
                0,
                String::from("42.0"),
            ))))),
        );

        println!("Ast printed: {}", printer.visit_expr(&expr));
    }
}
