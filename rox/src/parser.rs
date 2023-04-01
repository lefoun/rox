use crate::error::ParseError;
use crate::expr::{
    expr_type::{self, *},
    Expr,
};
use crate::scanner::{
    Token,
    TokenType::{self, *},
};
use std::iter::Peekable;

pub trait Visitor {
    type Value;
    type Error;
    fn accept(&self, expr: Expr) -> Result<Self::Value, Self::Error>;
    fn visit_binary(&self, expr: expr_type::Binary) -> Result<Self::Value, Self::Error>;
    fn visit_grouping(&self, expr: expr_type::Grouping) -> Result<Self::Value, Self::Error>;
    fn visit_literal(&self, expr: expr_type::Literal) -> Result<Self::Value, Self::Error>;
    fn visit_unary(&self, expr: expr_type::Unary) -> Result<Self::Value, Self::Error>;
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
            .next_if(|token| Self::matches(token, &[DoubleEqual, BangEqual]))
        {
            let right = self.comparison()?;
            expr = Expr::Binary(Binary::new(Box::new(expr), operator, Box::new(right)));
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
            expr = Expr::Binary(Binary::new(Box::new(expr), operator, Box::new(right)));
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
            expr = Expr::Binary(Binary::new(Box::new(expr), op, Box::new(right)));
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
            expr = Expr::Binary(Binary::new(Box::new(expr), op, Box::new(right)));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = self
            .tokens
            .next_if(|token| Self::matches(token, &[Minus, Bang]))
        {
            let expr = self.unary()?;
            Ok(Expr::Unary(Unary::new(op, Box::new(expr))))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.next() {
            match token.token_type() {
                True | False | Number(_) | RoxString(_) | Null => {
                    Ok(Expr::Literal(Literal::new(token)))
                }
                LeftParen => {
                    let expr = self.expression()?;
                    match self.tokens.next() {
                        Some(tkn) if tkn.token_type() == RightParen => {
                            Ok(Expr::Grouping(Grouping::new(Box::new(expr))))
                        }
                        Some(other) => Err(ParseError::UnexpectedToken {
                            token: other.lexem().to_string(),
                            line: other.line(),
                        }),
                        None => Err(ParseError::UnexpectedEOF),
                    }
                }
                _ => Err(ParseError::UnexpectedToken {
                    token: token.to_string(),
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

// struct AstPrinter;

// impl AstPrinter {
//     fn eval(&self, expr: &Expr) -> String {
//         match &expr {
//             Expr::Binary(Binary(lhs, op, rhs)) => {
//                 self.parenthesize(op.lexem(), iter::once(lhs).chain(iter::once(rhs)))
//             }
//             Expr::Grouping(Grouping(exp)) => self.parenthesize("group", iter::once(exp)),
//             Expr::Literal(Literal(token)) => token.lexem().to_owned(),
//             Expr::Unary(Unary(token, exp)) => self.parenthesize(token.lexem(), iter::once(exp)),
//         }
//     }
//     fn parenthesize<'a, T: Iterator<Item = &'a Box<Expr>>>(&self, name: &str, exprs: T) -> String {
//         let mut s = String::new();
//         s.push('(');
//         s.push(' ');
//         s.push_str(name);
//         for expr in exprs {
//             s.push(' ');
//             s.push_str(&self.eval(expr));
//         }
//         s.push(')');
//         s
//     }
// }

// mod test {
//     use super::{AstPrinter, Expr, TokenType, Binary, Unary, Grouping, Literal};
//     use crate::scanner::Token;

//     #[test]
//     fn it_prints() {
//         use Expr::*;
//         use TokenType::*;
//         let printer = AstPrinter;
//         let expr = Binary(Binary(
//             Box::new(Unary(Unary(
//                 Token::new(Minus, 0, String::from("-")),
//                 Box::new(Literal(Literal(Token::new(Number(43.2), 0, String::from("43.2"))))),
//             ))),
//             Token::new(Star, 0, String::from("*")),
//             Box::new(Grouping(Grouping(Box::new(Literal(Literal(Token::new(
//                 Number(42.0),
//                 0,
//                 String::from("42.0"),
//             ))))))),
//         );

//         println!("Ast printed: {}", printer.eval(&expr));
//     }
// }
