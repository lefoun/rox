use crate::error::ParseError;
use crate::exprs::{expr_type::*, Expr};
use crate::scanner::{
    Token,
    TokenType::{self, *},
};
use crate::stmts::{stmt_type, Stmt};
use std::iter::Peekable;

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while let Some(next_token) = self.tokens.peek() {
            if next_token.token_type() == TokenType::Eof {
                break;
            }
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    eprintln!("{e}");
                    self.synchronisze();
                }
            }
        }
        statements
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self.tokens.peek();
        if next_token.unwrap().token_type() == TokenType::Let {
            self.tokens.next();
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = if let Some(token) = self.tokens.next() {
            token
        } else {
            return Err(ParseError::UnexpectedEOF);
        };
        if name.token_type() != Identifier {
            return Err(ParseError::ExpectedToken {
                token: format!("Identifier"),
                line: name.line(),
            });
        }
        let mut initializer = None;
        if let Some(token) = self.tokens.peek() {
            if token.token_type() == Equal {
                self.tokens.next();
                initializer = Some(self.expression()?);
            }
        };
        if let Some(token) = self.tokens.next() {
            if token.token_type() == SemiColon {
                Ok(Stmt::VarDecl(stmt_type::VarDecl::new(name, initializer)))
            } else {
                Err(ParseError::ExpectedToken {
                    token: ";".to_string(),
                    line: token.line(),
                })
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self.tokens.peek();
        if next_token.unwrap().token_type() == TokenType::Print {
            self.tokens.next();
            self.print_stmt()
        } else {
            self.statement_expr()
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        let next_token = self.tokens.next();
        if next_token.is_some() && next_token.unwrap().token_type() == TokenType::SemiColon {
            Ok(Stmt::Print(stmt_type::Print::new(expr)))
        } else {
            Err(ParseError::ExpectedToken {
                token: format!(";"),
                line: expr.line(),
            })
        }
    }

    fn statement_expr(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        let next_token = self.tokens.next();
        if next_token.is_some() && next_token.unwrap().token_type() == TokenType::SemiColon {
            Ok(Stmt::ExprStmt(stmt_type::ExprStmt::new(expr)))
        } else {
            Err(ParseError::ExpectedToken {
                token: format!(";"),
                line: expr.line(),
            })
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
                        Some(other) if other.token_type() != Eof => Err(ParseError::UnexpectedToken {
                            token: other.lexem().to_string(),
                            line: other.line(),
                        }),
                        Some(_) /* EOF */| None => Err(ParseError::UnexpectedEOF),
                    }
                }
                Identifier => Ok(Expr::Variable(Variable::new(token))),
                Eof => Err(ParseError::UnexpectedEOF),
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
                Class | Fun | Let | While | If | Print | Return => (),
                _ => self.synchronisze(),
            }
        }
    }
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn eval(&self, expr: &Expr) -> String {
        match &expr {
            Expr::Binary(expr) => self.parenthesize(
                &format!("Binary: {}", expr.op_lexem()),
                std::iter::once(expr.lhs()).chain(std::iter::once(expr.rhs())),
            ),

            Expr::Grouping(exp) => self.parenthesize("group", std::iter::once(exp.ty())),
            Expr::Literal(token) => token.lexem().to_owned(),
            Expr::Unary(expr) => self.parenthesize(
                &format!("Unary :{}", expr.op_lexem()),
                std::iter::once(expr.ty()),
            ),
            Expr::Variable(var) => self.parenthesize(var.name(), std::iter::empty::<&Box<Expr>>()),
        }
    }
    fn parenthesize<'a, T: Iterator<Item = &'a Box<Expr>>>(&self, name: &str, exprs: T) -> String {
        let mut s = String::new();
        s.push('(');
        s.push(' ');
        s.push_str(name);
        for expr in exprs {
            s.push(' ');
            s.push_str(&self.eval(expr));
        }
        s.push(')');
        s
    }
}
