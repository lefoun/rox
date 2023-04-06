use crate::error::ParseError;
use crate::exprs::Expr;
use crate::scanner::{
    Token,
    TokenType::{self, *},
};
use crate::stmts::Stmt;
use std::iter::Peekable;

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    repl_mode: bool,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T, repl_mode: bool) -> Self {
        Self {
            tokens: tokens.peekable(),
            repl_mode,
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while let Some(next_token) = self.tokens.peek() {
            if next_token.token_type() == Eof {
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
        if self.next_matches(&[Let])? {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = if let Some(token) = self.tokens.next() {
            token
        } else {
            return Err(ParseError::ExpectedToken {
                token: format!("Identifier"),
            });
        };
        let mut initializer = None;
        if self.next_matches(&[Equal])? {
            initializer = Some(self.expression()?);
        }
        if self.next_matches(&[SemiColon])? {
            Ok(Stmt::new_var_decl(name, initializer))
        } else {
            Err(ParseError::ExpectedToken {
                token: format!(";"),
            })
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.next_matches(&[Print])? {
            self.print_stmt()
        } else if self.next_matches(&[LeftBrace])? {
            Ok(Stmt::new_block(self.block()?))
        } else if self.next_matches(&[If])? {
            self.if_stmt()
        } else {
            self.statement_expr()
        }
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        if !self.next_matches(&[LeftBrace])? {
            return Err(ParseError::ExpectedToken {
                token: format!("{{"),
            });
        }
        let then_branch = Stmt::new_block(self.block()?);
        if self.next_matches(&[Else])? {
            if !self.next_matches(&[LeftBrace])? {
                return Err(ParseError::ExpectedToken {
                    token: format!("{{"),
                });
            }
            let else_branch = Stmt::new_block(self.block()?);
            Ok(Stmt::new_if_stmt(expr, then_branch, Some(else_branch)))
        } else {
            Ok(Stmt::new_if_stmt(expr, then_branch, None))
        }
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !self.is_at_end() && !self.next_matches(&[RightBrace])? {
            stmts.push(self.declaration()?);
        }

        Ok(stmts)
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        if self.next_matches(&[SemiColon])? {
            Ok(Stmt::new_print(expr))
        } else {
            Err(ParseError::ExpectedToken {
                token: format!(";"),
            })
        }
    }

    fn statement_expr(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;

        match self.next_matches(&[SemiColon]) {
            Ok(true) => Ok(Stmt::new_expr_stmt(expr)),
            Ok(false) if Self::matches(self.tokens.peek().unwrap(), &[Eof]) && self.repl_mode => {
                Ok(Stmt::new_print(expr))
            }
            Ok(false) => Err(ParseError::ExpectedToken {
                token: format!(";"),
            }),

            Err(e) => Err(e),
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality()?;
        if self.next_matches(&[Equal])? {
            let value = self.assignment()?;
            let var = match &expr {
                Expr::Variable(v) => v.name(),
                _ => {
                    return Err(ParseError::BadOperand {
                        msg: format!("Bad target for assignment operator"),
                    })
                }
            };
            return Ok(Expr::new_assignment(var, value));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while let Some(operator) = self
            .tokens
            .next_if(|token| Self::matches(token, &[DoubleEqual, BangEqual]))
        {
            let right = self.comparison()?;
            expr = Expr::new_binary(expr, operator, right);
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
            expr = Expr::new_binary(expr, operator, right);
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
            expr = Expr::new_binary(expr, op, right);
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
            expr = Expr::new_binary(expr, op, right);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = self
            .tokens
            .next_if(|token| Self::matches(token, &[Minus, Bang]))
        {
            let expr = self.unary()?;
            Ok(Expr::new_unary(op, expr))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(token) = self.tokens.next() {
            match token.token_type() {
                True | False | Number(_) | RoxString(_) | Null => Ok(Expr::new_literal(token)),
                LeftParen => {
                    let expr = self.expression()?;
                    match self.tokens.next() {
                        Some(tkn) if tkn.token_type() == RightParen => Ok(Expr::new_grouping(expr)),
                        Some(other) if other.token_type() != Eof => {
                            Err(ParseError::UnexpectedToken {
                                token: other.lexem().to_string(),
                            })
                        }
                        Some(_) | None => Err(ParseError::UnexpectedEOF),
                    }
                }
                Identifier => Ok(Expr::new_variable(token.lexem())),
                Eof => Err(ParseError::UnexpectedEOF),
                _ => Err(ParseError::UnexpectedToken {
                    token: token.to_string(),
                }),
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    fn matches(token: &Token, typs: &[TokenType]) -> bool {
        typs.contains(&token.token_type())
    }

    fn next_matches(&mut self, typs: &[TokenType]) -> Result<bool, ParseError> {
        let token = self.tokens.peek();
        if token.is_none() {
            return Err(ParseError::UnexpectedEOF);
        }
        if typs.contains(&token.unwrap().token_type()) {
            self.tokens.next();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.tokens
            .peek()
            .map_or(true, |token| token.token_type() == Eof)
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
        match expr {
            Expr::Binary(expr) => self.parenthesize(
                &format!("Binary: {}", expr.operator_lexem()),
                std::iter::once(expr.lhs()).chain(std::iter::once(expr.rhs())),
            ),

            Expr::Grouping(exp) => self.parenthesize("group", std::iter::once(exp.ty())),
            Expr::Literal(token) => token.lexem().to_owned(),
            Expr::Unary(expr) => self.parenthesize(
                &format!("Unary :{}", expr.operator_lexem()),
                std::iter::once(expr.expr()),
            ),
            Expr::Variable(var) => self.parenthesize(var.name(), std::iter::empty::<&Expr>()),
            Expr::Assignment(expr) => self.parenthesize(expr.name(), std::iter::once(expr.ty())),
        }
    }
    fn parenthesize<'a, T: Iterator<Item = &'a Expr>>(&self, name: &str, exprs: T) -> String {
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
