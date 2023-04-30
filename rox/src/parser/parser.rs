use super::error::ParseError;
use super::exprs::Expr;
use super::stmts::{stmt_type, Stmt};
use crate::scanner::scanner::{
    Token,
    TokenType::{self, *},
};
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
                token: "Identifier".to_string(),
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
                token: ";".to_string(),
            })
        }
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.next_matches(&[LeftBrace])? {
            Ok(Stmt::new_block(self.block()?))
        } else if self.next_matches(&[If])? {
            self.if_stmt()
        } else if self.next_matches(&[While])? {
            self.while_loop_stmt()
        } else if self.next_matches(&[For])? {
            self.for_loop_stmt()
        } else if self.next_matches(&[Fn])? {
            self.function_decl("function")
        } else if self.next_matches(&[Return])? {
            self.return_stmt()
        } else {
            self.statement_expr()
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        if self.next_matches(&[SemiColon])? {
            Ok(Stmt::new_return_stmt(None))
        } else {
            let ret = Ok(Stmt::new_return_stmt(Some(self.expression()?)));
            self.consume_next(SemiColon, Some(";"))?;
            ret
        }
    }

    fn function_decl(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self.consume_next(Identifier, Some(kind))?;
        self.next_matches_or_error(LeftParen, "(")?;
        let mut params = Vec::new();
        loop {
            if params.len() >= 255 {
                return Err(ParseError::MaxNbArgumentsReached);
            }
            if self.next_matches(&[RightParen])? {
                break;
            } else {
                params.push(self.consume_next(Identifier, Some("parameter"))?);
                if self.next_matches(&[Comma])? {
                    continue;
                } else if self.next_matches(&[RightParen])? {
                    break;
                } else {
                    return Err(ParseError::ExpectedToken {
                        token: "expected a parameter or a ')'".to_string(),
                    });
                }
            }
        }
        let mut body = Vec::new();
        self.next_matches_or_error(LeftBrace, "{")?;
        while !self.next_matches(&[RightBrace])? {
            body.push(self.declaration()?);
        }

        Ok(Stmt::new_function_decl(name, params, body))
    }

    fn while_loop_stmt(&mut self) -> Result<Stmt, ParseError> {
        let condition = self.expression()?;
        let body = if self.next_matches(&[LeftBrace])? {
            stmt_type::Block::new(self.block()?)
        } else {
            return Err(ParseError::ExpectedToken {
                token: "{".to_string(),
            });
        };
        Ok(Stmt::new_while_loop(condition, body))
    }

    fn for_loop_stmt(&mut self) -> Result<Stmt, ParseError> {
        /*
         * this functions desugares a for loop into a block
         * which contains an optional variable declaration, a while loop, and
         * an optional expression (usually an incrementer)
         * the loop looks like this: for (var i = 0; i < 10; i++) {}
         */
        let mut block = Vec::new();
        self.next_matches_or_error(LeftParen, "(")?;
        if self.next_matches(&[Let])? {
            let initializer = self.var_declaration()?;
            block.push(initializer);
        }
        let condition = self.expression()?;
        let mut end_of_iteration_expr = None;
        self.next_matches_or_error(SemiColon, ";")?;
        if !self.next_matches(&[RightParen])? {
            end_of_iteration_expr = Some(Stmt::new_expr_stmt(self.expression()?));
        }
        self.next_matches_or_error(RightParen, ")")?;
        self.next_matches_or_error(LeftBrace, "{")?;
        let mut body = self.block()?;
        if let Some(expr) = end_of_iteration_expr {
            body.push(expr)
        }
        let loop_stmt = Stmt::new_while_loop(condition, stmt_type::Block::new(body));
        block.push(loop_stmt);
        Ok(Stmt::new_block(block))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        if !self.next_matches(&[LeftBrace])? {
            return Err(ParseError::ExpectedToken {
                token: "{".to_string(),
            });
        }
        let then_branch = Stmt::new_block(self.block()?);
        if self.next_matches(&[Else])? {
            if !self.next_matches(&[LeftBrace])? {
                return Err(ParseError::ExpectedToken {
                    token: "{".to_string(),
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
        while !self.next_matches(&[RightBrace])? {
            stmts.push(self.declaration()?);
        }

        Ok(stmts)
    }

    fn statement_expr(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        match self.next_matches(&[SemiColon]) {
            Ok(true) => Ok(Stmt::new_expr_stmt(expr)),
            Ok(false) if Self::matches(self.tokens.peek().unwrap(), &[Eof]) && self.repl_mode => {
                Ok(Stmt::new_print_repl(expr))
            }
            Ok(false) => Err(ParseError::ExpectedToken {
                token: ";".to_string(),
            }),

            Err(e) => Err(e),
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;
        if self.next_matches(&[Equal])? {
            let value = self.assignment()?;
            let var = match &expr {
                Expr::Variable(v) => v.name(),
                _ => {
                    return Err(ParseError::BadOperand {
                        msg: "Bad target for assignment operator".to_string(),
                    })
                }
            };
            return Ok(Expr::new_assignment(var, value));
        }
        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let left = self.logical_and()?;
        if let Some(operator) = self.tokens.next_if(|token| Self::matches(token, &[Or])) {
            let right = self.logical_and()?;
            Ok(Expr::new_binary(left, operator, right))
        } else {
            Ok(left)
        }
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let left = self.equality()?;
        if let Some(operator) = self.tokens.next_if(|token| Self::matches(token, &[And])) {
            let right = self.equality()?;
            Ok(Expr::new_binary(left, operator, right))
        } else {
            Ok(left)
        }
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
            .next_if(|token| Self::matches(token, &[Star, Slash, Percent]))
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
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        if let Some(token) = self
            .tokens
            .next_if(|token| Self::matches(token, &[LeftParen]))
        {
            expr = self.finish_call(expr, token)?;
        }
        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr, left_paren: Token) -> Result<Expr, ParseError> {
        let mut args = Vec::new();
        loop {
            if let Some(token) = self.tokens.peek() {
                if args.len() >= 255 {
                    return Err(ParseError::MaxNbArgumentsReached);
                }
                if token.token_type() == RightParen {
                    self.tokens.next();
                    break;
                } else {
                    args.push(self.expression()?);
                    if self.next_matches(&[Comma])? {
                        continue;
                    } else if self.next_matches(&[RightParen])? {
                        break;
                    } else {
                        return Err(ParseError::ExpectedToken {
                            token: "expected a parameter or a ')'".to_string(),
                        });
                    }
                }
            } else {
                return Err(ParseError::UnexpectedEOF);
            }
        }
        Ok(Expr::new_call(expr, left_paren, args))
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
                                token: other.lexeme().to_string(),
                            })
                        }
                        Some(_) | None => Err(ParseError::UnexpectedEOF),
                    }
                }
                Identifier => Ok(Expr::new_variable(token.lexeme())),
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

    fn next_matches_or_error(&mut self, ty: TokenType, lexeme: &str) -> Result<(), ParseError> {
        let token = self.tokens.peek();
        if token.is_none() {
            return Err(ParseError::UnexpectedEOF);
        }
        if ty == token.unwrap().token_type() {
            self.tokens.next();
            Ok(())
        } else {
            Err(ParseError::ExpectedToken {
                token: lexeme.to_string(),
            })
        }
    }

    fn consume_next(
        &mut self,
        ty: TokenType,
        expect_lexeme: Option<&str>,
    ) -> Result<Token, ParseError> {
        let token = self.tokens.next();
        if token.is_none() {
            return Err(ParseError::UnexpectedEOF);
        }
        let token = token.unwrap();
        if ty == token.token_type() {
            Ok(token)
        } else if expect_lexeme.is_some() {
            Err(ParseError::ExpectedToken {
                token: expect_lexeme.unwrap().to_string(),
            })
        } else {
            Err(ParseError::UnexpectedToken {
                token: token.to_string(),
            })
        }
    }

    fn synchronisze(&mut self) {
        while let Some(token) = self.tokens.next() {
            match token.token_type() {
                Class | Fn | Let | While | If | Return => (),
                _ => self.synchronisze(),
            }
        }
    }
}
