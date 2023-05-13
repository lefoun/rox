use crate::{
    parser::{
        self,
        error::ParseError,
        exprs::{self, Expr, ExprVisitor},
        stmts::{self, Stmt, StmtVisitor},
    },
    scanner::scanner::Token,
};
use std::collections::HashMap;

use super::interpreter::Interpreter;

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
        }
    }

    pub fn resolve(&mut self, stmts: Vec<Stmt>) -> Result<(), ParseError> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: Stmt) -> Result<(), ParseError> {
        <Self as StmtVisitor>::accept(self, stmt)
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, false);
        }
    }

    pub fn define(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }

    pub fn resolve_local_var(&mut self, name: &str, expr: Expr) {
        let mut nb_scopes = 0;
        for scope in self.scopes.iter().rev() {
            if scope.get(name).is_some() {
                // dbg!(format!("found {} at {} {:?}", name, nb_scopes, expr));
                self.interpreter.resolve(expr, nb_scopes);
                return;
            }
            nb_scopes += 1;
        }
    }
}

impl<'a> ExprVisitor for Resolver<'a> {
    type Error = parser::error::ParseError;
    type Value = ();

    fn accept(&mut self, expr: crate::parser::exprs::Expr) -> Result<Self::Value, Self::Error> {
        match expr {
            Expr::Assignment(expr) => self.visit_assignment(expr),
            Expr::Binary(expr) => self.visit_binary(expr),
            Expr::Call(expr) => self.visit_call(expr),
            Expr::Grouping(expr) => self.visit_grouping(expr),
            Expr::Literal(expr) => self.visit_literal(expr),
            Expr::Unary(expr) => self.visit_unary(expr),
            Expr::Variable(expr) => self.visit_variable(expr),
        }
    }

    fn visit_assignment(
        &mut self,
        expr: exprs::expr_type::Assignment,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, expr.ty().clone())?;
        let name = expr.name().to_owned();
        let expr = Expr::Assignment(expr);
        self.resolve_local_var(name.as_str(), expr);
        Ok(())
    }

    fn visit_binary(
        &mut self,
        expr: crate::parser::exprs::expr_type::Binary,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, expr.lhs().clone())?;
        <Self as ExprVisitor>::accept(self, expr.rhs().clone())?;
        Ok(())
    }

    fn visit_call(
        &mut self,
        expr: crate::parser::exprs::expr_type::Call,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, expr.callee().clone())?;
        for arg in expr.args() {
            <Self as ExprVisitor>::accept(self, arg.clone())?;
        }
        Ok(())
    }

    fn visit_grouping(
        &mut self,
        expr: crate::parser::exprs::expr_type::Grouping,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, expr.ty().clone())
    }

    fn visit_literal(
        &mut self,
        _expr: crate::parser::exprs::expr_type::Literal,
    ) -> Result<Self::Value, Self::Error> {
        Ok(())
    }

    fn visit_unary(
        &mut self,
        expr: crate::parser::exprs::expr_type::Unary,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, expr.expr().clone())
    }

    fn visit_variable(
        &mut self,
        expr: crate::parser::exprs::expr_type::Variable,
    ) -> Result<Self::Value, Self::Error> {
        if self
            .scopes
            .last()
            .and_then(|s| s.get(expr.name()))
            .is_some_and(|v| *v == false)
        {
            return Err(Self::Error::ReadVarInInit);
        }
        let name = expr.name().to_owned();
        let expr = Expr::Variable(expr);
        self.resolve_local_var(name.as_str(), expr);
        Ok(())
    }
}

impl<'a> StmtVisitor for Resolver<'a> {
    type Error = ParseError;
    type Value = ();
    fn accept(&mut self, stmt: crate::parser::stmts::Stmt) -> Result<Self::Value, Self::Error> {
        match stmt {
            Stmt::ReplPrint(stmt) => self.visit_repl_print(stmt),
            Stmt::ExprStmt(stmt) => self.visit_expr_stmt(stmt),
            Stmt::VarDecl(stmt) => self.visit_var_stmt(stmt),
            Stmt::Block(stmt) => self.visit_block(stmt),
            Stmt::IfStmt(stmt) => self.visit_if_stmt(stmt),
            Stmt::WhileLoop(stmt) => self.visit_while_loop(stmt),
            Stmt::FunctionDecl(stmt) => self.visit_function_decl(stmt),
            Stmt::ReturnStmt(stmt) => self.visit_return_stmt(stmt),
        }
    }

    fn visit_block(&mut self, stmt: stmts::stmt_type::Block) -> Result<Self::Value, Self::Error> {
        self.begin_scope();
        for stmt in stmt.stmts() {
            <Self as StmtVisitor>::accept(self, stmt.clone())?;
        }
        self.end_scope();
        Ok(())
    }

    fn visit_expr_stmt(
        &mut self,
        stmt: stmts::stmt_type::ExprStmt,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, stmt.value().clone())
    }

    fn visit_function_decl(
        &mut self,
        stmt: stmts::stmt_type::FunctionDecl,
    ) -> Result<Self::Value, Self::Error> {
        self.declare(stmt.name().to_string());
        self.define(stmt.name().to_string());

        self.begin_scope();
        for param in stmt.params() {
            self.declare(param.to_string());
            self.define(param.to_string());
        }
        for stmt in stmt.body() {
            <Self as StmtVisitor>::accept(self, stmt.clone())?;
        }
        self.end_scope();
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        stmt: stmts::stmt_type::IfStmt,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, stmt.condition().clone())?;
        <Self as StmtVisitor>::accept(self, stmt.then_branch().clone())?;
        if let Some(else_branch) = stmt.else_branch() {
            <Self as StmtVisitor>::accept(self, else_branch.to_owned())?;
        }
        Ok(())
    }

    fn visit_repl_print(
        &mut self,
        stmt: stmts::stmt_type::ReplPrint,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, stmt.value().clone())
    }

    fn visit_return_stmt(
        &mut self,
        stmt: stmts::stmt_type::ReturnStmt,
    ) -> Result<Self::Value, Self::Error> {
        if let Some(stm) = stmt.value() {
            <Self as ExprVisitor>::accept(self, stm.clone())?;
        }
        Ok(())
    }

    fn visit_var_stmt(
        &mut self,
        stmt: stmts::stmt_type::VarDecl,
    ) -> Result<Self::Value, Self::Error> {
        let name = stmt.name().to_owned();
        self.declare(name.clone());
        if let Some(expr) = stmt.ty() {
            <Self as ExprVisitor>::accept(self, expr.clone())?;
        }
        self.define(name);
        Ok(())
    }

    fn visit_while_loop(
        &mut self,
        stmt: stmts::stmt_type::WhileLoop,
    ) -> Result<Self::Value, Self::Error> {
        <Self as ExprVisitor>::accept(self, stmt.condition().clone())?;
        for stm in stmt.body().stmts() {
            <Self as StmtVisitor>::accept(self, stm.clone())?;
        }
        Ok(())
    }
}
