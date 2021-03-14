use std::collections::HashMap;
use crate::ast::{Stmt,Expr,Token,ExprVar};
use newtype_enum::Enum;

struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
        }
    }

    fn block(&mut self, stmts: &[Stmt]) {
        self.begin_scope();
        self.resolve_stmts(stmts);
        self.end_scope();
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) {
        for n in stmts {
            self.resolve_stmt(n);
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
    }

    fn resolve_expr(&mut self, expr: &Expr) {
    }

    fn resolve_var_stmt(&mut self, name: &Token, init: &Option<Expr>) {
        self.declare(name.lexeme.clone());
        if let Some(init) = init {
            self.resolve_expr(init);
        }
        self.define(name.lexeme.clone());
    }

    fn resolve_var_expr(&mut self, expr: &ExprVar::Variable) -> Result<(),ResolverError> {
        if let Some(scope) = self.scopes.last() {
            let name = &expr.name.lexeme;
            if !scope.get(name).unwrap() {
                Err(ResolverError::CantReadInInit(name.to_string()))
            } else {
                Ok(self.resolve_local(&Expr::from_variant(expr.clone()), &expr.name))
            }
        } else {
            Ok(())
        }
    }

    fn resolve_assign_expr(&mut self, expr: &ExprVar::Assign) -> Result<(),ResolverError> {
        self.resolve_expr(&expr.expr);
        self.resolve_local(&expr.expr, &expr.name);
        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for n in self.scopes.iter() {
            if n.contains_key(&name.lexeme.to_string()) {
                //self.resolve
            }
        }
    }

    fn declare(&mut self, name: String) {
        if !self.scopes.is_empty() {
            self.scopes.last_mut().unwrap().insert(name, false);
        }
    }

    fn define(&mut self, name: String) {
        if !self.scopes.is_empty() {
            self.scopes.last_mut().unwrap().insert(name, true);
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

enum ResolverError {
    CantReadInInit(String),
    InternalError(String),

}
