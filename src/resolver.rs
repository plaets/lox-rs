use std::collections::HashMap;
use newtype_enum::Enum;
use crate::ast::{Stmt,Expr,Token,ExprVar,StmtVar};

#[derive(Debug)]
pub struct LocalsMap(pub HashMap<Expr,usize>);

#[derive(Clone, Debug, PartialEq)]
enum FunctionType {
    None,
    Function,
    Method,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    locals: HashMap<Expr,usize>,                //why does this compile and what bugs does this cause
    non_critical_errors: Vec<ResolverError>,
    current_function: FunctionType,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            locals: HashMap::new(),
            non_critical_errors: vec![],
            current_function: FunctionType::None,
        }
    }

    pub fn resolve(&mut self, stmts: &[Stmt]) -> (LocalsMap,Vec<ResolverError>) {
        for n in stmts {
            if let Err(err) = self.resolve_stmt(n) {
                let mut errors = vec![err];
                errors.append(&mut self.non_critical_errors);
                return (self.move_locals_map(), self.non_critical_errors.drain(0..).collect::<Vec<_>>())
            }
        }
        (self.move_locals_map(), self.non_critical_errors.drain(0..).collect::<Vec<_>>())
    }

    fn move_locals_map(&mut self) -> LocalsMap {
        let mut res = HashMap::new();
        std::mem::swap(&mut res, &mut self.locals);
        LocalsMap(res)
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(),ResolverError> {
        match stmt {
            Stmt::ExprStmt(expr) => self.resolve_expr(&expr.expr),
            Stmt::If(stmt) => self.resolve_if(&stmt),
            Stmt::Print(stmt) => self.resolve_expr(&stmt.expr),
            Stmt::Return(stmt) => self.resolve_return(&stmt),
            Stmt::While(stmt) => self.resolve_while(&stmt),
            Stmt::Fun(stmt) => self.resolve_fun(&stmt),
            Stmt::Class(stmt) => self.resolve_class(&stmt),
            Stmt::Var(stmt) => self.resolve_var_stmt(&stmt),
            Stmt::Block(stmt) => self.resolve_block_stmt(&stmt),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(),ResolverError> {
        match expr {
            Expr::Assign(expr) => self.resolve_assign(&expr),
            Expr::Logical(expr) => self.resolve_logical(&expr),
            Expr::Binary(expr) => self.resolve_binary(&expr),
            Expr::Call(expr) => self.resolve_call(&expr),
            Expr::Get(expr) => self.resolve_get(&expr),
            Expr::Set(expr) => self.resolve_set(&expr),
            Expr::This(expr) => self.resolve_this(&expr),
            Expr::Unary(expr) => self.resolve_expr(&expr.expr),
            Expr::Literal(_) => Ok(()),
            Expr::Variable(expr) => self.resolve_variable(&expr),
            Expr::Grouping(expr) => self.resolve_expr(&expr.expr),
        }
    }

    fn resolve_stmts(&mut self, stmts: &[Stmt]) -> Result<(),ResolverError> {
        for n in stmts {
            self.resolve_stmt(n)?;
        }
        Ok(())
    }

    fn resolve_block_stmt(&mut self, stmt: &StmtVar::Block) -> Result<(),ResolverError> {
        self.begin_scope();
        self.resolve_stmts(&stmt.body)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_var_stmt(&mut self, stmt: &StmtVar::Var) -> Result<(),ResolverError> {
        let name = stmt.name.lexeme.clone();
        self.declare(name.clone(), &stmt.name);
        if let Some(init) = &stmt.init {
            self.resolve_expr(init)?;
        }
        self.define(name.clone());
        Ok(())
    }

    fn resolve_fun(&mut self, stmt: &StmtVar::Fun) -> Result<(),ResolverError> {
        self.declare(stmt.stmt.0.lexeme.clone(), &stmt.stmt.0);
        self.define(stmt.stmt.0.lexeme.clone());
        self.resolve_function(stmt, FunctionType::Function)
    }

    fn resolve_class(&mut self, stmt: &StmtVar::Class) -> Result<(),ResolverError> {
        self.declare(stmt.name.lexeme.clone(), &stmt.name);
        self.begin_scope();
        self.scopes.last_mut().unwrap().insert("this".to_string(), true);

        for m in stmt.methods.iter() {
            let decl = FunctionType::Method;
            self.resolve_function(&m, decl)?;
        }

        self.end_scope();
        self.define(stmt.name.lexeme.clone());
        Ok(())
    }

    fn resolve_function(&mut self, stmt: &StmtVar::Fun, function_type: FunctionType) -> Result<(),ResolverError> {
        let enclosing = self.current_function.clone();
        self.current_function = function_type;

        self.begin_scope();
        for param in stmt.stmt.1.iter() {
            self.declare(param.lexeme.clone(), param);
            self.define(param.lexeme.clone());
        }
        self.resolve_stmts(&stmt.stmt.2.body)?;
        self.end_scope();

        self.current_function = enclosing;
        Ok(())
    }

    fn resolve_if(&mut self, stmt: &StmtVar::If) -> Result<(),ResolverError> {
        self.resolve_expr(&stmt.cond)?;
        self.resolve_stmt(&*stmt.then)?;
        if let Some(else_b) = &stmt.else_b {
            self.resolve_stmt(&*else_b)?;
        }
        Ok(())
    }

    fn resolve_return(&mut self, stmt: &StmtVar::Return) -> Result<(),ResolverError> {
        if self.current_function == FunctionType::None {
            self.non_critical_errors.push(ResolverError::new(*stmt.keyword.clone(), 
                                             ResolverErrorReason::ReturnOutsideOfFunction))
        }
        if let Some(value) = &stmt.value {
            self.resolve_expr(value)?;
        }
        Ok(())
    }

    fn resolve_while(&mut self, stmt: &StmtVar::While) -> Result<(),ResolverError> {
        self.resolve_expr(&stmt.cond)?;
        self.resolve_stmt(&*stmt.body)
    }

    fn resolve_variable(&mut self, expr: &ExprVar::Variable) -> Result<(),ResolverError> {
        if let Some(scope) = self.scopes.last() {
            let name = &expr.name.lexeme;
            if scope.get(name).is_some() && !scope.get(name).unwrap() {
                self.non_critical_errors.push(ResolverError::new((*expr.name).clone(),
                    ResolverErrorReason::CantReadInInit(name.to_string())))
            } 
        } 
        Ok(self.resolve_local(&Expr::from_variant(expr.clone()), &expr.name))
    }

    fn resolve_assign(&mut self, expr: &ExprVar::Assign) -> Result<(),ResolverError> {
        self.resolve_expr(&expr.expr)?;
        self.resolve_local(&expr.expr, &expr.name);
        Ok(())
    }

    fn resolve_binary(&mut self, expr: &ExprVar::Binary) -> Result<(),ResolverError> {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)
    }

    fn resolve_logical(&mut self, expr: &ExprVar::Logical) -> Result<(),ResolverError> {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)
    }

    fn resolve_call(&mut self, expr: &ExprVar::Call) -> Result<(),ResolverError> {
        self.resolve_expr(&expr.callee)?;
        for n in expr.args.iter() {
            self.resolve_expr(n)?;
        }
        Ok(())
    }

    fn resolve_get(&mut self, stmt: &ExprVar::Get) -> Result<(),ResolverError> {
        self.resolve_expr(&stmt.object)
    }

    fn resolve_set(&mut self, stmt: &ExprVar::Set) -> Result<(),ResolverError> {
        self.resolve_expr(&stmt.value)?;
        self.resolve_expr(&stmt.object)
    }

    fn resolve_this(&mut self, expr: &ExprVar::This) -> Result<(),ResolverError> {
        self.resolve_local(&Expr::from_variant(expr.clone()), &expr.keyword);
        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for n in (0..self.scopes.len()).rev() {
            if self.scopes[n].contains_key(&name.lexeme.to_string()) {
                self.locals.insert(expr.clone(), self.scopes.len()-1-n);
                return;
            }
        }
    }

    fn declare(&mut self, name: String, err_token: &Token) {
        if let Some(scope) = self.scopes.last_mut() { //TODO: im assuming this is none only if scopes is empty
            if scope.contains_key(&name) {
                self.non_critical_errors.push(ResolverError::new(err_token.clone(), 
                                               ResolverErrorReason::AlreadyExistsInScope(name)))
            } else {
                scope.insert(name, false);
            }
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

#[derive(Clone, Debug)]
pub struct ResolverError {
    token: Token,
    error_type: ResolverErrorReason,
}

impl ResolverError {
    pub fn new(token: Token, error_type: ResolverErrorReason) -> Self {
        Self {
            token,
            error_type,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ResolverErrorReason {
    CantReadInInit(String),
    AlreadyExistsInScope(String),
    ReturnOutsideOfFunction,
}
