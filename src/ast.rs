use std::fmt;
use std::hash::{Hash,Hasher};
use gcmodule::{Cc,Trace,Tracer};
use strum_macros::EnumDiscriminants;
use newtype_enum::{newtype_enum};

//is this enum necessary
#[enumeration(rename_all = "snake_case")]
#[derive(Debug, Clone, PartialEq, Eq, Hash, enum_utils::FromStr)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumDiscriminants)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,

    Bang,
    BangEqual,
    EqualEqual,
    Equal,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,

    String(String),             //TODO: how to handle invlaid variants like TokenType::String(Object::Number)?
    Number(String),             //f64 cant be hashed also this allows us to change object implementaion without touching ast
    Keyword(Keyword),
    Identifier(String),

    Slash,
    Eof,
}

#[derive(PartialEq,Eq,Clone,Debug,Hash)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String, //should i even use lexeme when im already storing strings in the enum variants?
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.token_type, self.lexeme)
    }
}

#[derive(Debug,Clone,PartialEq,Hash)]
pub struct FunctionStmt(pub Box<Token>, pub Vec<Token>, pub StmtVar::Block);     //name, args, body

#[derive(Debug,Clone,Trace)]
pub struct CcFunctionStmt(Cc<FunctionStmt>);

impl std::ops::Deref for CcFunctionStmt {
    type Target = Cc<FunctionStmt>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

//im not sure if i actually have to implement this, i guess?? maybe???
impl Hash for CcFunctionStmt {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialEq for CcFunctionStmt {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl CcFunctionStmt {
    pub fn new(f: FunctionStmt) -> Self {
        Self(Cc::new(f))
    }
}

impl Trace for FunctionStmt {
    fn trace(&self, _tracer: &mut Tracer) { }
}

#[newtype_enum(variants = "pub StmtVar")]
#[derive(Debug,Clone,PartialEq,Hash)]
pub enum Stmt {
    ExprStmt { expr: Expr },
    If { cond: Expr, then: Box<Stmt>, else_b: Option<Box<Stmt>> },
    Print { expr: Expr },
    Return { keyword: Box<Token>, value: Option<Expr> }, 
    While { cond: Expr, body: Box<Stmt> },
    Var { name: Box<Token>, init: Option<Expr> },
    //TODO: first field has to be an identifier, how to avoid having to check the type again in the interpreter?
    Fun { stmt: CcFunctionStmt },
    Class { name: Box<Token>, methods: Vec<StmtVar::Fun>, superclass: Option<ExprVar::Variable> },
    //having tokens here is pretty cool as it allows better error handling 
    Block { left_brace: Box<Token>, body: Vec<Stmt> },
}

impl Stmt {
    pub fn get_token(&self) -> Token {
        match self {
            Stmt::ExprStmt(StmtVar::ExprStmt{expr}) => expr.get_token(),
            Stmt::If(StmtVar::If{cond, ..}) => cond.get_token(),
            Stmt::Print(StmtVar::Print{expr}) => expr.get_token(),
            Stmt::Return(StmtVar::Return{keyword, ..}) => *(keyword.clone()),
            Stmt::While(StmtVar::While{cond, ..}) => cond.get_token(),
            Stmt::Var(StmtVar::Var{name, ..}) => *(name.clone()),
            Stmt::Fun(StmtVar::Fun{stmt, ..}) => *(stmt.0.0.clone()),
            Stmt::Class(StmtVar::Class{name, ..}) => *(name.clone()),
            Stmt::Block(StmtVar::Block{left_brace, ..}) => *(left_brace.clone()),
        }
    }

    //super quality perfect code
    //im just appending strings lol whats the problem
    #[cfg(test)]
    pub fn stringify_tree(&self) -> String {
        use newtype_enum::Enum;
        "(".to_string() + &match self {
            Stmt::ExprStmt(StmtVar::ExprStmt{expr}) => return expr.stringify_tree(),
            Stmt::Class(StmtVar::Class{name, methods, superclass}) => format!("class {}{} [{}]", name.lexeme,
                superclass.as_ref().map_or("".to_string(), |v| String::from(" < ") + &v.name.lexeme),
                methods.iter().fold(String::new(), |t, v| t + &Stmt::from_variant(v.clone()).stringify_tree())),
            Stmt::If(StmtVar::If{cond, then, else_b}) => format!("if {} {}", cond.stringify_tree(), then.stringify_tree()) + 
                                            &else_b.as_ref().map_or("".to_string(), |v| " ".to_string() + &v.stringify_tree()),
            Stmt::Print(StmtVar::Print{expr}) => format!("print {}", expr.stringify_tree()),
            Stmt::Return(StmtVar::Return{value, ..}) => format!("return {}", value.as_ref().map_or("".to_string(), |v| v.stringify_tree())),
            Stmt::While(StmtVar::While{cond, body}) => format!("while {} {}", cond.stringify_tree(), body.stringify_tree()),
            Stmt::Var(StmtVar::Var{name, init}) => format!("var {} {}", name.lexeme.clone(), init.as_ref().map_or("".to_string(), |v| v.stringify_tree())),
            Stmt::Fun(StmtVar::Fun{stmt}) => format!("fun {}({}) {}", stmt.0.0.lexeme.clone(), 
                stmt.1.iter().fold(String::from(""), |t, v| t + &v.lexeme.clone()),
                format!("[{}]", stmt.2.body.iter().fold(String::new(), 
                |t, v| t + &v.stringify_tree()+ ",").strip_suffix(",").or(Some("")).unwrap())),
            Stmt::Block(StmtVar::Block{body, ..}) => return format!("[{}]", body.iter().fold(String::new(), |t, v| t + &v.stringify_tree() + ",").strip_suffix(",").or(Some("")).unwrap()),
        }.to_string() + ")"
    }
}

#[newtype_enum(variants = "pub ExprVar")]
#[derive(PartialEq,Eq,Debug,Clone,Hash)]
pub enum Expr {
    Assign { name: Box<Token>, expr: Box<Expr> },
    Logical { left: Box<Expr>, op: Box<Keyword>, right: Box<Expr> },
    Binary { left: Box<Expr>, op: Box<Token>, right: Box<Expr> },
    Call { callee: Box<Expr>, left_paren: Box<Token>, args: Vec<Expr> },
    Get { object: Box<Expr>, name: Box<Token> },
    Set { object: Box<Expr>, name: Box<Token>, value: Box<Expr> },
    This { keyword: Box<Token> },
    Super { keyword: Box<Token>, method: Box<Token> },
    Unary { op: Box<Token>, expr: Box<Expr> },
    Literal { token: Box<Token> },
    Variable { name: Box<Token> },
    Grouping { expr: Box<Expr> },
}

impl Expr {
    pub fn get_token(&self) -> Token {
        match self {
            Expr::Assign(ExprVar::Assign{name, ..}) => *(name.clone()),
            Expr::Logical(ExprVar::Logical{left, ..}) => left.get_token(),
            Expr::Binary(ExprVar::Binary{op, ..}) => *(op.clone()),
            Expr::Call(ExprVar::Call{left_paren, ..}) => *(left_paren.clone()),
            Expr::Get(ExprVar::Get{name, ..}) => *(name.clone()),
            Expr::Set(ExprVar::Set{name, ..}) => *(name.clone()),
            Expr::This(ExprVar::This{keyword, ..}) => *(keyword.clone()),
            Expr::Super(ExprVar::Super{keyword, ..}) => *(keyword.clone()),
            Expr::Unary(ExprVar::Unary{op, ..}) => *(op.clone()),
            Expr::Literal(ExprVar::Literal{token, ..}) => *(token.clone()),
            Expr::Variable(ExprVar::Variable{name, ..}) => *(name.clone()),
            Expr::Grouping(ExprVar::Grouping{expr, ..}) => expr.get_token(),
        }
    }

    #[cfg(test)]
    pub fn stringify_tree(&self) -> String {
        "(".to_string() + &match self {
            Expr::Assign(ExprVar::Assign{name, expr}) => format!("= {} {}", name.lexeme, expr.stringify_tree()),
            Expr::Set(ExprVar::Set{name, object, value}) => format!("= {}.{} {}", object.stringify_tree(), name.lexeme, value.stringify_tree()),
            Expr::Get(ExprVar::Get{name, object}) => format!("{}.{}", object.stringify_tree(), name.lexeme),
            Expr::Logical(ExprVar::Logical{left, op, right}) => format!("{:?} {} {}", op, left.stringify_tree(), right.stringify_tree()),
            Expr::Binary(ExprVar::Binary{left, op, right}) => format!("{} {} {}", op.lexeme, left.stringify_tree(), right.stringify_tree()),
            Expr::Call(ExprVar::Call{callee, args, ..}) => format!("{} ({})", callee.stringify_tree(), 
                                   args.iter().fold(String::new(), 
                                   |t, v| t + &v.stringify_tree()+ ", ").strip_suffix(", ").or(Some("")).unwrap()),
            Expr::Unary(ExprVar::Unary{op, expr}) => format!("{} {}", op, expr.stringify_tree()),
            Expr::Literal(ExprVar::Literal{token}) => return token.lexeme.clone(),
            Expr::Variable(ExprVar::Variable{name}) => return name.lexeme.clone(),
            Expr::Grouping(ExprVar::Grouping{expr}) => expr.stringify_tree(),
            Expr::Super(ExprVar::Super{method,..}) => return format!("super.{}", method.lexeme), 
            Expr::This(ExprVar::This{..}) => return format!("this"), 
        }.to_string() + ")"
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
