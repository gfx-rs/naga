use crate::{front::wgsl::text::Text, Span};

#[derive(Clone, Debug, Default)]
pub struct TranslationUnit {
    pub enables: Vec<Enable>,
    pub decls: Vec<GlobalDecl>,
}

#[derive(Clone, Debug)]
pub struct Enable {
    pub name: Ident,
    pub span: Span,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Ident {
    pub name: Text,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub name: Ident,
    pub exprs: Vec<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct GlobalDecl {
    pub kind: GlobalDeclKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum GlobalDeclKind {
    Fn(Fn),
    Override(Override),
    Var(Var),
    Let(Let),
    Const(Let),
    StaticAssert(StaticAssert),
    Struct(Struct),
    Type(TypeDecl),
}

#[derive(Clone, Debug)]
pub struct Fn {
    pub attribs: Vec<Attribute>,
    pub name: Ident,
    pub args: Vec<Arg>,
    pub ret_attribs: Vec<Attribute>,
    pub ret: Option<Type>,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Override {
    pub attribs: Vec<Attribute>,
    pub name: Ident,
    pub ty: Option<Type>,
    pub val: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub attribs: Vec<Attribute>,
    pub inner: VarNoAttribs,
}

#[derive(Clone, Debug)]
pub struct VarNoAttribs {
    pub address_space: Option<Ident>,
    pub access_mode: Option<Ident>,
    pub name: Ident,
    pub ty: Option<Type>,
    pub val: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct StaticAssert {
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<Arg>,
}

#[derive(Clone, Debug)]
pub struct TypeDecl {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub attribs: Vec<Attribute>,
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Ident(Ident, Vec<Type>),
    Array(Ident, Box<Type>, Option<Expr>),
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Block(Block),
    Expr(Expr),
    Break,
    Continue,
    Discard,
    For(For),
    If(If),
    Loop(Block),
    Return(Option<Expr>),
    StaticAssert(StaticAssert),
    Switch(Switch),
    While(While),
    Continuing(Block),
    BreakIf(Expr),
    Empty,
}

#[derive(Clone, Debug)]
pub struct For {
    pub init: Option<Expr>,
    pub cond: Option<Expr>,
    pub update: Option<Expr>,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct If {
    pub cond: Expr,
    pub block: Block,
    pub else_: Option<Box<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct While {
    pub cond: Expr,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct Switch {
    pub expr: Expr,
    pub cases: Vec<Case>,
}

#[derive(Clone, Debug)]
pub struct Case {
    pub selectors: Vec<CaseSelector>,
    pub block: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum CaseSelector {
    Expr(Expr),
    Default,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Underscore,
    VarDecl(Box<VarDecl>),
    Literal(Literal),
    Ident(IdentExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Assign(AssignExpr),
    Call(CallExpr),
    Index(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, Ident),
    Postfix(PostfixExpr),
}

#[derive(Clone, Debug)]
pub struct IdentExpr {
    pub name: Ident,
    pub generics: Vec<Type>,
    pub array_len: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    AbstractInt(i64),
    AbstractFloat(f64),
    I32(i32),
    U32(u32),
    F32(f32),
    F16(half::f16),
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Ref,
    Deref,
    Not,
    Minus,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Box<Expr>,
    pub op: Option<crate::BinaryOperator>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: crate::BinaryOperator,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct PostfixExpr {
    pub expr: Box<Expr>,
    pub op: PostfixOp,
}

#[derive(Copy, Clone, Debug)]
pub enum PostfixOp {
    Increment,
    Decrement,
}

#[derive(Clone, Debug)]
pub enum VarDecl {
    Var(VarNoAttribs),
    Const(Let),
    Let(Let),
}

#[derive(Clone, Debug)]
pub struct Let {
    pub name: Ident,
    pub ty: Option<Type>,
    pub val: Expr,
}
