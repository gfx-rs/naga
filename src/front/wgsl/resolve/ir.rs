use std::hash::Hash;

use crate::{FastHashSet, Span};

use crate::front::wgsl::{
    ast::{Ident, Literal},
    resolve::{features::EnabledFeatures, inbuilt_functions::InbuiltFunction},
};

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct DeclId(pub u32);
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct LocalId(pub u32);

#[derive(Clone, Debug)]
pub struct TranslationUnit {
    pub features: EnabledFeatures,
    pub decls: Vec<Decl>,
    pub roots: Vec<DeclId>,
    pub dependency_order: Vec<DeclId>,
}

impl TranslationUnit {
    pub fn decls_ordered(&self) -> impl Iterator<Item = (DeclId, &Decl)> {
        self.dependency_order
            .iter()
            .map(move |id| (*id, &self.decls[id.0 as usize]))
    }

    pub fn get(&self, id: DeclId) -> &Decl {
        &self.decls[id.0 as usize]
    }
}

impl TranslationUnit {
    pub fn new(features: EnabledFeatures) -> Self {
        Self {
            features,
            decls: Vec::new(),
            roots: Vec::new(),
            dependency_order: Vec::new(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct DeclDependency {
    pub id: DeclId,
    pub usage: Span,
}

impl Hash for DeclDependency {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for DeclDependency {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for DeclDependency {}

#[derive(Clone, Debug)]
pub struct Decl {
    pub kind: DeclKind,
    /// The direct dependencies of this declaration.
    pub dependencies: FastHashSet<DeclDependency>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum DeclKind {
    Fn(Fn),
    Override(Override),
    Var(Var),
    Const(Let),
    StaticAssert(Expr),
    Struct(Struct),
    Type(TypeDecl),
}

#[derive(Clone, Debug)]
pub struct Fn {
    pub stage: ShaderStage,
    pub name: Ident,
    pub args: Vec<Arg>,
    pub ret_binding: Option<Binding>,
    pub ret: Option<Type>,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub enum ShaderStage {
    None,
    Vertex,
    Fragment(Option<crate::EarlyDepthTest>),
    Compute(Option<Expr>, Option<Expr>, Option<Expr>),
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub binding: Option<Binding>,
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
    pub id: LocalId,
}

#[derive(Clone, Debug)]
pub enum Binding {
    Builtin(crate::BuiltIn),
    Location {
        location: Option<Expr>,
        interpolation: Option<crate::Interpolation>,
        sampling: Option<crate::Sampling>,
    },
}

#[derive(Clone, Debug)]
pub struct Override {
    pub id: Option<Expr>,
    pub name: Ident,
    pub ty: Type,
    pub val: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Var {
    pub attribs: VarAttribs,
    pub inner: VarNoAttribs,
}

#[derive(Clone, Debug)]
pub struct VarAttribs {
    pub group: Option<Expr>,
    pub binding: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct VarNoAttribs {
    pub address_space: crate::AddressSpace,
    pub name: Ident,
    pub ty: Type,
    pub val: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub attribs: FieldAttribs,
    pub name: Ident,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct FieldAttribs {
    pub align: Option<Expr>,
    pub size: Option<Expr>,
    pub binding: Option<Binding>,
}

#[derive(Clone, Debug)]
pub struct TypeDecl {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Inbuilt(InbuiltType),
    User(DeclId),
}

#[derive(Clone, Debug)]
pub enum InbuiltType {
    Scalar {
        kind: crate::ScalarKind,
        width: crate::Bytes,
    },
    Vector {
        size: crate::VectorSize,
        kind: crate::ScalarKind,
        width: crate::Bytes,
    },
    Matrix {
        columns: crate::VectorSize,
        rows: crate::VectorSize,
        width: crate::Bytes,
    },
    Image {
        dim: crate::ImageDimension,
        arrayed: bool,
        class: crate::ImageClass,
    },
    Sampler {
        comparison: bool,
    },
    Array {
        of: Box<Type>,
        len: Option<Expr>,
    },
    BindingArray {
        of: Box<Type>,
        len: Option<Expr>,
    },
    Pointer {
        to: Box<Type>,
        space: crate::AddressSpace,
    },
    Atomic {
        kind: crate::ScalarKind,
        width: crate::Bytes,
    },
    Infer,
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
    Expr(ExprStatementKind),
    Block(Block),
    Break,
    Continue,
    Discard,
    For(For),
    If(If),
    Loop(Loop),
    Return(Option<Expr>),
    StaticAssert(Expr),
    Switch(Switch),
    While(While),
}

#[derive(Clone, Debug)]
pub struct ExprStatement {
    pub kind: ExprStatementKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprStatementKind {
    VarDecl(VarDecl),
    Call(CallExpr),
    Assign(AssignExpr),
}

#[derive(Clone, Debug)]
pub struct CallStmt {
    pub name: Ident,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct For {
    pub init: Option<ExprStatement>,
    pub cond: Option<Expr>,
    /// Var decls are not going to be here.
    pub update: Option<ExprStatement>,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub struct If {
    pub cond: Expr,
    pub block: Block,
    pub else_: Option<Box<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct Loop {
    pub body: Block,
    pub continuing: Option<Block>,
    pub break_if: Option<Expr>,
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
    Error,
    Literal(Literal),
    Local(LocalId),
    Global(DeclId),
    Unary(UnaryExpr),
    AddrOf(Box<Expr>),
    Deref(Box<Expr>),
    Binary(BinaryExpr),
    Call(CallExpr),
    Index(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, Ident),
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: crate::UnaryOperator,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub target: AssignTarget,
    pub op: Option<crate::BinaryOperator>,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum AssignTarget {
    Phony,
    Expr(Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: FnTarget,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum FnTarget {
    Decl(DeclId),
    InbuiltFunction(InbuiltFunction, Vec<Type>),
    InbuiltType(Box<InbuiltType>),
    Error,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: crate::BinaryOperator,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub kind: VarDeclKind,
    pub id: LocalId,
}

#[derive(Clone, Debug)]
pub enum VarDeclKind {
    Var(VarNoAttribs),
    Const(Let),
    Let(Let),
}

#[derive(Clone, Debug)]
pub struct Let {
    pub name: Ident,
    pub ty: Type,
    pub val: Expr,
}
