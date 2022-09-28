use std::hash::Hash;

use crate::{FastHashSet, Span};

use crate::front::wgsl::{
    ast::{AssignOp, BinaryOp, Ident, Literal, PostfixOp, UnaryOp},
    resolve::{
        features::EnabledFeatures,
        inbuilt::{
            AccessMode, AddressSpace, AttributeType, Builtin, ConservativeDepth, DepthTextureType,
            InterpolationSample, InterpolationType, MatType, PrimitiveType, SampledTextureType,
            SamplerType, StorageTextureType, TexelFormat, VecType,
        },
        inbuilt_functions::InbuiltFunction,
    },
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

#[derive(Clone, Debug)]
pub struct Attribute {
    pub ty: AttributeType,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum DeclDependencyKind {
    Decl(DeclId),
    Inbuilt(InbuiltFunction),
}

#[derive(Copy, Clone, Debug)]
pub struct DeclDependency {
    pub kind: DeclDependencyKind,
    pub usage: Span,
}

impl Hash for DeclDependency {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl PartialEq for DeclDependency {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for DeclDependency {}

#[derive(Clone, Debug)]
pub struct Decl {
    pub kind: DeclKind,
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
    pub attribs: FnAttribs,
    pub name: Ident,
    pub args: Vec<Arg>,
    pub ret_attribs: ArgAttribs,
    pub ret: Option<Type>,
    pub block: Block,
}

#[derive(Clone, Debug)]
pub enum FnAttribs {
    None,
    Vertex,
    Fragment(Option<ConservativeDepth>),
    Compute(Option<Expr>, Option<Expr>, Option<Expr>),
}

#[derive(Clone, Debug)]
pub struct Arg {
    pub attribs: ArgAttribs,
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
    pub id: LocalId,
}

#[derive(Clone, Debug)]
pub struct ArgAttribs {
    pub builtin: Option<Builtin>,
    pub location: Option<Expr>,
    pub interpolate: Option<(InterpolationType, InterpolationSample)>,
    pub invariant: bool,
}

#[derive(Clone, Debug)]
pub struct Override {
    pub id: Option<Expr>,
    pub name: Ident,
    pub ty: Option<Type>,
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
    pub address_space: AddressSpace,
    pub access_mode: AccessMode,
    pub name: Ident,
    pub ty: Option<Type>,
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
    pub arg: ArgAttribs,
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

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum FloatType {
    F16,
    F32,
    F64,
    Infer,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum SampleType {
    F64,
    F32,
    I32,
    U32,
}

#[derive(Clone, Debug)]
pub enum InbuiltType {
    Primitive(PrimitiveType),
    Vec {
        ty: PrimitiveType,
        comp: VecType,
    },
    Mat {
        ty: FloatType,
        comp: MatType,
    },
    SampledTexture(SampledTextureType, SampleType),
    DepthTexture(DepthTextureType),
    StorageTexture(StorageTextureType, TexelFormat, AccessMode),
    Sampler(SamplerType),
    Array {
        of: Box<Type>,
        len: Option<Expr>,
    },
    BindingArray {
        of: Box<Type>,
        len: Option<Expr>,
    },
    Ptr {
        to: Box<Type>,
        address_space: AddressSpace,
        access_mode: AccessMode,
    },
    Atomic {
        signed: bool, // Only i32 and u32 are allowed.
    },
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
    Loop(Block),
    Return(Option<Expr>),
    StaticAssert(Expr),
    Switch(Switch),
    While(While),
    Continuing(Block),
    BreakIf(Expr),
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
    Postfix(PostfixExpr),
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
    pub update: Option<ExprStatement>, // var decls are not allowed here.
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
    Error,
    Literal(Literal),
    Local(LocalId),
    Global(DeclId),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
    Index(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, Ident),
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct AssignExpr {
    pub lhs: Box<AssignTarget>,
    pub op: AssignOp,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct CallExpr {
    pub target: FnTarget,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct AssignTarget {
    pub kind: AssignTargetKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum AssignTargetKind {
    Ignore,
    Local(LocalId),
    Global(DeclId),
    Index(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, Ident),
    Deref(Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum FnTarget {
    Decl(DeclId),
    InbuiltFunction(InbuiltFunction),
    InbuiltType(Box<InbuiltType>),
    Error,
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct PostfixExpr {
    pub expr: Box<Expr>,
    pub op: PostfixOp,
}

#[derive(Clone, Debug)]
pub struct VarDecl {
    pub kind: VarDeclKind,
    pub local: LocalId,
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
    pub ty: Option<Type>,
    pub val: Expr,
}
