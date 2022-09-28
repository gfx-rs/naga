use aho_corasick::AhoCorasick;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::front::wgsl::ast::UnaryOp;
use crate::front::wgsl::resolve::inbuilt::Attribute;
use crate::front::wgsl::WgslError;
use crate::{
    front::wgsl::ast,
    front::wgsl::ast::{ExprKind, GlobalDeclKind, Ident, StmtKind, VarDecl},
    front::wgsl::resolve::{
        features::{EnabledFeatures, Feature},
        inbuilt::{
            reserved_matcher, AccessMode, AddressSpace, AttributeType, Builtin, ConservativeDepth,
            DepthTextureType, InterpolationSample, InterpolationType, MatType, Matcher,
            PrimitiveType, SampledTextureType, SamplerType, StorageTextureType, TexelFormat,
            ToStaticString, VecType,
        },
        inbuilt_functions::InbuiltFunction,
        index::Index,
        ir::{DeclDependency, DeclId, FnTarget, InbuiltType, LocalId},
    },
    front::wgsl::text::{Interner, Text},
    BinaryOperator, ImageClass, ScalarKind, Span, StorageAccess, UnaryOperator,
};

mod dependency;
pub mod features;
pub mod inbuilt;
pub mod inbuilt_functions;
mod index;
pub mod ir;

pub fn resolve(
    tu: ast::TranslationUnit,
    intern: &mut Interner,
    diagnostics: &mut Vec<WgslError>,
) -> ir::TranslationUnit {
    let index = index::generate_index(&tu, diagnostics);

    let mut out = ir::TranslationUnit::new(EnabledFeatures::new(intern));

    for enable in tu.enables {
        out.features.enable(enable, intern, diagnostics);
    }

    let mut resolver = Resolver {
        kws: Box::new(Kws::init(intern)),
        access_mode: Matcher::new(intern),
        address_space: Matcher::new(intern),
        builtin: Matcher::new(intern),
        interpolation_sample: Matcher::new(intern),
        interpolation_type: Matcher::new(intern),
        primitive: Matcher::new(intern),
        vec: Matcher::new(intern),
        mat: Matcher::new(intern),
        sampled_texture: Matcher::new(intern),
        depth_texture: Matcher::new(intern),
        sampler: Matcher::new(intern),
        storage_texture: Matcher::new(intern),
        texel_format: Matcher::new(intern),
        conservative_depth: Matcher::new(intern),
        inbuilt_function: Matcher::new(intern),
        tu: &mut out,
        index,
        diagnostics,
        intern,
        reserved_matcher: reserved_matcher(),
        locals: 0,
        in_function: false,
        scopes: Vec::new(),
        dependencies: FxHashSet::default(),
    };

    for decl in tu.decls {
        resolver.decl(decl);
    }

    dependency::resolve_all_dependencies(&mut out, diagnostics);

    out
}

struct Resolver<'a> {
    index: Index,
    tu: &'a mut ir::TranslationUnit,
    diagnostics: &'a mut Vec<WgslError>,
    intern: &'a mut Interner,
    reserved_matcher: AhoCorasick,
    access_mode: Matcher<AccessMode>,
    address_space: Matcher<AddressSpace>,
    builtin: Matcher<Builtin>,
    interpolation_sample: Matcher<InterpolationSample>,
    interpolation_type: Matcher<InterpolationType>,
    primitive: Matcher<PrimitiveType>,
    vec: Matcher<VecType>,
    mat: Matcher<MatType>,
    sampled_texture: Matcher<SampledTextureType>,
    depth_texture: Matcher<DepthTextureType>,
    sampler: Matcher<SamplerType>,
    storage_texture: Matcher<StorageTextureType>,
    texel_format: Matcher<TexelFormat>,
    conservative_depth: Matcher<ConservativeDepth>,
    inbuilt_function: Matcher<InbuiltFunction>,
    kws: Box<Kws>,
    locals: u32,
    in_function: bool,
    scopes: Vec<FxHashMap<Text, (LocalId, Span, bool)>>,
    dependencies: FxHashSet<DeclDependency>,
}

impl<'a> Resolver<'a> {
    fn decl(&mut self, decl: ast::GlobalDecl) {
        self.locals = 0;

        let kind = match decl.kind {
            GlobalDeclKind::Fn(f) => {
                let f = self.fn_(f);

                if !matches!(f.stage, ir::ShaderStage::None) {
                    self.tu.roots.push(DeclId(self.tu.decls.len() as _));
                }

                ir::DeclKind::Fn(f)
            }
            GlobalDeclKind::Override(ov) => ir::DeclKind::Override(self.ov(ov)),
            GlobalDeclKind::Var(v) => ir::DeclKind::Var(ir::Var {
                attribs: self.var_attribs(v.attribs),
                inner: self.var(v.inner),
            }),
            GlobalDeclKind::Let(l) => ir::DeclKind::Const(self.let_(l)),
            GlobalDeclKind::Const(c) => ir::DeclKind::Const(self.let_(c)),
            GlobalDeclKind::StaticAssert(s) => ir::DeclKind::StaticAssert(self.expr(s.expr)),
            GlobalDeclKind::Struct(s) => {
                self.verify_ident(s.name);
                ir::DeclKind::Struct(ir::Struct {
                    name: s.name,
                    fields: s.fields.into_iter().map(|f| self.field(f)).collect(),
                })
            }
            GlobalDeclKind::Type(ty) => {
                self.verify_ident(ty.name);
                ir::DeclKind::Type(ir::TypeDecl {
                    name: ty.name,
                    ty: self.ty(ty.ty),
                })
            }
        };

        let decl = ir::Decl {
            kind,
            span: decl.span,
            dependencies: std::mem::take(&mut self.dependencies),
        };

        self.tu.decls.push(decl);
    }

    fn fn_(&mut self, fn_: ast::Fn) -> ir::Fn {
        self.verify_ident(fn_.name);

        self.in_function = true;

        self.scopes.push(FxHashMap::default());
        let args = fn_.args.into_iter().map(|x| self.arg(x)).collect();
        let block = self.block_inner(fn_.block);
        self.pop_scope();

        self.in_function = false;

        ir::Fn {
            stage: self.fn_attribs(fn_.attribs),
            name: fn_.name,
            args,
            ret_binding: self.binding(fn_.ret_attribs),
            ret: fn_.ret.map(|x| self.ty(x)),
            block,
        }
    }

    fn ov(&mut self, o: ast::Override) -> ir::Override {
        self.verify_ident(o.name);

        let mut id = None;
        let a: Vec<_> = o
            .attribs
            .into_iter()
            .filter_map(|x| self.attrib(x))
            .collect();
        for attrib in a {
            match attrib.ty {
                AttributeType::Id(expr) => {
                    if id.is_some() {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    } else {
                        id = Some(self.expr(expr));
                    }
                }
                _ => {
                    self.diagnostics.push(
                        WgslError::new("this attribute is not allowed here").marker(attrib.span),
                    );
                }
            }
        }

        ir::Override {
            id,
            name: o.name,
            ty: o.ty.map(|x| self.ty(x)).unwrap_or(ir::Type {
                kind: ir::TypeKind::Inbuilt(InbuiltType::Infer),
                span: Span::UNDEFINED,
            }),
            val: o.val.map(|x| self.expr(x)),
        }
    }

    fn arg(&mut self, arg: ast::Arg) -> ir::Arg {
        self.verify_ident(arg.name);

        let args = self.scopes.last_mut().expect("no scope");
        let id = LocalId(self.locals);
        self.locals += 1;
        let old = args.insert(arg.name.name, (id, arg.span, false));
        if let Some((_, span, _)) = old {
            self.diagnostics.push(
                WgslError::new("duplicate argument name")
                    .label(arg.name.span, "redefined here")
                    .label(span, "previous definition"),
            );
        }

        ir::Arg {
            binding: self.binding(arg.attribs),
            name: arg.name,
            ty: self.ty(arg.ty),
            span: arg.span,
            id,
        }
    }

    fn let_(&mut self, l: ast::Let) -> ir::Let {
        self.verify_ident(l.name);
        ir::Let {
            name: l.name,
            ty: l.ty.map(|x| self.ty(x)).unwrap_or(ir::Type {
                kind: ir::TypeKind::Inbuilt(InbuiltType::Infer),
                span: Span::UNDEFINED,
            }),
            val: self.expr(l.val),
        }
    }

    fn var_attribs(&mut self, attribs: Vec<ast::Attribute>) -> ir::VarAttribs {
        let mut out = ir::VarAttribs {
            group: None,
            binding: None,
        };

        let a: Vec<_> = attribs.into_iter().filter_map(|x| self.attrib(x)).collect();
        for attrib in a {
            match attrib.ty {
                AttributeType::Group(g) => {
                    if out.group.is_some() {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    } else {
                        out.group = Some(self.expr(g));
                    }
                }
                AttributeType::Binding(b) => {
                    if out.binding.is_some() {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    } else {
                        out.binding = Some(self.expr(b));
                    }
                }
                _ => {
                    self.diagnostics.push(
                        WgslError::new("this attribute is not allowed here").marker(attrib.span),
                    );
                }
            }
        }

        out
    }

    fn binding(&mut self, attribs: Vec<ast::Attribute>) -> Option<ir::Binding> {
        let mut out = None;

        let a: Vec<_> = attribs.into_iter().filter_map(|x| self.attrib(x)).collect();

        let mut inv = None;
        let mut builtin = None;

        for attrib in a {
            self.binding_inner(attrib, &mut out, &mut inv, &mut builtin);
        }

        if let Some(invariant) = inv {
            if builtin.is_none() {
                self.diagnostics.push(
                    WgslError::new("invariant requires a `@builtin(position)` attribute")
                        .marker(invariant),
                );
            }
        }

        out
    }

    fn binding_inner(
        &mut self,
        attrib: Attribute,
        out: &mut Option<ir::Binding>,
        inv: &mut Option<Span>,
        builtin: &mut Option<Span>,
    ) {
        match attrib.ty {
            AttributeType::Builtin(b) => match out {
                Some(ir::Binding::Builtin(_)) if inv.is_none() => {
                    self.diagnostics
                        .push(WgslError::new("duplicate attribute").marker(attrib.span));
                }
                Some(ir::Binding::Location { .. }) => {
                    self.diagnostics.push(
                        WgslError::new("this attribute is not allowed here").marker(attrib.span),
                    );
                }
                _ => {
                    *out = Some(ir::Binding::Builtin(b.into()));
                    *builtin = Some(attrib.span);
                }
            },
            AttributeType::Location(l) => match out {
                Some(ir::Binding::Builtin(_)) => {
                    self.diagnostics.push(
                        WgslError::new("this attribute is not allowed here").marker(attrib.span),
                    );
                }
                Some(ir::Binding::Location { ref location, .. }) if location.is_some() => {
                    self.diagnostics
                        .push(WgslError::new("duplicate attribute").marker(attrib.span));
                }
                Some(ir::Binding::Location {
                    ref mut location, ..
                }) => {
                    *location = Some(self.expr(l));
                }
                None => {
                    *out = Some(ir::Binding::Location {
                        location: Some(self.expr(l)),
                        interpolation: None,
                        sampling: None,
                    });
                }
            },
            AttributeType::Interpolate(i, s) => match out {
                Some(ir::Binding::Builtin(_)) => {
                    self.diagnostics.push(
                        WgslError::new("this attribute is not allowed here").marker(attrib.span),
                    );
                }
                Some(ir::Binding::Location { interpolation, .. }) if interpolation.is_some() => {
                    self.diagnostics
                        .push(WgslError::new("duplicate attribute").marker(attrib.span));
                }
                Some(ir::Binding::Location {
                    ref mut interpolation,
                    ref mut sampling,
                    ..
                }) => {
                    *interpolation = Some(i.into());
                    *sampling = s.map(|x| x.into());
                }
                None => {
                    *out = Some(ir::Binding::Location {
                        location: None,
                        interpolation: Some(i.into()),
                        sampling: s.map(|x| x.into()),
                    });
                }
            },
            AttributeType::Invariant => match out {
                Some(ir::Binding::Builtin(_)) if builtin.is_none() => {
                    self.diagnostics
                        .push(WgslError::new("duplicate attribute").marker(attrib.span));
                }
                Some(ir::Binding::Builtin(ref mut b)) => match b {
                    crate::BuiltIn::Position { ref mut invariant } => {
                        *invariant = true;
                        *inv = Some(attrib.span);
                    }
                    _ => {
                        self.diagnostics.push(
                            WgslError::new("this attribute is not allowed here")
                                .marker(attrib.span),
                        );
                    }
                },
                Some(ir::Binding::Location { .. }) => {
                    self.diagnostics.push(
                        WgslError::new("this attribute is not allowed here").marker(attrib.span),
                    );
                }
                None => {
                    *out = Some(ir::Binding::Builtin(crate::BuiltIn::Position {
                        invariant: true,
                    }));
                    *inv = Some(attrib.span);
                }
            },
            _ => {
                self.diagnostics
                    .push(WgslError::new("this attribute is not allowed here").marker(attrib.span));
            }
        }
    }

    fn fn_attribs(&mut self, attribs: Vec<ast::Attribute>) -> ir::ShaderStage {
        let mut out = ir::ShaderStage::None;
        let mut expect_compute = None;

        let a: Vec<_> = attribs.into_iter().filter_map(|x| self.attrib(x)).collect();
        for attrib in a {
            match attrib.ty {
                AttributeType::Const => self.diagnostics.push(
                    WgslError::new("user defined `const` functions are not allowed")
                        .marker(attrib.span),
                ),
                AttributeType::Vertex => {
                    if let ir::ShaderStage::None = out {
                        out = ir::ShaderStage::Vertex;
                    } else {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    }
                }
                AttributeType::Fragment => {
                    if let ir::ShaderStage::None = out {
                        out = ir::ShaderStage::Fragment(None);
                    } else {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    }
                }
                AttributeType::Compute => {
                    if let ir::ShaderStage::None = out {
                        expect_compute = Some(attrib.span);
                        out = ir::ShaderStage::Compute(None, None, None);
                    } else if expect_compute.is_some() {
                        expect_compute = None;
                    } else {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    }
                }
                AttributeType::WorkgroupSize(x, y, z) => {
                    if let ir::ShaderStage::None = out {
                        expect_compute = Some(attrib.span);
                        out = ir::ShaderStage::Compute(
                            Some(self.expr(x)),
                            y.map(|x| self.expr(x)),
                            z.map(|x| self.expr(x)),
                        );
                    } else if expect_compute.is_some() {
                        expect_compute = None;
                    } else {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    }
                }
                AttributeType::ConservativeDepth(depth) => {
                    if let ir::ShaderStage::Fragment(_) = out {
                        out = ir::ShaderStage::Fragment(Some(crate::EarlyDepthTest {
                            conservative: depth.map(|x| x.into()),
                        }));
                    } else {
                        self.diagnostics.push(
                            WgslError::new("this attribute is not allowed here")
                                .marker(attrib.span),
                        );
                    }
                }
                _ => {
                    self.diagnostics.push(
                        WgslError::new("this attribute is not allowed here").marker(attrib.span),
                    );
                }
            }
        }

        if let Some(span) = expect_compute {
            self.diagnostics.push(
                WgslError::new(if matches!(out, ir::ShaderStage::Compute(None, _, _)) {
                    "`@compute` without `@workgroup_size` attribute"
                } else {
                    "`@workgroup_size` without `@compute` attribute"
                })
                .marker(span),
            );
        }

        out
    }

    fn field(&mut self, field: ast::Arg) -> ir::Field {
        let mut attribs = ir::FieldAttribs {
            align: None,
            binding: None,
            size: None,
        };

        let a: Vec<_> = field
            .attribs
            .into_iter()
            .filter_map(|x| self.attrib(x))
            .collect();

        let mut inv = None;
        let mut builtin = None;

        for attrib in a {
            match attrib.ty {
                AttributeType::Align(expr) => {
                    if attribs.align.is_some() {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    } else {
                        attribs.align = Some(self.expr(expr));
                    }
                }
                AttributeType::Size(expr) => {
                    if attribs.size.is_some() {
                        self.diagnostics
                            .push(WgslError::new("duplicate attribute").marker(attrib.span));
                    } else {
                        attribs.size = Some(self.expr(expr));
                    }
                }
                _ => {
                    self.binding_inner(attrib, &mut attribs.binding, &mut inv, &mut builtin);
                }
            }
        }

        ir::Field {
            attribs,
            name: field.name,
            ty: self.ty(field.ty),
        }
    }

    fn var(&mut self, v: ast::VarNoAttribs) -> ir::VarNoAttribs {
        self.verify_ident(v.name);

        let ty = v.ty.map(|x| self.ty(x)).unwrap_or(ir::Type {
            kind: ir::TypeKind::Inbuilt(InbuiltType::Infer),
            span: Span::UNDEFINED,
        });

        let as_ = v
            .address_space
            .map(|x| (self.address_space(x), x.span))
            .and_then(|(a, s)| a.map(|a| (a, s)));
        let am = v
            .access_mode
            .map(|x| (self.access_mode(x), x.span))
            .and_then(|(a, s)| a.map(|a| (a, s)));

        let address_space = as_.map(|x| x.0).unwrap_or_else(|| {
            if let ir::TypeKind::Inbuilt(
                InbuiltType::BindingArray { .. }
                | InbuiltType::Sampler { .. }
                | InbuiltType::Image { .. },
            ) = &ty.kind
            {
                // Infer handle if its a resource type.
                AddressSpace::Handle
            } else {
                AddressSpace::Private
            }
        });

        if self.in_function && address_space != AddressSpace::Function {
            let span = as_.unwrap().1;
            self.diagnostics.push(
                WgslError::new(format!(
                    "cannot declare variable with address space `{}` in a function",
                    address_space
                ))
                .marker(span),
            );
        } else if !self.in_function && address_space == AddressSpace::Function {
            let span = as_.unwrap().1;
            self.diagnostics.push(
                WgslError::new(
                    "cannot declare variable with address space `function` outside of a function",
                )
                .marker(span),
            );
        }

        let address_space = if let Some((mode, span)) = am {
            let mut x = address_space.into();
            match x {
                crate::AddressSpace::Storage { ref mut access } => *access = mode.into(),
                _ => {
                    self.diagnostics.push(
                        WgslError::new(format!(
                            "cannot declare variable with access mode `{}` in address space `{}`",
                            mode, address_space
                        ))
                        .marker(span),
                    );
                }
            }

            x
        } else {
            address_space.into()
        };

        ir::VarNoAttribs {
            address_space,
            name: v.name,
            ty,
            val: v.val.map(|x| self.expr(x)),
        }
    }

    fn ty(&mut self, ty: ast::Type) -> ir::Type {
        let span = ty.span;
        let t = match &ty.kind {
            ast::TypeKind::Ident(ident, generics) => Some((*ident, generics.len() == 0)),
            _ => None,
        };
        let kind = if let Some(inbuilt) = self.inbuilt(ty) {
            ir::TypeKind::Inbuilt(inbuilt)
        } else {
            let (ident, no_generics) = t.unwrap();
            if !no_generics {
                self.diagnostics
                    .push(WgslError::new("unexpected generics on type").marker(ident.span));
            }

            if let Some(user) = self.index.get(ident.name) {
                self.dependencies.insert(DeclDependency {
                    id: user,
                    usage: ident.span,
                });
                ir::TypeKind::User(user)
            } else {
                self.diagnostics
                    .push(WgslError::new("undefined type").marker(ident.span));
                ir::TypeKind::Inbuilt(InbuiltType::Scalar {
                    kind: ScalarKind::Sint,
                    width: 4,
                })
            }
        };

        ir::Type { kind, span }
    }

    fn block(&mut self, block: ast::Block) -> ir::Block {
        self.scopes.push(FxHashMap::default());
        let ret = self.block_inner(block);
        self.pop_scope();
        ret
    }

    fn block_inner(&mut self, block: ast::Block) -> ir::Block {
        let mut stmts = Vec::with_capacity(block.stmts.len());
        for stmt in block.stmts {
            if let Some(stmt) = self.stmt(stmt) {
                stmts.push(stmt);
            }
        }

        ir::Block {
            stmts,
            span: block.span,
        }
    }

    fn stmt(&mut self, stmt: ast::Stmt) -> Option<ir::Stmt> {
        let kind = match stmt.kind {
            StmtKind::Block(block) => ir::StmtKind::Block(self.block(block)),
            StmtKind::Expr(expr) => ir::StmtKind::Expr(self.expr_statement(expr)?.kind),
            StmtKind::Break => ir::StmtKind::Break,
            StmtKind::Continue => ir::StmtKind::Continue,
            StmtKind::Discard => ir::StmtKind::Discard,
            StmtKind::For(for_) => {
                self.scopes.push(FxHashMap::default());
                let init = for_.init.and_then(|x| self.expr_statement(x));
                let cond = for_.cond.map(|x| self.expr(x));
                let update = for_
                    .update
                    .and_then(|x| self.expr_statement(x))
                    .and_then(|x| {
                        if matches!(x.kind, ir::ExprStatementKind::VarDecl(_)) {
                            self.diagnostics.push(
                                WgslError::new("variable declaration not allowed here")
                                    .marker(x.span),
                            );
                            None
                        } else {
                            Some(x)
                        }
                    });
                let block = self.block_inner(for_.block);
                self.pop_scope();

                ir::StmtKind::For(ir::For {
                    init,
                    cond,
                    update,
                    block,
                })
            }
            StmtKind::If(if_) => {
                let cond = self.expr(if_.cond);
                let block = self.block(if_.block);
                let mut else_ = if_.else_.and_then(|x| self.stmt(*x)).map(Box::new);

                if !matches!(
                    else_.as_ref().map(|x| &x.kind),
                    Some(ir::StmtKind::If(_) | ir::StmtKind::Block(_)) | None
                ) {
                    self.diagnostics.push(
                        WgslError::new("`else` must be followed by `if` or block")
                            .marker(stmt.span),
                    );
                    else_ = None;
                }

                ir::StmtKind::If(ir::If { cond, block, else_ })
            }
            StmtKind::Loop(mut block) => {
                let (continuing, break_if) = if let Some(continuing) = block.stmts.pop() {
                    match continuing.kind {
                        StmtKind::Continuing(mut c) => {
                            let break_if = if let Some(break_if) = c.stmts.pop() {
                                match break_if.kind {
                                    StmtKind::BreakIf(b) => Some(self.expr(b)),
                                    _ => {
                                        c.stmts.push(break_if);
                                        None
                                    }
                                }
                            } else {
                                None
                            };

                            (Some(self.block(c)), break_if)
                        }
                        _ => {
                            block.stmts.push(continuing);
                            (None, None)
                        }
                    }
                } else {
                    (None, None)
                };

                let body = self.block(block);
                ir::StmtKind::Loop(ir::Loop {
                    body,
                    continuing,
                    break_if,
                })
            }
            StmtKind::Return(expr) => ir::StmtKind::Return(expr.map(|x| self.expr(x))),
            StmtKind::StaticAssert(assert) => ir::StmtKind::StaticAssert(self.expr(assert.expr)),
            StmtKind::Switch(switch) => ir::StmtKind::Switch(ir::Switch {
                expr: self.expr(switch.expr),
                cases: switch
                    .cases
                    .into_iter()
                    .map(|case| ir::Case {
                        selectors: case
                            .selectors
                            .into_iter()
                            .map(|sel| match sel {
                                ast::CaseSelector::Expr(expr) => {
                                    ir::CaseSelector::Expr(self.expr(expr))
                                }
                                ast::CaseSelector::Default => ir::CaseSelector::Default,
                            })
                            .collect(),
                        block: self.block(case.block),
                        span: case.span,
                    })
                    .collect(),
            }),
            StmtKind::While(while_) => ir::StmtKind::While(ir::While {
                cond: self.expr(while_.cond),
                block: self.block(while_.block),
            }),
            StmtKind::Continuing(c) => {
                let _ = self.block(c);
                self.diagnostics.push(
                    WgslError::new("`continuing` must be the last statement in `loop`")
                        .marker(stmt.span),
                );
                return None;
            }
            StmtKind::BreakIf(x) => {
                let _ = self.expr(x);
                self.diagnostics.push(
                    WgslError::new("`break if` must be the last statement in `continuing`")
                        .marker(stmt.span),
                );
                return None;
            }
            StmtKind::Empty => return None,
        };

        Some(ir::Stmt {
            kind,
            span: stmt.span,
        })
    }

    fn expr(&mut self, expr: ast::Expr) -> ir::Expr {
        let kind = match expr.kind {
            ExprKind::Underscore => {
                self.diagnostics
                    .push(WgslError::new("cannot use `_` as an expression").marker(expr.span));
                ir::ExprKind::Error
            }
            ExprKind::VarDecl(_) => {
                self.diagnostics.push(
                    WgslError::new("cannot use variable declaration as an expression")
                        .marker(expr.span),
                );
                ir::ExprKind::Error
            }
            ExprKind::Literal(l) => {
                match l {
                    ast::Literal::F16(_) => {
                        self.tu
                            .features
                            .require(Feature::Float16, expr.span, &mut self.diagnostics)
                    }
                    _ => {}
                }
                ir::ExprKind::Literal(l)
            }
            ExprKind::Ident(ident) => {
                self.verify_ident(ident.name);
                if ident.generics.len() != 0 {
                    self.diagnostics
                        .push(WgslError::new("generics not allowed here").marker(expr.span));
                }
                self.resolve_access(ident.name)
            }
            ExprKind::Unary(u) => match u.op {
                UnaryOp::Ref => ir::ExprKind::AddrOf(Box::new(self.expr(*u.expr))),
                UnaryOp::Deref => ir::ExprKind::Deref(Box::new(self.expr(*u.expr))),
                op => {
                    let op = match op {
                        UnaryOp::Not => UnaryOperator::Not,
                        UnaryOp::Minus => UnaryOperator::Negate,
                        _ => unreachable!(),
                    };
                    ir::ExprKind::Unary(ir::UnaryExpr {
                        op,
                        expr: Box::new(self.expr(*u.expr)),
                    })
                }
            },
            ExprKind::Binary(b) => ir::ExprKind::Binary(ir::BinaryExpr {
                op: b.op,
                lhs: Box::new(self.expr(*b.lhs)),
                rhs: Box::new(self.expr(*b.rhs)),
            }),
            ExprKind::Assign(_) => {
                self.diagnostics.push(
                    WgslError::new("cannot use assignment as an expression").marker(expr.span),
                );
                ir::ExprKind::Error
            }
            ExprKind::Call(call) => {
                let target = self.call_target(*call.target);
                let args = call.args.into_iter().map(|x| self.expr(x)).collect();
                ir::ExprKind::Call(ir::CallExpr { target, args })
            }
            ExprKind::Index(on, with) => {
                ir::ExprKind::Index(Box::new(self.expr(*on)), Box::new(self.expr(*with)))
            }
            ExprKind::Member(on, member) => ir::ExprKind::Member(Box::new(self.expr(*on)), member),
            ExprKind::Postfix(_) => {
                self.diagnostics.push(
                    WgslError::new("cannot use postfix statement as an expression")
                        .marker(expr.span),
                );
                ir::ExprKind::Error
            }
        };

        ir::Expr {
            kind,
            span: expr.span,
        }
    }

    fn expr_statement(&mut self, expr: ast::Expr) -> Option<ir::ExprStatement> {
        let kind = match expr.kind {
            ExprKind::VarDecl(decl) => {
                let (name, kind) = match *decl {
                    VarDecl::Var(v) => (v.name, ir::VarDeclKind::Var(self.var(v))),
                    VarDecl::Const(c) => (c.name, ir::VarDeclKind::Const(self.let_(c))),
                    VarDecl::Let(l) => (l.name, ir::VarDeclKind::Let(self.let_(l))),
                };

                ir::ExprStatementKind::VarDecl(ir::VarDecl {
                    kind,
                    id: {
                        let id = LocalId(self.locals);
                        self.locals += 1;
                        let old = self
                            .scopes
                            .last_mut()
                            .expect("no scopes")
                            .insert(name.name, (id, name.span, false));

                        if let Some((_, span, _)) = old {
                            self.diagnostics.push(
                                WgslError::new("shadowing is not allowed in the same scope")
                                    .label(span, "previously declared here")
                                    .label(name.span, "redeclared here"),
                            );
                        }

                        id
                    },
                })
            }
            ExprKind::Call(call) => {
                let target = self.call_target(*call.target);
                let args = call.args.into_iter().map(|x| self.expr(x)).collect();
                ir::ExprStatementKind::Call(ir::CallExpr { target, args })
            }
            ExprKind::Assign(assign) => {
                if let ExprKind::Underscore = &assign.lhs.kind {
                    if assign.op.is_none() {
                        ir::ExprStatementKind::Assign(ir::AssignExpr {
                            target: ir::AssignTarget::Phony,
                            value: Box::new(self.expr(*assign.rhs)),
                        })
                    } else {
                        self.diagnostics.push(
                            WgslError::new("`_` is not allowed here").marker(assign.lhs.span),
                        );
                        return None;
                    }
                } else {
                    let lhs = Box::new(self.expr(*assign.lhs));
                    let rhs = Box::new(self.expr(*assign.rhs));

                    let value = match assign.op {
                        Some(op) => Box::new(ir::Expr {
                            kind: ir::ExprKind::Binary(ir::BinaryExpr {
                                lhs: lhs.clone(),
                                op,
                                rhs,
                            }),
                            span: expr.span,
                        }),
                        None => rhs,
                    };

                    let target = ir::AssignTarget::Expr(lhs);

                    ir::ExprStatementKind::Assign(ir::AssignExpr { target, value })
                }
            }
            ExprKind::Postfix(postfix) => {
                let span = postfix.expr.span;
                let expr = Box::new(self.expr(*postfix.expr));

                let target = ir::AssignTarget::Expr(expr.clone());
                let value = Box::new(ir::Expr {
                    kind: ir::ExprKind::Binary(ir::BinaryExpr {
                        op: BinaryOperator::Add,
                        rhs: Box::new(ir::Expr {
                            kind: ir::ExprKind::Literal(ast::Literal::AbstractInt(1)),
                            span,
                        }),
                        lhs: expr,
                    }),
                    span,
                });

                ir::ExprStatementKind::Assign(ir::AssignExpr { target, value })
            }
            _ => {
                self.diagnostics
                    .push(WgslError::new("this expression is not allowed here").marker(expr.span));
                return None;
            }
        };

        Some(ir::ExprStatement {
            kind,
            span: expr.span,
        })
    }

    fn call_target(&mut self, target: ast::Expr) -> FnTarget {
        match target.kind {
            ExprKind::Ident(ident) => {
                let name = ident.name;

                if let Some(decl) = self.index.get(name.name) {
                    self.dependencies.insert(DeclDependency {
                        id: decl,
                        usage: name.span,
                    });
                    FnTarget::Decl(decl)
                } else if let Some(ty) = self.constructible_inbuilt(ident, target.span) {
                    FnTarget::InbuiltType(Box::new(ty))
                } else if let Some(inbuilt) = self.inbuilt_function.get(name.name) {
                    FnTarget::InbuiltFunction(inbuilt)
                } else {
                    self.diagnostics
                        .push(WgslError::new("undefined function").marker(name.span));
                    FnTarget::Error
                }
            }
            _ => {
                self.diagnostics
                    .push(WgslError::new("invalid function call target").marker(target.span));
                FnTarget::Error
            }
        }
    }

    fn constructible_inbuilt(&mut self, ident: ast::IdentExpr, span: Span) -> Option<InbuiltType> {
        let name = ident.name.name;
        Some(if name == self.kws.array {
            if ident.generics.len() > 1 {
                self.diagnostics
                    .push(WgslError::new("too many generics for `array`").marker(ident.name.span));
            }
            let of = ident
                .generics
                .into_iter()
                .next()
                .map(|x| self.ty(x))
                .unwrap_or(ir::Type {
                    kind: ir::TypeKind::Inbuilt(InbuiltType::Infer),
                    span: ident.name.span,
                });
            let len = ident.array_len.map(|x| self.expr(*x));
            InbuiltType::Array {
                of: Box::new(of),
                len,
            }
        } else if let Some(inbuilt) =
            self.constructible_non_array_inbuilt(ident.name, ident.generics, span)
        {
            inbuilt
        } else {
            return None;
        })
    }

    fn inbuilt(&mut self, ty: ast::Type) -> Option<InbuiltType> {
        let span = ty.span;
        let no_generics = |this: &mut Self, generics: Vec<ast::Type>, name: &str| {
            if generics.len() != 0 {
                this.diagnostics.push(
                    WgslError::new(format!("`{}` cannot have generic parameters", name))
                        .marker(span),
                );
            }
        };

        let ty = match ty.kind {
            ast::TypeKind::Ident(ident, generics) => {
                if ident.name == self.kws.atomic {
                    if generics.len() != 1 {
                        self.diagnostics.push(
                            WgslError::new(format!(
                                "`atomic` must have exactly one generic parameter"
                            ))
                            .marker(ty.span),
                        );
                    }

                    if let Some(InbuiltType::Scalar { kind, width }) = generics
                        .into_iter()
                        .next()
                        .map(|x| self.inbuilt(x))
                        .flatten()
                    {
                        InbuiltType::Atomic { kind, width }
                    } else {
                        self.diagnostics.push(
                            WgslError::new(format!(
                                "`atomic` must have a scalar type as its generic parameter",
                            ))
                            .marker(ty.span),
                        );
                        InbuiltType::Atomic {
                            kind: ScalarKind::Sint,
                            width: 4,
                        }
                    }
                } else if ident.name == self.kws.ptr {
                    let mut generics = generics.into_iter();
                    let address_space = generics.next();
                    let to = generics.next().map(|x| self.ty(x));
                    let access_mode = generics.next();

                    let address_space = address_space
                        .and_then(|x| {
                            self.ty_to_ident(x, "address space")
                                .map(|x| (self.address_space(x), x.span))
                        })
                        .and_then(|(a, s)| a.map(|a| (a, s)));

                    let access_mode = access_mode
                        .and_then(|access_mode| {
                            self.ty_to_ident(access_mode, "access mode")
                                .map(|x| (self.access_mode(x), x.span))
                        })
                        .and_then(|(a, s)| a.map(|a| (a, s)));

                    let to = to.unwrap_or_else(|| {
                        self.diagnostics
                            .push(WgslError::new(format!("expected type")).marker(span));
                        ir::Type {
                            kind: ir::TypeKind::Inbuilt(InbuiltType::Scalar {
                                kind: ScalarKind::Sint,
                                width: 4,
                            }),
                            span,
                        }
                    });

                    let address_space = address_space.map(|x| x.0).unwrap_or_else(|| {
                        self.diagnostics
                            .push(WgslError::new(format!("expected address space")).marker(span));
                        AddressSpace::Function
                    });

                    let address_space = if let Some((mode, span)) = access_mode {
                        let mut x = address_space.into();
                        match x {
                            crate::AddressSpace::Storage { ref mut access } => {
                                *access = mode.into()
                            }
                            _ => {
                                self.diagnostics.push(
                                    WgslError::new(format!(
                                        "cannot declare variable with access mode `{}` in address space `{}`",
                                        mode, address_space
                                    ))
                                        .marker(span),
                                );
                            }
                        }

                        x
                    } else {
                        address_space.into()
                    };

                    InbuiltType::Pointer {
                        space: address_space,
                        to: Box::new(to),
                    }
                } else if let Some(s) = self.sampled_texture.get(ident.name) {
                    let name = s.to_static_str();

                    if generics.len() != 1 {
                        self.diagnostics.push(
                            WgslError::new(format!(
                                "`{}` must have exactly 1 generic parameter",
                                name
                            ))
                            .marker(ty.span),
                        );
                    }

                    let sample_type = generics.into_iter().next().and_then(|x| self.inbuilt(x));
                    let kind = match sample_type {
                        Some(InbuiltType::Scalar { kind, .. }) => kind,
                        Some(_) => {
                            self.diagnostics.push(
                                WgslError::new(format!(
                                    "`{}` must have a scalar type as its generic parameter",
                                    name
                                ))
                                .marker(ty.span),
                            );
                            ScalarKind::Float
                        }
                        None => ScalarKind::Float,
                    };

                    let (dim, arrayed, multi) = s.into();
                    InbuiltType::Image {
                        dim,
                        arrayed,
                        class: ImageClass::Sampled { kind, multi },
                    }
                } else if let Some(depth) = self.depth_texture.get(ident.name) {
                    no_generics(self, generics, depth.to_static_str());

                    let (dim, arrayed, multi) = depth.into();
                    InbuiltType::Image {
                        dim,
                        arrayed,
                        class: ImageClass::Depth { multi },
                    }
                } else if let Some(s) = self.storage_texture.get(ident.name) {
                    let name = s.to_static_str();

                    if generics.len() != 2 {
                        self.diagnostics.push(
                            WgslError::new(format!(
                                "`{}` must have exactly 2 generic parameters",
                                name
                            ))
                            .marker(ty.span),
                        );
                    }

                    let mut generics = generics.into_iter();
                    let texel_format = generics
                        .next()
                        .and_then(|x| self.ty_to_ident(x, "texel format"))
                        .and_then(|x| self.texel_format(x))
                        .unwrap_or(TexelFormat::Rgba8Unorm);
                    let access = generics
                        .next()
                        .and_then(|x| self.ty_to_ident(x, "access mode"))
                        .map(|x| (self.access_mode(x), x.span));

                    let access = if let Some((access, span)) = access {
                        let access = access.unwrap_or(AccessMode::Write);
                        if access != AccessMode::Write {
                            self.tu.features.require(
                                Feature::StorageImageRead,
                                span,
                                &mut self.diagnostics,
                            );
                        }
                        access.into()
                    } else {
                        StorageAccess::STORE
                    };

                    let (dim, arrayed) = s.into();
                    InbuiltType::Image {
                        dim,
                        arrayed,
                        class: ImageClass::Storage {
                            format: texel_format.into(),
                            access,
                        },
                    }
                } else if let Some(s) = self.sampler.get(ident.name) {
                    no_generics(self, generics, s.to_static_str());
                    InbuiltType::Sampler {
                        comparison: s.into(),
                    }
                } else if let Some(inbuilt) =
                    self.constructible_non_array_inbuilt(ident, generics, span)
                {
                    inbuilt
                } else {
                    return None;
                }
            }
            ast::TypeKind::Array(array, of, len) => {
                if array.name == self.kws.array {
                    InbuiltType::Array {
                        of: Box::new(self.ty(*of)),
                        len: len.map(|x| self.expr(x)),
                    }
                } else {
                    // Is `binding_array`
                    self.tu.features.require(
                        Feature::BindingArray,
                        array.span,
                        &mut self.diagnostics,
                    );

                    InbuiltType::BindingArray {
                        of: Box::new(self.ty(*of)),
                        len: len.map(|x| self.expr(x)),
                    }
                }
            }
        };

        Some(ty)
    }

    fn constructible_non_array_inbuilt(
        &mut self,
        ident: Ident,
        generics: Vec<ast::Type>,
        span: Span,
    ) -> Option<InbuiltType> {
        let ty = if let Some(prim) = self.primitive.get(ident.name) {
            if generics.len() != 0 {
                self.diagnostics.push(
                    WgslError::new(format!(
                        "`{}` cannot have generic parameters",
                        prim.to_static_str()
                    ))
                    .marker(span),
                );
            }

            match prim {
                PrimitiveType::F64 => {
                    self.tu
                        .features
                        .require(Feature::Float64, ident.span, &mut self.diagnostics)
                }
                PrimitiveType::F16 => {
                    self.tu
                        .features
                        .require(Feature::Float16, ident.span, &mut self.diagnostics)
                }
                _ => {}
            }

            let (kind, width) = prim.into();
            InbuiltType::Scalar { kind, width }
        } else if let Some(comp) = self.vec.get(ident.name) {
            let name = comp.to_static_str();

            if generics.len() != 1 {
                self.diagnostics.push(
                    WgslError::new(format!("`{}` must have exactly 1 generic parameter", name))
                        .marker(span),
                );
            }

            if let Some(InbuiltType::Scalar { kind, width }) = generics
                .into_iter()
                .next()
                .map(|x| self.inbuilt(x))
                .flatten()
            {
                InbuiltType::Vector {
                    kind,
                    width,
                    size: comp.into(),
                }
            } else {
                self.diagnostics.push(
                    WgslError::new(format!(
                        "`{}` must have a scalar type as its generic parameter",
                        name
                    ))
                    .marker(span),
                );
                InbuiltType::Vector {
                    kind: ScalarKind::Sint,
                    width: 4,
                    size: comp.into(),
                }
            }
        } else if let Some(comp) = self.mat.get(ident.name) {
            let name = comp.to_static_str();

            if generics.len() != 1 {
                self.diagnostics.push(
                    WgslError::new(format!("`{}` must have exactly 1 generic parameter", name))
                        .marker(span),
                );
            }

            if let Some(InbuiltType::Scalar { width, kind }) = generics
                .into_iter()
                .next()
                .map(|x| self.inbuilt(x))
                .flatten()
            {
                if kind != ScalarKind::Float {
                    self.diagnostics.push(
                        WgslError::new(format!(
                            "`{}` must have a floating point type as its generic parameter",
                            name
                        ))
                        .marker(span),
                    );
                }

                let (columns, rows) = comp.into();
                InbuiltType::Matrix {
                    columns,
                    rows,
                    width,
                }
            } else {
                self.diagnostics.push(
                    WgslError::new(format!(
                        "`{}` must have a floating-point type as its generic parameter",
                        name
                    ))
                    .marker(span),
                );

                let (columns, rows) = comp.into();
                InbuiltType::Matrix {
                    columns,
                    rows,
                    width: 4,
                }
            }
        } else {
            return None;
        };

        Some(ty)
    }

    fn attrib(&mut self, attrib: ast::Attribute) -> Option<Attribute> {
        let args = |this: &mut Self, args: usize| {
            if attrib.exprs.len() != args {
                this.diagnostics
                    .push(WgslError::new("expected 1 argument").marker(attrib.span));
                None
            } else {
                Some(())
            }
        };
        let expr_as_ident = |this: &mut Self, expr: &ast::Expr| match &expr.kind {
            ExprKind::Ident(i) => {
                if i.generics.len() != 0 || i.array_len.is_some() {
                    this.diagnostics
                        .push(WgslError::new("expected identifier").marker(expr.span));
                }
                Some(i.name)
            }
            _ => None,
        };

        let span = attrib.span;
        let ty = match attrib.name.name {
            x if x == self.kws.align => args(self, 1)
                .map(|_| AttributeType::Align(attrib.exprs.into_iter().next().unwrap())),
            x if x == self.kws.binding => args(self, 1)
                .map(|_| AttributeType::Binding(attrib.exprs.into_iter().next().unwrap())),
            x if x == self.kws.builtin => args(self, 1).and_then(|_| {
                expr_as_ident(self, &attrib.exprs[0])
                    .and_then(|ident| self.builtin(ident).map(|x| AttributeType::Builtin(x)))
            }),
            x if x == self.kws.compute => args(self, 0).map(|_| AttributeType::Compute),
            x if x == self.kws.const_ => args(self, 0).map(|_| AttributeType::Const),
            x if x == self.kws.fragment => args(self, 0).map(|_| AttributeType::Fragment),
            x if x == self.kws.group => args(self, 1)
                .map(|_| AttributeType::Group(attrib.exprs.into_iter().next().unwrap())),
            x if x == self.kws.id => {
                args(self, 1).map(|_| AttributeType::Id(attrib.exprs.into_iter().next().unwrap()))
            }
            x if x == self.kws.interpolate => {
                if attrib.exprs.len() < 1 || attrib.exprs.len() > 2 {
                    self.diagnostics
                        .push(WgslError::new("expected 1 or 2 arguments").marker(attrib.span));
                    None
                } else {
                    let ty = expr_as_ident(self, &attrib.exprs[0])
                        .and_then(|x| self.interpolation_type(x))
                        .unwrap_or(InterpolationType::Perspective);
                    let sample = attrib
                        .exprs
                        .get(1)
                        .and_then(|x| expr_as_ident(self, x))
                        .and_then(|x| self.interpolation_sample(x));

                    if ty == InterpolationType::Flat && sample.is_some() {
                        let span = attrib.exprs[1].span;
                        self.diagnostics.push(
                            WgslError::new("flat interpolation must not have a sample type")
                                .marker(span),
                        );
                    }

                    Some(AttributeType::Interpolate(ty, sample))
                }
            }
            x if x == self.kws.invariant => args(self, 0).map(|_| AttributeType::Invariant),
            x if x == self.kws.location => args(self, 1)
                .map(|_| AttributeType::Location(attrib.exprs.into_iter().next().unwrap())),
            x if x == self.kws.size => {
                args(self, 1).map(|_| AttributeType::Size(attrib.exprs.into_iter().next().unwrap()))
            }
            x if x == self.kws.vertex => args(self, 0).map(|_| AttributeType::Vertex),
            x if x == self.kws.workgroup_size => {
                if attrib.exprs.len() < 1 || attrib.exprs.len() > 3 {
                    self.diagnostics
                        .push(WgslError::new("expected 1, 2, or 3 arguments").marker(attrib.span));
                    None
                } else {
                    let mut iter = attrib.exprs.into_iter();
                    let x = iter.next().unwrap();
                    let y = iter.next();
                    let z = iter.next();
                    Some(AttributeType::WorkgroupSize(x, y, z))
                }
            }
            x if x == self.kws.early_depth_test => args(self, 1).map(|_| {
                AttributeType::ConservativeDepth(
                    expr_as_ident(self, &attrib.exprs.into_iter().next().unwrap())
                        .and_then(|x| self.conservative_depth(x)),
                )
            }),
            _ => {
                self.diagnostics
                    .push(WgslError::new("unknown attribute").marker(attrib.name.span));
                None
            }
        };

        ty.map(|ty| Attribute { span, ty })
    }

    fn access_mode(&mut self, ident: Ident) -> Option<AccessMode> {
        match self.access_mode.get(ident.name) {
            Some(x) => Some(x),
            None => {
                self.diagnostics
                    .push(WgslError::new("unknown access mode").marker(ident.span));
                None
            }
        }
    }

    fn address_space(&mut self, ident: Ident) -> Option<AddressSpace> {
        match self.address_space.get(ident.name) {
            Some(AddressSpace::Handle) => {
                self.diagnostics.push(
                    WgslError::new("`handle` address space is not allowed here")
                        .marker(ident.span)
                        .note("consider removing the address space (`<handle>`)"),
                );
                Some(AddressSpace::Handle)
            }
            Some(x) => Some(x),
            None => {
                self.diagnostics
                    .push(WgslError::new("unknown address space").marker(ident.span));
                None
            }
        }
    }

    fn builtin(&mut self, ident: Ident) -> Option<Builtin> {
        match self.builtin.get(ident.name) {
            Some(x) => Some(x),
            None => {
                self.diagnostics
                    .push(WgslError::new("unknown builtin").marker(ident.span));
                None
            }
        }
    }

    fn interpolation_sample(&mut self, ident: Ident) -> Option<InterpolationSample> {
        match self.interpolation_sample.get(ident.name) {
            Some(x) => Some(x),
            None => {
                self.diagnostics
                    .push(WgslError::new("unknown interpolation sample").marker(ident.span));
                None
            }
        }
    }

    fn interpolation_type(&mut self, ident: Ident) -> Option<InterpolationType> {
        match self.interpolation_type.get(ident.name) {
            Some(x) => Some(x),
            None => {
                self.diagnostics
                    .push(WgslError::new("unknown interpolation type").marker(ident.span));
                None
            }
        }
    }

    fn texel_format(&mut self, ident: Ident) -> Option<TexelFormat> {
        match self.texel_format.get(ident.name) {
            Some(x) => Some(x),
            None => {
                self.diagnostics
                    .push(WgslError::new("unknown texel format").marker(ident.span));
                None
            }
        }
    }

    fn conservative_depth(&mut self, ident: Ident) -> Option<ConservativeDepth> {
        match self.conservative_depth.get(ident.name) {
            Some(x) => Some(x),
            None => {
                self.diagnostics
                    .push(WgslError::new("unknown conservative depth").marker(ident.span));
                None
            }
        }
    }

    fn ty_to_ident(&mut self, ty: ast::Type, expected: &str) -> Option<Ident> {
        match &ty.kind {
            ast::TypeKind::Ident(ident, generics) => {
                if generics.len() != 0 {
                    self.diagnostics
                        .push(WgslError::new(format!("expected {}", expected)).marker(ty.span));
                }
                Some(*ident)
            }
            ast::TypeKind::Array(..) => {
                self.diagnostics
                    .push(WgslError::new(format!("expected {}", expected)).marker(ty.span));
                None
            }
        }
    }

    fn verify_ident(&mut self, ident: Ident) {
        let text = self.intern.resolve(ident.name);
        if let Some(m) = self.reserved_matcher.earliest_find(text) {
            if m.len() == text.len() {
                self.diagnostics.push(
                    WgslError::new("usage of reserved identifier")
                        .marker(ident.span)
                        .note(
                            "this is reserved by the WGSL spec, consider renaming the identifier",
                        ),
                );
            }
        }
    }

    fn resolve_access(&mut self, ident: Ident) -> ir::ExprKind {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((id, _, used)) = scope.get_mut(&ident.name) {
                *used = true;
                return ir::ExprKind::Local(*id);
            }
        }

        if let Some(global) = self.index.get(ident.name) {
            self.dependencies.insert(DeclDependency {
                id: global,
                usage: ident.span,
            });
            ir::ExprKind::Global(global)
        } else {
            self.diagnostics
                .push(WgslError::new("undefined identifier").marker(ident.span));
            ir::ExprKind::Error
        }
    }

    fn pop_scope(&mut self) {
        self.scopes.pop().unwrap();
    }
}

struct Kws {
    align: Text,
    binding: Text,
    builtin: Text,
    compute: Text,
    const_: Text,
    fragment: Text,
    group: Text,
    id: Text,
    interpolate: Text,
    invariant: Text,
    location: Text,
    size: Text,
    vertex: Text,
    workgroup_size: Text,
    early_depth_test: Text,
    array: Text,
    atomic: Text,
    ptr: Text,
}

impl Kws {
    fn init(intern: &mut Interner) -> Self {
        Self {
            align: intern.get_static("align"),
            binding: intern.get_static("binding"),
            builtin: intern.get_static("builtin"),
            compute: intern.get_static("compute"),
            const_: intern.get_static("const"),
            fragment: intern.get_static("fragment"),
            group: intern.get_static("group"),
            id: intern.get_static("id"),
            interpolate: intern.get_static("interpolate"),
            invariant: intern.get_static("invariant"),
            location: intern.get_static("location"),
            size: intern.get_static("size"),
            vertex: intern.get_static("vertex"),
            workgroup_size: intern.get_static("workgroup_size"),
            early_depth_test: intern.get_static("early_depth_test"),
            array: intern.get_static("array"),
            atomic: intern.get_static("atomic"),
            ptr: intern.get_static("ptr"),
        }
    }
}
