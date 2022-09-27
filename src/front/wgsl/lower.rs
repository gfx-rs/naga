use crate::front::wgsl::const_eval::{Evaluator, Value};
use crate::front::wgsl::WgslError;
use crate::front::Typifier;
use crate::proc::{Alignment, Layouter};
use crate::{
    FastHashMap, Handle, ImageClass, ImageDimension, ScalarKind, StorageFormat, TypeInner,
};
use std::fmt::{Display, Formatter};
use wgsl::resolve::inbuilt::{
    AccessMode, AddressSpace, Builtin, ConservativeDepth, DepthTextureType, InterpolationSample,
    InterpolationType, MatType, PrimitiveType, SampledTextureType, SamplerType, StorageTextureType,
    TexelFormat, VecType,
};
use wgsl::resolve::ir::{
    Arg, ArgAttribs, Block, CaseSelector, Decl, DeclId, DeclKind, Expr, ExprKind,
    ExprStatementKind, FloatType, Fn, FnAttribs, InbuiltType, Let, LocalId, SampleType, Stmt,
    StmtKind, Struct, TranslationUnit, Type, TypeKind, Var, VarDeclKind,
};
use wgsl::text::Interner;

pub struct Lowerer<'a> {
    module: crate::Module,
    eval: Evaluator<'a>,
    intern: &'a Interner,
    tu: &'a TranslationUnit,
    errors: Vec<WgslError>,
    decl_map: FastHashMap<DeclId, DeclData>,
    layouter: Layouter,
    typifier: Typifier,
    locals: FastHashMap<LocalId, LocalData>,
}

enum DeclData {
    Function(Handle<crate::Function>),
    Global(Handle<crate::GlobalVariable>),
    Const(Handle<crate::Constant>),
    Type(Handle<crate::Type>),
    Assert,
    Override,
    EntryPoint,
    Error,
}

enum LocalData {
    Variable(Handle<crate::LocalVariable>),
    Let(Handle<crate::Expression>),
}

impl<'a> Lowerer<'a> {
    pub fn new(module: &'a TranslationUnit, intern: &'a Interner) -> Self {
        Self {
            module: crate::Module::default(),
            eval: Evaluator::new(module, intern),
            intern,
            tu: module,
            errors: Vec::new(),
            decl_map: FastHashMap::default(),
            layouter: Layouter::default(),
            typifier: Typifier::default(),
            locals: FastHashMap::default(),
        }
    }

    pub fn lower(mut self) -> Result<crate::Module, Vec<WgslError>> {
        for (id, decl) in self.tu.decls_ordered() {
            let data = self.decl(decl);
            self.decl_map.insert(id, data);
        }

        let eval_errors = self.eval.finish();
        if self.errors.is_empty() && eval_errors.is_empty() {
            Ok(self.module)
        } else {
            self.errors.extend(eval_errors);
            Err(self.errors)
        }
    }

    fn decl(&mut self, decl: &Decl) -> DeclData {
        match decl.kind {
            DeclKind::Fn(ref f) => {
                let handle = self.fn_(f, decl.span.into());
                handle
                    .map(DeclData::Function)
                    .unwrap_or(DeclData::EntryPoint)
            }
            DeclKind::Override(_) => {
                self.errors.push(WgslError {
                    message: "overrides are not supported yet".to_string(),
                    labels: vec![(decl.span.into(), "".to_string())],
                    notes: vec![],
                });
                DeclData::Override
            }
            DeclKind::Var(ref v) => {
                let handle = self.var(v, decl.span.into());
                handle.map(DeclData::Global).unwrap_or(DeclData::Error)
            }
            DeclKind::Const(ref c) => {
                let handle = self.const_(c, decl.span.into());
                handle.map(DeclData::Const).unwrap_or(DeclData::Error)
            }
            DeclKind::StaticAssert(ref expr) => {
                let value = self.eval.as_bool(expr).unwrap_or(true);
                if !value {
                    self.errors.push(WgslError {
                        message: "static assertion failed".to_string(),
                        labels: vec![(decl.span.into(), "".to_string())],
                        notes: vec![],
                    });
                }
                DeclData::Assert
            }
            DeclKind::Struct(ref s) => {
                let handle = self.struct_(s, decl.span.into());
                handle.map(DeclData::Type).unwrap_or(DeclData::Error)
            }
            DeclKind::Type(ref ty) => {
                let handle = self.ty(&ty.ty);
                handle.map(DeclData::Type).unwrap_or(DeclData::Error)
            }
        }
    }

    fn fn_(&mut self, f: &Fn, span: crate::Span) -> Option<Handle<crate::Function>> {
        let name = self.intern.resolve(f.name.name).to_string();
        let is_frag = matches!(f.attribs, FnAttribs::Fragment(_));

        let mut fun = crate::Function {
            name: Some(name.clone()),
            arguments: f
                .args
                .iter()
                .filter_map(|arg| self.arg(arg, is_frag))
                .collect(),
            result: f.ret.as_ref().and_then(|x| {
                Some(crate::FunctionResult {
                    ty: self.ty(x)?,
                    binding: self.binding(&f.ret_attribs, x.span.into(), false),
                })
            }),
            local_variables: Default::default(),
            expressions: Default::default(),
            named_expressions: Default::default(),
            body: Default::default(),
        };

        self.locals.clear();
        let body = self.block(&f.block, &mut fun);
        fun.body = body;

        let entry = match f.attribs {
            FnAttribs::None => {
                return Some(self.module.functions.append(fun, span));
            }
            FnAttribs::Vertex => crate::EntryPoint {
                name: name.clone(),
                stage: crate::ShaderStage::Vertex,
                early_depth_test: None,
                workgroup_size: [0, 0, 0],
                function: fun,
            },
            FnAttribs::Fragment(ref early) => crate::EntryPoint {
                name: name.clone(),
                stage: crate::ShaderStage::Fragment,
                early_depth_test: early.map(|early| crate::EarlyDepthTest {
                    conservative: Some(match early {
                        ConservativeDepth::GreaterEqual => crate::ConservativeDepth::GreaterEqual,
                        ConservativeDepth::LessEqual => crate::ConservativeDepth::LessEqual,
                        ConservativeDepth::Unchanged => crate::ConservativeDepth::Unchanged,
                    }),
                }),
                workgroup_size: [0, 0, 0],
                function: fun,
            },
            FnAttribs::Compute(ref x, ref y, ref z) => crate::EntryPoint {
                name: name.clone(),
                stage: crate::ShaderStage::Compute,
                early_depth_test: None,
                workgroup_size: [
                    x.as_ref()
                        .and_then(|x| self.eval.as_positive_int(&x))
                        .unwrap_or(1),
                    y.as_ref()
                        .and_then(|y| self.eval.as_positive_int(&y))
                        .unwrap_or(1),
                    z.as_ref()
                        .and_then(|z| self.eval.as_positive_int(&z))
                        .unwrap_or(1),
                ],
                function: fun,
            },
        };

        self.module.entry_points.push(entry);
        None
    }

    fn var(&mut self, v: &Var, span: crate::Span) -> Option<Handle<crate::GlobalVariable>> {
        let name = self.intern.resolve(v.inner.name.name).to_string();
        let init = v
            .inner
            .val
            .as_ref()
            .and_then(|x| self.eval.eval(x).map(|v| (v, x.span)));
        let ty = if let Some(ref ty) = v.inner.ty {
            self.ty(ty)?
        } else if let Some((init, span)) = init {
            self.val_to_ty(init, span.into())
        } else {
            self.errors.push(WgslError {
                message: "global variable must have a type or an initializer".to_string(),
                labels: vec![(span, "".to_string())],
                notes: vec![],
            });
            return None;
        };

        let binding = if let Some(ref binding) = v.attribs.binding {
            if let Some(ref group) = v.attribs.group {
                let binding = self.eval.as_positive_int(binding).unwrap_or(0);
                let group = self.eval.as_positive_int(group).unwrap_or(0);
                Some(crate::ResourceBinding { binding, group })
            } else {
                self.errors.push(WgslError {
                    message: "resource variable must have both binding and group".to_string(),
                    labels: vec![(span, "".to_string())],
                    notes: vec![],
                });
                None
            }
        } else {
            None
        };

        let var = crate::GlobalVariable {
            name: Some(name),
            space: self.address_space(v.inner.address_space, v.inner.access_mode),
            binding,
            ty,
            init: init.map(|(v, span)| self.val_to_const(v, span.into())),
        };

        Some(self.module.global_variables.append(var, span))
    }

    fn struct_(&mut self, s: &Struct, span: crate::Span) -> Option<Handle<crate::Type>> {
        let name = self.intern.resolve(s.name.name).to_string();

        let mut members = Vec::with_capacity(s.fields.len());
        let mut offset = 0;
        let mut alignment = Alignment::ONE;

        for field in s.fields.iter() {
            let name = self.intern.resolve(field.name.name).to_string();
            let ty = self.ty(&field.ty)?;
            let binding = self.binding(&field.attribs.arg, field.name.span.into(), true);

            self.layouter
                .update(&self.module.types, &self.module.constants)
                .unwrap();

            let min_align = self.layouter[ty].alignment;
            let min_size = self.layouter[ty].size;

            let align = field
                .attribs
                .align
                .as_ref()
                .and_then(|x| self.eval.as_positive_int(x));
            let size = field
                .attribs
                .size
                .as_ref()
                .and_then(|x| self.eval.as_positive_int(x))
                .unwrap_or(min_size);

            if size < min_size {
                self.errors.push(WgslError {
                    message: "size attribute is too small".to_string(),
                    labels: vec![
                        (field.name.span.into(), format!("set size is `{}`", size)),
                        (field.ty.span.into(), format!("type size is `{}`", min_size)),
                    ],
                    notes: vec![],
                });
            }

            let align = if let Some(align) = align {
                if let Some(align) = Alignment::new(align) {
                    if align >= min_align {
                        align
                    } else {
                        self.errors.push(WgslError {
                            message: "alignment attribute is too small".to_string(),
                            labels: vec![
                                (
                                    field.name.span.into(),
                                    format!("set alignment is `{}`", align),
                                ),
                                (
                                    field.ty.span.into(),
                                    format!("type alignment is `{}`", min_align),
                                ),
                            ],
                            notes: vec![],
                        });
                        min_align
                    }
                } else {
                    self.errors.push(WgslError {
                        message: "alignment must be a power of two".to_string(),
                        labels: vec![(field.name.span.into(), format!("set to `{}`", align))],
                        notes: vec![],
                    });
                    min_align
                }
            } else {
                min_align
            };

            offset = align.round_up(offset);
            alignment = alignment.max(align);

            members.push(crate::StructMember {
                name: Some(name),
                binding,
                ty,
                offset,
            });

            offset += size;
        }

        let ty = crate::Type {
            name: Some(name),
            inner: TypeInner::Struct {
                members,
                span: alignment.round_up(offset),
            },
        };

        Some(self.module.types.insert(ty, span))
    }

    fn const_(&mut self, c: &Let, span: crate::Span) -> Option<Handle<crate::Constant>> {
        let ident = self.intern.resolve(c.name.name).to_string();
        let value = self.eval.eval(&c.val)?;
        let (width, value) = self.val_to_scalar(value);

        let constant = crate::Constant {
            name: Some(ident),
            specialization: None,
            inner: crate::ConstantInner::Scalar { width, value },
        };
        Some(self.module.constants.append(constant, span))
    }

    fn arg(&mut self, arg: &Arg, is_frag: bool) -> Option<crate::FunctionArgument> {
        Some(crate::FunctionArgument {
            name: Some(self.intern.resolve(arg.name.name).to_string()),
            ty: self.ty(&arg.ty)?,
            binding: self.binding(&arg.attribs, arg.span.into(), is_frag),
        })
    }

    fn block(&mut self, b: &Block, fun: &mut crate::Function) -> crate::Block {
        let mut block = crate::Block::with_capacity(b.stmts.len());

        for stmt in b.stmts.iter() {
            self.stmt(stmt, &mut block, fun);
        }

        block
    }

    fn stmt(&mut self, s: &Stmt, b: &mut crate::Block, fun: &mut crate::Function) -> Option<()> {
        let stmt = match s.kind {
            StmtKind::Expr(ref k) => return Some(self.expr_stmt(k, s.span.into(), b, fun)),
            StmtKind::Block(ref b) => crate::Statement::Block(self.block(b, fun)),
            StmtKind::Break => crate::Statement::Break,
            StmtKind::Continue => crate::Statement::Continue,
            StmtKind::Discard => crate::Statement::Kill,
            StmtKind::For(ref f) => {
                let mut block = crate::Block::with_capacity(2);
                if let Some(ref x) = f.init {
                    self.expr_stmt(&x.kind, x.span.into(), &mut block, fun);
                }

                let mut body = crate::Block::with_capacity(2);
                if let Some(ref x) = f.cond {
                    let condition = self.expr(&x, &mut body, fun)?;
                    body.push(
                        crate::Statement::If {
                            condition,
                            accept: crate::Block::new(),
                            reject: {
                                let mut b = crate::Block::new();
                                b.push(crate::Statement::Break, x.span.into());
                                b
                            },
                        },
                        x.span.into(),
                    );
                }
                body.push(
                    crate::Statement::Block(self.block(&f.block, fun)),
                    f.block.span.into(),
                );

                let mut continuing = crate::Block::new();
                if let Some(ref x) = f.update {
                    self.expr_stmt(&x.kind, x.span.into(), &mut continuing, fun)
                }

                block.push(
                    crate::Statement::Loop {
                        body,
                        continuing,
                        break_if: None,
                    },
                    s.span.into(),
                );

                crate::Statement::Block(block)
            }
            StmtKind::If(ref i) => {
                let condition = self.expr(&i.cond, b, fun)?;
                let accept = self.block(&i.block, fun);
                let reject = i
                    .else_
                    .as_ref()
                    .map(|stmt| {
                        let mut b = crate::Block::with_capacity(1);
                        self.stmt(&stmt, &mut b, fun);
                        b
                    })
                    .unwrap_or_default();

                crate::Statement::If {
                    condition,
                    accept,
                    reject,
                }
            }
            StmtKind::Loop(ref l) => {
                let continuing = l.stmts.last().and_then(|x| match x.kind {
                    StmtKind::Continuing(ref b) => Some(b),
                    _ => None,
                });
                let break_if = continuing
                    .and_then(|b| b.stmts.last())
                    .and_then(|x| match x.kind {
                        StmtKind::BreakIf(ref e) => Some(e),
                        _ => None,
                    });

                let len = continuing
                    .map(|_| l.stmts.len() - 1)
                    .unwrap_or(l.stmts.len());
                let stmts = l.stmts.iter().take(len);
                let mut body = crate::Block::with_capacity(len);
                for stmt in stmts {
                    self.stmt(stmt, &mut body, fun);
                }

                let (continuing, break_if) = if let Some(continuing) = continuing {
                    let len = break_if
                        .map(|_| continuing.stmts.len() - 1)
                        .unwrap_or(continuing.stmts.len());
                    let stmts = continuing.stmts.iter().take(len);
                    let mut cont = crate::Block::with_capacity(len);
                    for stmt in stmts {
                        self.stmt(stmt, &mut cont, fun);
                    }

                    let b = break_if.map(|x| self.expr(x, &mut cont, fun))?;

                    (cont, b)
                } else {
                    (crate::Block::new(), None)
                };

                crate::Statement::Loop {
                    body,
                    continuing,
                    break_if,
                }
            }
            StmtKind::Return(ref e) => crate::Statement::Return {
                value: e.as_ref().and_then(|x| self.expr(x, b, fun)),
            },
            StmtKind::StaticAssert(ref expr) => {
                if let Some(value) = self.eval.as_bool(expr) {
                    if !value {
                        self.errors.push(WgslError {
                            message: "static assertion failed".to_string(),
                            labels: vec![(expr.span.into(), "".to_string())],
                            notes: vec![],
                        });
                    }
                }

                return None;
            }
            StmtKind::Switch(ref s) => {
                let selector = self.expr(&s.expr, b, fun)?;
                let cases = s
                    .cases
                    .iter()
                    .flat_map(|x| {
                        let block = self.block(&x.block, fun);
                        let this = &mut *self;
                        x.selectors
                            .iter()
                            .filter_map(move |sel| {
                                let value = match sel {
                                    CaseSelector::Expr(e) => {
                                        let value = this.eval.as_int(e)?;
                                        crate::SwitchValue::Integer(value)
                                    }
                                    CaseSelector::Default => crate::SwitchValue::Default,
                                };
                                Some(crate::SwitchCase {
                                    value,
                                    body: block.clone(),
                                    fall_through: false,
                                })
                            })
                            .collect::<Vec<_>>()
                    })
                    .collect();
                crate::Statement::Switch { selector, cases }
            }
            StmtKind::While(ref w) => {
                let mut body = crate::Block::with_capacity(3);
                let condition = self.expr(&w.cond, &mut body, fun)?;

                body.push(
                    crate::Statement::If {
                        condition,
                        accept: crate::Block::new(),
                        reject: {
                            let mut b = crate::Block::new();
                            b.push(crate::Statement::Break, w.cond.span.into());
                            b
                        },
                    },
                    w.cond.span.into(),
                );

                let b = self.block(&w.block, fun);
                body.push(crate::Statement::Block(b), w.block.span.into());

                crate::Statement::Loop {
                    body,
                    continuing: crate::Block::new(),
                    break_if: None,
                }
            }
            StmtKind::Continuing(_) | StmtKind::BreakIf(_) => {
                unreachable!("continuing or break if should've been handled in the parent loop");
            }
        };
        b.push(stmt, s.span.into());
        None
    }

    fn expr_stmt(
        &mut self,
        s: &ExprStatementKind,
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) {
        match *s {
            ExprStatementKind::VarDecl(ref decl) => match decl.kind {
                VarDeclKind::Var(_) => {}
                VarDeclKind::Const(_) => {}
                VarDeclKind::Let(_) => {}
            },
            ExprStatementKind::Call(ref call) => {}
            ExprStatementKind::Assign(_) => {}
            ExprStatementKind::Postfix(_) => {}
        }
    }

    fn expr(
        &mut self,
        e: &Expr,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let start = fun.expressions.len();
        let handle = self.expr_inner(e, fun)?;
        let range = fun.expressions.range_from(start);
        b.push(crate::Statement::Emit(range), e.span.into());
        Some(handle)
    }

    fn expr_inner(
        &mut self,
        e: &Expr,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        match e.kind {
            ExprKind::Error => {}
            ExprKind::Literal(_) => {}
            ExprKind::Local(_) => {}
            ExprKind::Global(_) => {}
            ExprKind::Unary(_) => {}
            ExprKind::Binary(_) => {}
            ExprKind::Call(_) => {}
            ExprKind::Index(_, _) => {}
            ExprKind::Member(_, _) => {}
        }

        None
    }

    fn binding(
        &mut self,
        attribs: &ArgAttribs,
        span: crate::Span,
        is_frag: bool,
    ) -> Option<crate::Binding> {
        if let Some(ref builtin) = attribs.builtin {
            if attribs.location.is_some() || attribs.interpolate.is_some() {
                self.errors.push(WgslError {
                    message: "builtin arguments cannot have location, interpolation, or invariant"
                        .to_string(),
                    labels: vec![(span, "".to_string())],
                    notes: vec![],
                });
            }

            let mut check_invariant = true;
            let ret = crate::Binding::BuiltIn(match builtin {
                Builtin::FragDepth => crate::BuiltIn::FragDepth,
                Builtin::FrontFacing => crate::BuiltIn::FrontFacing,
                Builtin::GlobalInvocationId => crate::BuiltIn::GlobalInvocationId,
                Builtin::InstanceIndex => crate::BuiltIn::InstanceIndex,
                Builtin::LocalInvocationId => crate::BuiltIn::LocalInvocationId,
                Builtin::LocalInvocationIndex => crate::BuiltIn::LocalInvocationIndex,
                Builtin::NumWorkgroups => crate::BuiltIn::NumWorkGroups,
                Builtin::Position => crate::BuiltIn::Position {
                    invariant: {
                        check_invariant = false;
                        attribs.invariant
                    },
                },
                Builtin::SampleIndex => crate::BuiltIn::SampleIndex,
                Builtin::SampleMask => crate::BuiltIn::SampleMask,
                Builtin::VertexIndex => crate::BuiltIn::VertexIndex,
                Builtin::WorkgroupId => crate::BuiltIn::WorkGroupId,
                Builtin::PrimitiveIndex => crate::BuiltIn::PrimitiveIndex,
                Builtin::ViewIndex => crate::BuiltIn::ViewIndex,
            });

            if check_invariant && attribs.invariant {
                self.errors.push(WgslError {
                    message: "only position builtin can be invariant".to_string(),
                    labels: vec![(span, "".to_string())],
                    notes: vec![],
                });
            }

            Some(ret)
        } else if let Some(ref loc) = attribs.location {
            let location = self.eval.as_positive_int(loc)?;
            let (interpolation, sampling) =
                if let Some((interpolate, sampling)) = attribs.interpolate {
                    (
                        Some(match interpolate {
                            InterpolationType::Flat => crate::Interpolation::Flat,
                            InterpolationType::Linear => crate::Interpolation::Linear,
                            InterpolationType::Perspective => crate::Interpolation::Perspective,
                        }),
                        Some(match sampling {
                            InterpolationSample::Center => crate::Sampling::Center,
                            InterpolationSample::Centroid => crate::Sampling::Centroid,
                            InterpolationSample::Sample => crate::Sampling::Sample,
                        }),
                    )
                } else {
                    if is_frag {
                        (
                            Some(crate::Interpolation::Perspective),
                            Some(crate::Sampling::Center),
                        )
                    } else {
                        (None, None)
                    }
                };

            Some(crate::Binding::Location {
                location,
                interpolation,
                sampling,
            })
        } else {
            None
        }
    }

    fn primitive_ty(&mut self, ty: &PrimitiveType) -> (ScalarKind, crate::Bytes) {
        match ty {
            PrimitiveType::I32 => (ScalarKind::Sint, 4),
            PrimitiveType::U32 => (ScalarKind::Uint, 4),
            PrimitiveType::F64 => (ScalarKind::Float, 8),
            PrimitiveType::F32 => (ScalarKind::Float, 4),
            PrimitiveType::F16 => (ScalarKind::Float, 2),
            PrimitiveType::Bool => (ScalarKind::Bool, 1),
            PrimitiveType::Infer => unreachable!("cannot infer type here"),
        }
    }

    fn vec_type(&mut self, ty: &VecType) -> crate::VectorSize {
        match ty {
            VecType::Vec2 => crate::VectorSize::Bi,
            VecType::Vec3 => crate::VectorSize::Tri,
            VecType::Vec4 => crate::VectorSize::Quad,
        }
    }

    fn mat_type(&mut self, ty: &MatType) -> (crate::VectorSize, crate::VectorSize) {
        match ty {
            MatType::Mat2x2 => (crate::VectorSize::Bi, crate::VectorSize::Bi),
            MatType::Mat2x3 => (crate::VectorSize::Bi, crate::VectorSize::Tri),
            MatType::Mat2x4 => (crate::VectorSize::Bi, crate::VectorSize::Quad),
            MatType::Mat3x2 => (crate::VectorSize::Tri, crate::VectorSize::Bi),
            MatType::Mat3x3 => (crate::VectorSize::Tri, crate::VectorSize::Tri),
            MatType::Mat3x4 => (crate::VectorSize::Tri, crate::VectorSize::Quad),
            MatType::Mat4x2 => (crate::VectorSize::Quad, crate::VectorSize::Bi),
            MatType::Mat4x3 => (crate::VectorSize::Quad, crate::VectorSize::Tri),
            MatType::Mat4x4 => (crate::VectorSize::Quad, crate::VectorSize::Quad),
        }
    }

    fn ty(&mut self, ty: &Type) -> Option<Handle<crate::Type>> {
        match ty.kind {
            TypeKind::Inbuilt(ref inbuilt) => {
                let inner = match inbuilt {
                    InbuiltType::Primitive(prim) => {
                        let (kind, width) = self.primitive_ty(prim);
                        crate::TypeInner::Scalar { kind, width }
                    }
                    InbuiltType::Vec { ty, comp } => {
                        let (kind, width) = self.primitive_ty(ty);
                        crate::TypeInner::Vector {
                            size: self.vec_type(comp),
                            kind,
                            width,
                        }
                    }
                    InbuiltType::Mat { ty, comp } => {
                        let width = match ty {
                            FloatType::F64 => 8,
                            FloatType::F32 => 4,
                            FloatType::F16 => 2,
                            FloatType::Infer => unreachable!("cannot infer type here"),
                        };
                        let (columns, rows) = self.mat_type(comp);
                        crate::TypeInner::Matrix {
                            columns,
                            rows,
                            width,
                        }
                    }
                    InbuiltType::SampledTexture(tex_ty, sample_ty) => {
                        let (dim, arrayed, multi) = match tex_ty {
                            SampledTextureType::Texture1d => {
                                (crate::ImageDimension::D1, false, false)
                            }
                            SampledTextureType::Texture1dArray => {
                                (crate::ImageDimension::D1, true, false)
                            }
                            SampledTextureType::Texture2d => {
                                (crate::ImageDimension::D2, false, false)
                            }
                            SampledTextureType::TextureMultisampled2d => {
                                (crate::ImageDimension::D2, false, true)
                            }
                            SampledTextureType::Texture2dArray => {
                                (crate::ImageDimension::D2, true, false)
                            }
                            SampledTextureType::Texture3d => {
                                (crate::ImageDimension::D3, false, false)
                            }
                            SampledTextureType::TextureCube => {
                                (crate::ImageDimension::Cube, false, false)
                            }
                            SampledTextureType::TextureCubeArray => {
                                (crate::ImageDimension::Cube, true, false)
                            }
                        };

                        let kind = match sample_ty {
                            SampleType::F64 => crate::ScalarKind::Float,
                            SampleType::F32 => crate::ScalarKind::Float,
                            SampleType::I32 => crate::ScalarKind::Sint,
                            SampleType::U32 => crate::ScalarKind::Uint,
                        };

                        crate::TypeInner::Image {
                            dim,
                            arrayed,
                            class: crate::ImageClass::Sampled { kind, multi },
                        }
                    }
                    InbuiltType::DepthTexture(ty) => {
                        let (dim, arrayed, multi) = match ty {
                            DepthTextureType::Depth2d => (crate::ImageDimension::D2, false, false),
                            DepthTextureType::Depth2dArray => {
                                (crate::ImageDimension::D2, true, false)
                            }
                            DepthTextureType::DepthCube => {
                                (crate::ImageDimension::Cube, false, false)
                            }
                            DepthTextureType::DepthCubeArray => {
                                (crate::ImageDimension::Cube, true, false)
                            }
                            DepthTextureType::DepthMultisampled2d => {
                                (crate::ImageDimension::D2, false, true)
                            }
                        };

                        crate::TypeInner::Image {
                            dim,
                            arrayed,
                            class: crate::ImageClass::Depth { multi },
                        }
                    }
                    InbuiltType::StorageTexture(ty, format, mode) => {
                        let (dim, arrayed) = match ty {
                            StorageTextureType::Storage1d => (crate::ImageDimension::D1, false),
                            StorageTextureType::Storage1dArray => (crate::ImageDimension::D1, true),
                            StorageTextureType::Storage2d => (crate::ImageDimension::D2, false),
                            StorageTextureType::Storage2dArray => (crate::ImageDimension::D2, true),
                            StorageTextureType::Storage3d => (crate::ImageDimension::D3, false),
                        };

                        let format = match format {
                            TexelFormat::R32Float => crate::StorageFormat::R32Float,
                            TexelFormat::R32Sint => crate::StorageFormat::R32Sint,
                            TexelFormat::R32Uint => crate::StorageFormat::R32Uint,
                            TexelFormat::Rg32Float => crate::StorageFormat::Rg32Float,
                            TexelFormat::Rg32Sint => crate::StorageFormat::Rg32Sint,
                            TexelFormat::Rg32Uint => crate::StorageFormat::Rg32Uint,
                            TexelFormat::Rgba16Float => crate::StorageFormat::Rgba16Float,
                            TexelFormat::Rgba16Sint => crate::StorageFormat::Rgba16Sint,
                            TexelFormat::Rgba16Uint => crate::StorageFormat::Rgba16Uint,
                            TexelFormat::Rgba32Float => crate::StorageFormat::Rgba32Float,
                            TexelFormat::Rgba32Sint => crate::StorageFormat::Rgba32Sint,
                            TexelFormat::Rgba32Uint => crate::StorageFormat::Rgba32Uint,
                            TexelFormat::Rgba8Sint => crate::StorageFormat::Rgba8Sint,
                            TexelFormat::Rgba8Uint => crate::StorageFormat::Rgba8Uint,
                            TexelFormat::Rgba8Unorm => crate::StorageFormat::Rgba8Unorm,
                            TexelFormat::Rgba8Snorm => crate::StorageFormat::Rgba8Snorm,
                        };

                        crate::TypeInner::Image {
                            dim,
                            arrayed,
                            class: crate::ImageClass::Storage {
                                format,
                                access: self.access_mode(*mode),
                            },
                        }
                    }
                    InbuiltType::Sampler(ty) => crate::TypeInner::Sampler {
                        comparison: match ty {
                            SamplerType::Sampler => false,
                            SamplerType::SamplerComparison => true,
                        },
                    },
                    InbuiltType::Array { of, len } => {
                        let base = self.ty(&of)?;
                        self.layouter
                            .update(&self.module.types, &self.module.constants)
                            .unwrap();
                        crate::TypeInner::Array {
                            base,
                            size: len
                                .as_ref()
                                .and_then(|x| self.constant(x).map(crate::ArraySize::Constant))
                                .unwrap_or(crate::ArraySize::Dynamic),
                            stride: self.layouter[base].to_stride(),
                        }
                    }
                    InbuiltType::BindingArray { of, len } => {
                        let base = self.ty(&of)?;
                        crate::TypeInner::BindingArray {
                            base,
                            size: len
                                .as_ref()
                                .and_then(|x| self.constant(x).map(crate::ArraySize::Constant))
                                .unwrap_or(crate::ArraySize::Dynamic),
                        }
                    }
                    InbuiltType::Ptr {
                        to,
                        address_space,
                        access_mode,
                    } => crate::TypeInner::Pointer {
                        base: self.ty(&to)?,
                        space: self.address_space(*address_space, *access_mode),
                    },
                    InbuiltType::Atomic { signed } => crate::TypeInner::Atomic {
                        kind: if *signed {
                            crate::ScalarKind::Sint
                        } else {
                            crate::ScalarKind::Uint
                        },
                        width: 4,
                    },
                };

                Some(
                    self.module.types.insert(
                        crate::Type {
                            name: Some(
                                TypeFormatter {
                                    ty: &inner,
                                    types: &self.module.types,
                                    constants: &self.module.constants,
                                }
                                .to_string(),
                            ),
                            inner,
                        },
                        crate::Span::UNDEFINED,
                    ),
                )
            }
            TypeKind::User(ref id) => match self.decl_map[id] {
                DeclData::Type(handle) => Some(handle),
                _ => {
                    self.errors.push(WgslError {
                        message: "expected a type".to_string(),
                        labels: vec![(ty.span.into(), "".to_string())],
                        notes: vec![],
                    });
                    None
                }
            },
        }
    }

    fn constant(&mut self, expr: &Expr) -> Option<Handle<crate::Constant>> {
        let value = self.eval.eval(expr)?;
        Some(self.val_to_const(value, expr.span.into()))
    }

    fn val_to_const(&mut self, value: Value, span: crate::Span) -> Handle<crate::Constant> {
        let (width, value) = self.val_to_scalar(value);

        let value = crate::Constant {
            name: None,
            specialization: None,
            inner: crate::ConstantInner::Scalar { width, value },
        };

        self.module.constants.append(value, span)
    }

    fn val_to_ty(&mut self, value: Value, span: crate::Span) -> Handle<crate::Type> {
        let (width, value) = self.val_to_scalar(value);
        let ty = crate::Type {
            name: None,
            inner: crate::TypeInner::Scalar {
                kind: value.scalar_kind(),
                width,
            },
        };

        self.module.types.insert(ty, span)
    }

    fn val_to_scalar(&mut self, value: Value) -> (crate::Bytes, crate::ScalarValue) {
        match value {
            Value::Bool(b) => (1, crate::ScalarValue::Bool(b)),
            Value::AbstractInt(i) => (4, crate::ScalarValue::Sint(i)), // Concretize to `i32`.
            Value::I32(i) => (4, crate::ScalarValue::Sint(i as _)),
            Value::U32(u) => (4, crate::ScalarValue::Uint(u as _)),
            Value::AbstractFloat(f) => (4, crate::ScalarValue::Float(f)), // Concretize to `f32`.
            Value::F32(f) => (4, crate::ScalarValue::Float(f as _)),
            Value::F64(f) => (8, crate::ScalarValue::Float(f)),
        }
    }

    fn address_space(&mut self, space: AddressSpace, access: AccessMode) -> crate::AddressSpace {
        match space {
            AddressSpace::Function => crate::AddressSpace::Function,
            AddressSpace::Private => crate::AddressSpace::Private,
            AddressSpace::Storage => crate::AddressSpace::Storage {
                access: self.access_mode(access),
            },
            AddressSpace::Uniform => crate::AddressSpace::Uniform,
            AddressSpace::Workgroup => crate::AddressSpace::WorkGroup,
            AddressSpace::Handle => crate::AddressSpace::Handle,
            AddressSpace::PushConstant => crate::AddressSpace::PushConstant,
        }
    }

    fn access_mode(&mut self, access: AccessMode) -> crate::StorageAccess {
        match access {
            AccessMode::Read => crate::StorageAccess::LOAD,
            AccessMode::Write => crate::StorageAccess::STORE,
            AccessMode::ReadWrite => crate::StorageAccess::LOAD | crate::StorageAccess::STORE,
        }
    }
}

struct TypeFormatter<'a> {
    ty: &'a TypeInner,
    types: &'a crate::UniqueArena<crate::Type>,
    constants: &'a crate::Arena<crate::Constant>,
}

impl Display for TypeFormatter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self.ty {
            TypeInner::Scalar { kind, width } => ScalarFormatter { kind, width }.fmt(f),
            TypeInner::Vector { size, kind, width } => write!(
                f,
                "vec{}<{}>",
                VectorSizeFormatter { size },
                ScalarFormatter { kind, width }
            ),
            TypeInner::Matrix {
                columns,
                rows,
                width,
            } => write!(
                f,
                "mat{}x{}<{}>",
                columns as u8,
                VectorSizeFormatter { size: rows },
                ScalarFormatter {
                    kind: crate::ScalarKind::Float,
                    width
                }
            ),
            TypeInner::Atomic { kind, width } => {
                write!(f, "atomic<{}>", ScalarFormatter { kind, width })
            }
            TypeInner::Pointer { base, space } => write!(
                f,
                "ptr<{}, {}>",
                match space {
                    crate::AddressSpace::Function => "function",
                    crate::AddressSpace::Private => "private",
                    crate::AddressSpace::WorkGroup => "workgroup",
                    crate::AddressSpace::Uniform => "uniform",
                    crate::AddressSpace::Storage { .. } => "storage",
                    crate::AddressSpace::Handle => "handle",
                    crate::AddressSpace::PushConstant => "push_constant",
                },
                self.types
                    .get_handle(base)
                    .unwrap()
                    .name
                    .as_ref()
                    .expect("created type without name"),
            ),
            TypeInner::ValuePointer { .. } => {
                panic!("TypeInner::ValuePointer should not be formatted by the frontend")
            }
            TypeInner::Array { base, size, .. } => {
                let base = self
                    .types
                    .get_handle(base)
                    .unwrap()
                    .name
                    .as_ref()
                    .expect("created type without name");
                match size {
                    crate::ArraySize::Constant(c) => write!(
                        f,
                        "array<{}, {}>",
                        base,
                        match self.constants[c].inner {
                            crate::ConstantInner::Scalar {
                                value: crate::ScalarValue::Uint(u),
                                ..
                            } => u,
                            _ => panic!("Array size should be a constant"),
                        }
                    ),
                    crate::ArraySize::Dynamic => write!(f, "array<{}>", base),
                }
            }
            TypeInner::Struct { .. } => {
                panic!("TypeInner::Struct should not be formatted by the frontend")
            }
            TypeInner::Image {
                dim,
                arrayed,
                class,
            } => {
                let dim = match dim {
                    ImageDimension::D1 => "1d",
                    ImageDimension::D2 => "2d",
                    ImageDimension::D3 => "3d",
                    ImageDimension::Cube => "cube",
                };
                let arrayed = if arrayed { "_array" } else { "" };
                match class {
                    ImageClass::Sampled { kind, multi } => {
                        let multi = if multi { "multisampled_" } else { "" };
                        write!(
                            f,
                            "texture_{}{}{}<{}>",
                            multi,
                            dim,
                            arrayed,
                            match kind {
                                ScalarKind::Sint => "int",
                                ScalarKind::Uint => "uint",
                                ScalarKind::Float => "float",
                                ScalarKind::Bool => "bool",
                            }
                        )
                    }
                    ImageClass::Depth { multi } => {
                        let multi = if multi { "multisampled_" } else { "" };
                        write!(f, "texture_depth_{}{}{}", multi, dim, arrayed)
                    }
                    ImageClass::Storage { format, access } => {
                        write!(
                            f,
                            "texture_storage_{}{}<{}, {}>",
                            dim,
                            arrayed,
                            match format {
                                StorageFormat::R8Unorm => "r8unorm",
                                StorageFormat::R8Snorm => "r8snorm",
                                StorageFormat::R8Uint => "r8uint",
                                StorageFormat::R8Sint => "r8sint",
                                StorageFormat::R16Uint => "r16uint",
                                StorageFormat::R16Sint => "r16sint",
                                StorageFormat::R16Float => "r16float",
                                StorageFormat::Rg8Unorm => "rg8unorm",
                                StorageFormat::Rg8Snorm => "rg8snorm",
                                StorageFormat::Rg8Uint => "rg8uint",
                                StorageFormat::Rg8Sint => "rg8sint",
                                StorageFormat::R32Uint => "r32uint",
                                StorageFormat::R32Sint => "r32sint",
                                StorageFormat::R32Float => "r32float",
                                StorageFormat::Rg16Uint => "rg16uint",
                                StorageFormat::Rg16Sint => "rg16sint",
                                StorageFormat::Rg16Float => "rg16float",
                                StorageFormat::Rgba8Unorm => "rgba8unorm",
                                StorageFormat::Rgba8Snorm => "rgba8snorm",
                                StorageFormat::Rgba8Uint => "rgba8uint",
                                StorageFormat::Rgba8Sint => "rgba8sint",
                                StorageFormat::Rgb10a2Unorm => "rgb10a2unorm",
                                StorageFormat::Rg11b10Float => "rg11b10float",
                                StorageFormat::Rg32Uint => "rg32uint",
                                StorageFormat::Rg32Sint => "rg32sint",
                                StorageFormat::Rg32Float => "rg32float",
                                StorageFormat::Rgba16Uint => "rgba16uint",
                                StorageFormat::Rgba16Sint => "rgba16sint",
                                StorageFormat::Rgba16Float => "rgba16float",
                                StorageFormat::Rgba32Uint => "rgba32uint",
                                StorageFormat::Rgba32Sint => "rgba32sint",
                                StorageFormat::Rgba32Float => "rgba32float",
                            },
                            if access
                                .contains(crate::StorageAccess::STORE | crate::StorageAccess::LOAD)
                            {
                                "read_write"
                            } else if access.contains(crate::StorageAccess::STORE) {
                                "write"
                            } else {
                                "read"
                            }
                        )
                    }
                }
            }
            TypeInner::Sampler { comparison } => write!(
                f,
                "{}",
                if comparison {
                    "sampler_comparison"
                } else {
                    "sampler"
                }
            ),
            TypeInner::BindingArray { base, size } => {
                let base = self
                    .types
                    .get_handle(base)
                    .unwrap()
                    .name
                    .as_ref()
                    .expect("created type without name");
                match size {
                    crate::ArraySize::Constant(c) => write!(
                        f,
                        "binding_array<{}, {}>",
                        base,
                        match self.constants[c].inner {
                            crate::ConstantInner::Scalar {
                                value: crate::ScalarValue::Uint(u),
                                ..
                            } => u,
                            _ => panic!("Array size should be a constant"),
                        }
                    ),
                    crate::ArraySize::Dynamic => write!(f, "binding_array<{}>", base),
                }
            }
        }
    }
}

struct ScalarFormatter {
    kind: ScalarKind,
    width: crate::Bytes,
}

impl Display for ScalarFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            crate::ScalarKind::Sint => write!(f, "i{}", self.width * 8),
            crate::ScalarKind::Uint => write!(f, "u{}", self.width * 8),
            crate::ScalarKind::Float => write!(f, "f{}", self.width * 8),
            crate::ScalarKind::Bool => write!(f, "bool"),
        }
    }
}

struct VectorSizeFormatter {
    size: crate::VectorSize,
}

impl Display for VectorSizeFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.size {
            crate::VectorSize::Bi => write!(f, "2"),
            crate::VectorSize::Tri => write!(f, "3"),
            crate::VectorSize::Quad => write!(f, "4"),
        }
    }
}
