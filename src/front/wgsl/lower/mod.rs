use crate::front::wgsl::lower::const_eval::{Evaluator, Value};
use crate::front::wgsl::lower::format::TypeFormatter;
use crate::front::wgsl::resolve::ir::{
    Arg, Binding, Block, CaseSelector, Decl, DeclId, DeclKind, Expr, ExprKind, ExprStatementKind,
    Fn, InbuiltType, Let, LocalId, ShaderStage, Stmt, StmtKind, Struct, TranslationUnit, Type,
    TypeKind, Var, VarDeclKind,
};
use crate::front::wgsl::text::Interner;
use crate::front::wgsl::WgslError;
use crate::front::Typifier;
use crate::proc::{Alignment, Layouter};
use crate::{FastHashMap, Handle, TypeInner};

mod const_eval;
mod format;

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
    FunctionArg(u32),
}

struct RefExpression {
    handle: Handle<crate::Expression>,
    is_ref: bool,
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
                self.errors
                    .push(WgslError::new("overrides are not supported yet").marker(decl.span));
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
                    self.errors
                        .push(WgslError::new("static assertion failed").marker(decl.span));
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
        let is_frag = matches!(f.stage, ShaderStage::Fragment(_));

        self.locals.clear();
        let mut fun = crate::Function {
            name: Some(name.clone()),
            arguments: f
                .args
                .iter()
                .enumerate()
                .filter_map(|(i, arg)| {
                    self.locals.insert(arg.id, LocalData::FunctionArg(i as u32));
                    self.arg(arg, is_frag)
                })
                .collect(),
            result: f.ret.as_ref().and_then(|x| {
                Some(crate::FunctionResult {
                    ty: self.ty(x)?,
                    binding: f
                        .ret_binding
                        .as_ref()
                        .and_then(|b| self.binding(b, x.span, false)),
                })
            }),
            local_variables: Default::default(),
            expressions: Default::default(),
            named_expressions: Default::default(),
            body: Default::default(),
        };

        let body = self.block(&f.block, &mut fun);
        fun.body = body;

        let entry = match f.stage {
            ShaderStage::None => {
                return Some(self.module.functions.append(fun, span));
            }
            ShaderStage::Vertex => crate::EntryPoint {
                name: name.clone(),
                stage: crate::ShaderStage::Vertex,
                early_depth_test: None,
                workgroup_size: [0, 0, 0],
                function: fun,
            },
            ShaderStage::Fragment(early_depth_test) => crate::EntryPoint {
                name: name.clone(),
                stage: crate::ShaderStage::Fragment,
                early_depth_test,
                workgroup_size: [0, 0, 0],
                function: fun,
            },
            ShaderStage::Compute(ref x, ref y, ref z) => crate::EntryPoint {
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

        let ty = self
            .ty(&v.inner.ty)
            .or_else(|| init.map(|(init, span)| self.val_to_ty(init, span)));
        let ty = if let Some(ty) = ty {
            ty
        } else {
            self.errors.push(
                WgslError::new("global variable must have a type or an initializer").marker(span),
            );
            return None;
        };

        let binding = if let Some(ref binding) = v.attribs.binding {
            if let Some(ref group) = v.attribs.group {
                let binding = self.eval.as_positive_int(binding).unwrap_or(0);
                let group = self.eval.as_positive_int(group).unwrap_or(0);
                Some(crate::ResourceBinding { binding, group })
            } else {
                self.errors.push(
                    WgslError::new("resource variable must have both binding and group")
                        .marker(span),
                );
                None
            }
        } else {
            None
        };

        let var = crate::GlobalVariable {
            name: Some(name),
            space: v.inner.address_space,
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
            let binding = field
                .attribs
                .binding
                .as_ref()
                .and_then(|x| self.binding(x, field.name.span.into(), true));

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
                self.errors.push(
                    WgslError::new("size attribute is too small")
                        .label(field.name.span, format!("set size is `{}`", size))
                        .label(field.ty.span, format!("type size is `{}`", min_size)),
                );
            }

            let align = if let Some(align) = align {
                if let Some(align) = Alignment::new(align) {
                    if align >= min_align {
                        align
                    } else {
                        self.errors.push(
                            WgslError::new("alignment attribute is too small")
                                .label(field.name.span, format!("set alignment is `{}`", align))
                                .label(field.ty.span, format!("type alignment is `{}`", min_align)),
                        );
                        min_align
                    }
                } else {
                    self.errors.push(
                        WgslError::new("alignment must be a power of two")
                            .label(field.name.span, format!("set to `{}`", align)),
                    );
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
            binding: arg
                .binding
                .as_ref()
                .and_then(|x| self.binding(x, arg.span.into(), is_frag)),
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
                let body = self.block(&l.body, fun);
                let continuing = l
                    .continuing
                    .as_ref()
                    .map(|x| self.block(x, fun))
                    .unwrap_or(crate::Block::new());
                let break_if = l.break_if.as_ref().and_then(|x| self.expr(x, b, fun));

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
            ExprStatementKind::Call(_) => {}
            ExprStatementKind::Assign(_) => {}
        }
    }

    fn expr(
        &mut self,
        e: &Expr,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let start = fun.expressions.len();
        let handle = self.expr_load(e, fun)?;
        let range = fun.expressions.range_from(start);
        b.push(crate::Statement::Emit(range), e.span.into());
        Some(handle)
    }

    fn expr_load(
        &mut self,
        e: &Expr,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let handle = self.expr_base(e, fun)?;
        Some(if handle.is_ref {
            fun.expressions.append(
                crate::Expression::Load {
                    pointer: handle.handle,
                },
                e.span.into(),
            )
        } else {
            handle.handle
        })
    }

    fn expr_base(&mut self, e: &Expr, fun: &mut crate::Function) -> Option<RefExpression> {
        let (expr, is_ptr) = match e.kind {
            ExprKind::Error => return None,
            ExprKind::Literal(_) => return None,
            ExprKind::Local(local) => match self.locals[&local] {
                LocalData::Variable(var) => (crate::Expression::LocalVariable(var), true),
                LocalData::Let(l) => {
                    return Some(RefExpression {
                        handle: l,
                        is_ref: false,
                    })
                }
                LocalData::FunctionArg(arg) => (crate::Expression::FunctionArgument(arg), true),
            },
            ExprKind::Global(global) => match self.decl_map[&global] {
                DeclData::Function(_) | DeclData::EntryPoint => {
                    self.errors.push(WgslError {
                        message: "function cannot be used as an expression".to_string(),
                        labels: vec![(e.span.into(), "".to_string())],
                        notes: vec!["all function calls must be resolved statically".to_string()],
                    });
                    return None;
                }
                DeclData::Global(var) => (crate::Expression::GlobalVariable(var), true),
                DeclData::Const(constant) => (crate::Expression::Constant(constant), false),
                DeclData::Type(_) => {
                    self.errors.push(WgslError {
                        message: "expected value, found type".to_string(),
                        labels: vec![(e.span.into(), "".to_string())],
                        notes: vec![],
                    });
                    return None;
                }
                DeclData::Assert | DeclData::Override | DeclData::Error => return None,
            },
            ExprKind::Unary(ref un) => {
                let expr = self.expr_load(&un.expr, fun)?;
                (crate::Expression::Unary { op: un.op, expr }, false)
            }
            ExprKind::Binary(ref bin) => {
                let left = self.expr_load(&bin.lhs, fun)?;
                let right = self.expr_load(&bin.rhs, fun)?;
                (
                    crate::Expression::Binary {
                        left,
                        right,
                        op: bin.op,
                    },
                    false,
                )
            }
            ExprKind::Call(_) => return None,
            ExprKind::Index(ref base, ref index) => {
                let base = self.expr_base(base, fun)?;
                let index = self.expr_load(index, fun)?;
                (
                    crate::Expression::Access {
                        base: base.handle,
                        index,
                    },
                    base.is_ref,
                )
            }
            ExprKind::AddrOf(ref e) => {
                let expr = self.expr_base(&e, fun)?;
                if !expr.is_ref {
                    let mut error =
                        WgslError::new("cannot take the address of this expression").marker(e.span);

                    if matches!(e.kind, ExprKind::Local(l) if matches!(self.locals[&l], LocalData::Let(_)))
                    {
                        error
                            .notes
                            .push("`let` bindings resolve to values, not references".to_string());
                    }

                    error.notes.push(
                        "consider assigning to a `var` and then taking the address".to_string(),
                    );

                    self.errors.push(error);
                    return None;
                }

                return Some(RefExpression {
                    handle: expr.handle,
                    is_ref: false,
                });
            }
            ExprKind::Deref(ref e) => {
                let expr = self.expr_base(&e, fun)?;
                if expr.is_ref {
                    self.errors
                        .push(WgslError::new("cannot dereference this expression").marker(e.span));
                    return None;
                }

                return Some(RefExpression {
                    handle: expr.handle,
                    is_ref: true,
                });
            }
            ExprKind::Member(_, _) => return None,
        };

        Some(RefExpression {
            handle: fun.expressions.append(expr, e.span.into()),
            is_ref: is_ptr,
        })
    }

    fn binding(
        &mut self,
        binding: &Binding,
        span: crate::Span,
        is_frag: bool,
    ) -> Option<crate::Binding> {
        match *binding {
            Binding::Builtin(b) => Some(crate::Binding::BuiltIn(b)),
            Binding::Location {
                ref location,
                interpolation,
                sampling,
            } => {
                let interpolation =
                    interpolation.or_else(|| is_frag.then_some(crate::Interpolation::Perspective));
                let sampling = sampling.or_else(|| is_frag.then_some(crate::Sampling::Center));

                if let Some(loc) = location {
                    Some(crate::Binding::Location {
                        location: self.eval.as_positive_int(loc)?,
                        interpolation,
                        sampling,
                    })
                } else {
                    self.errors.push(
                        WgslError::new("location must be specified for all bindings").marker(span),
                    );
                    None
                }
            }
        }
    }

    fn ty(&mut self, ty: &Type) -> Option<Handle<crate::Type>> {
        match ty.kind {
            TypeKind::Inbuilt(ref inbuilt) => {
                let inner = match *inbuilt {
                    InbuiltType::Scalar { kind, width } => TypeInner::Scalar { kind, width },
                    InbuiltType::Vector { size, kind, width } => {
                        TypeInner::Vector { size, kind, width }
                    }
                    InbuiltType::Matrix {
                        columns,
                        rows,
                        width,
                    } => TypeInner::Matrix {
                        columns,
                        rows,
                        width,
                    },
                    InbuiltType::Image {
                        dim,
                        arrayed,
                        class,
                    } => TypeInner::Image {
                        dim,
                        arrayed,
                        class,
                    },
                    InbuiltType::Sampler { comparison } => TypeInner::Sampler { comparison },
                    InbuiltType::Array { ref of, ref len } => {
                        let base = self.ty(&of)?;
                        self.layouter
                            .update(&self.module.types, &self.module.constants)
                            .unwrap();
                        TypeInner::Array {
                            base,
                            size: len
                                .as_ref()
                                .and_then(|x| self.constant(x).map(crate::ArraySize::Constant))
                                .unwrap_or(crate::ArraySize::Dynamic),
                            stride: self.layouter[base].to_stride(),
                        }
                    }
                    InbuiltType::BindingArray { ref of, ref len } => {
                        let base = self.ty(&of)?;
                        TypeInner::BindingArray {
                            base,
                            size: len
                                .as_ref()
                                .and_then(|x| self.constant(x).map(crate::ArraySize::Constant))
                                .unwrap_or(crate::ArraySize::Dynamic),
                        }
                    }
                    InbuiltType::Pointer { ref to, space } => TypeInner::Pointer {
                        base: self.ty(&to)?,
                        space,
                    },
                    InbuiltType::Atomic { kind, width } => TypeInner::Atomic { kind, width },
                    InbuiltType::Infer => return None,
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
                ref x => {
                    self.errors.push(
                        WgslError::new("expected type").label(ty.span, format!("found `{}`", x)),
                    );
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
            inner: TypeInner::Scalar {
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
}
