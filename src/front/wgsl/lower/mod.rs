use crate::front::wgsl::ast::Literal;
use crate::front::wgsl::lower::const_eval::{Evaluator, Value};
use crate::front::wgsl::lower::format::TypeInnerFormatter;
use crate::front::wgsl::resolve::ir::{
    Arg, AssignTarget, Binding, Block, CallExpr, CaseSelector, Decl, DeclId, DeclKind, Expr,
    ExprKind, ExprStatementKind, Fn, FnTarget, InbuiltType, Let, LocalId, ShaderStage, Stmt,
    StmtKind, Struct, TranslationUnit, Type, TypeKind, Var, VarDeclKind,
};
use crate::front::wgsl::text::Interner;
use crate::front::wgsl::WgslError;
use crate::front::Typifier;
use crate::proc::{Alignment, Layouter, ResolveContext, TypeResolution};
use crate::{proc, FastHashMap, Handle, TypeInner};
use std::fmt::Display;

mod const_eval;
mod format;
mod inbuilt_function;

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

        self.locals.clear();
        let mut fun = crate::Function {
            name: Some(name.clone()),
            arguments: f
                .args
                .iter()
                .enumerate()
                .filter_map(|(i, arg)| {
                    self.locals.insert(arg.id, LocalData::FunctionArg(i as u32));
                    self.arg(arg)
                })
                .collect(),
            result: f.ret.as_ref().and_then(|x| {
                let ty = self.ty(x)?;
                Some(crate::FunctionResult {
                    ty,
                    binding: f
                        .ret_binding
                        .as_ref()
                        .and_then(|b| self.binding(b, x.span, ty)),
                })
            }),
            local_variables: Default::default(),
            expressions: Default::default(),
            named_expressions: Default::default(),
            body: Default::default(),
        };

        let mut body = self.block(&f.block, &mut fun);
        proc::ensure_block_returns(&mut body);
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
                .and_then(|x| self.binding(x, field.name.span.into(), ty));

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

    fn arg(&mut self, arg: &Arg) -> Option<crate::FunctionArgument> {
        let ty = self.ty(&arg.ty)?;
        Some(crate::FunctionArgument {
            name: Some(self.intern.resolve(arg.name.name).to_string()),
            ty,
            binding: arg
                .binding
                .as_ref()
                .and_then(|x| self.binding(x, arg.span.into(), ty)),
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
                        x.selectors
                            .iter()
                            .filter_map(|sel| {
                                let value = match sel {
                                    CaseSelector::Expr(e) => {
                                        let value = self.eval.as_int(e)?;
                                        crate::SwitchValue::Integer(value)
                                    }
                                    CaseSelector::Default => crate::SwitchValue::Default,
                                };
                                Some(crate::SwitchCase {
                                    value,
                                    body: self.block(&x.block, fun),
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
                VarDeclKind::Var(ref v) => {
                    let name = self.intern.resolve(v.name.name).to_string();
                    let expr = v.val.as_ref().and_then(|x| self.expr(x, b, fun));
                    let ty = self
                        .ty(&v.ty)
                        .or_else(|| expr.and_then(|x| self.type_handle_of(x, fun)));

                    if let Some(ty) = ty {
                        let var = fun.local_variables.append(
                            crate::LocalVariable {
                                name: Some(name.clone()),
                                ty,
                                init: None,
                            },
                            span,
                        );
                        if let Some(expr) = expr {
                            fun.named_expressions.insert(expr, name);
                            b.push(
                                crate::Statement::Store {
                                    pointer: fun
                                        .expressions
                                        .append(crate::Expression::LocalVariable(var), span),
                                    value: expr,
                                },
                                span,
                            );
                        }

                        self.locals.insert(decl.id, LocalData::Variable(var));
                    } else {
                        self.errors.push(
                            WgslError::new(
                                "variable declaration must have either initializer or type",
                            )
                            .marker(span),
                        );
                    }
                }
                VarDeclKind::Const(_) => {
                    self.errors.push(
                        WgslError::new("const declarations are not supported here yet")
                            .marker(span),
                    );
                }
                VarDeclKind::Let(ref l) => {
                    let name = self.intern.resolve(l.name.name).to_string();
                    let expr = self.expr(&l.val, b, fun);
                    let ty = self.ty(&l.ty);

                    if let Some(ty) = ty {
                        let inferred = expr.and_then(|x| self.type_handle_of(x, fun));
                        if let Some(inferred) = inferred {
                            if inferred != ty {
                                self.errors.push(
                                    WgslError::new("mismatched types")
                                        .label(l.ty.span, format!("expected {}", self.fmt_type(ty)))
                                        .label(
                                            l.val.span,
                                            format!("found {}", self.fmt_type(inferred)),
                                        ),
                                );
                            }
                        }
                    }

                    if let Some(expr) = expr {
                        fun.named_expressions.insert(expr, name);
                        self.locals.insert(decl.id, LocalData::Let(expr));
                    }
                }
            },
            ExprStatementKind::Call(ref call) => {
                self.call(call, span, b, fun);
            }
            ExprStatementKind::Assign(ref assign) => {
                let rhs = self.expr(&assign.value, b, fun);
                match assign.target {
                    AssignTarget::Expr(ref lhs) => {
                        if let Some(l) = self.expr_base(&lhs, b, fun) {
                            if !l.is_ref {
                                let mut error =
                                    WgslError::new("cannot assign to value").marker(lhs.span);

                                match fun.expressions[l.handle] {
                                    crate::Expression::Swizzle { .. } => {
                                        error.notes.push("cannot assign to a swizzle".to_string());
                                        error.notes.push(
                                            "consider assigning to each component separately"
                                                .to_string(),
                                        );
                                    }
                                    _ => {}
                                }

                                self.errors.push(error);
                            }

                            if let Some(rhs) = rhs {
                                b.push(
                                    crate::Statement::Store {
                                        pointer: l.handle,
                                        value: rhs,
                                    },
                                    span,
                                );
                            }
                        }
                    }
                    AssignTarget::Phony => {}
                }
            }
        }
    }

    fn expr(
        &mut self,
        e: &Expr,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let expr = self.expr_base(e, b, fun)?;
        Some(if expr.is_ref {
            self.emit_expr(
                crate::Expression::Load {
                    pointer: expr.handle,
                },
                e.span.into(),
                b,
                fun,
            )
        } else {
            expr.handle
        })
    }

    fn expr_base(
        &mut self,
        e: &Expr,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<RefExpression> {
        let (expr, is_ref) = match e.kind {
            ExprKind::Error => return None,
            ExprKind::Literal(ref l) => {
                let inner = match *l {
                    Literal::Bool(b) => crate::ConstantInner::Scalar {
                        width: 1,
                        value: crate::ScalarValue::Bool(b),
                    },
                    Literal::AbstractInt(i) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Sint(i), // Concretize to i32.
                    },
                    Literal::AbstractFloat(f) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Float(f), // Concretize to f32.
                    },
                    Literal::I32(i) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Sint(i as _),
                    },
                    Literal::U32(i) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Uint(i as _),
                    },
                    Literal::F32(f) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Float(f as _),
                    },
                    Literal::F16(f) => crate::ConstantInner::Scalar {
                        width: 2,
                        value: crate::ScalarValue::Float(f.to_f64()),
                    },
                };

                (
                    crate::Expression::Constant(self.module.constants.append(
                        crate::Constant {
                            name: None,
                            inner,
                            specialization: None,
                        },
                        e.span,
                    )),
                    false,
                )
            }
            ExprKind::Local(local) => match *self.locals.get(&local)? {
                LocalData::Variable(var) => (crate::Expression::LocalVariable(var), true),
                LocalData::Let(l) => {
                    return Some(RefExpression {
                        handle: l,
                        is_ref: false,
                    })
                }
                LocalData::FunctionArg(arg) => (crate::Expression::FunctionArgument(arg), false),
            },
            ExprKind::Global(global) => match *self.decl_map.get(&global)? {
                DeclData::Function(_) | DeclData::EntryPoint => {
                    self.errors.push(WgslError {
                        message: "function cannot be used as an expression".to_string(),
                        labels: vec![(e.span.into(), "".to_string())],
                        notes: vec!["all function calls must be resolved statically".to_string()],
                    });
                    return None;
                }
                DeclData::Global(var) => {
                    let space = self.module.global_variables[var].space;
                    (
                        crate::Expression::GlobalVariable(var),
                        !matches!(space, crate::AddressSpace::Handle),
                    )
                }
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
                let expr = self.expr(&un.expr, b, fun)?;
                (crate::Expression::Unary { op: un.op, expr }, false)
            }
            ExprKind::Binary(ref bin) => {
                let mut left = self.expr(&bin.lhs, b, fun)?;
                let mut right = self.expr(&bin.rhs, b, fun)?;

                // Insert splats if required.
                if bin.op != crate::BinaryOperator::Multiply {
                    let left_size = match *self.type_of(left, fun)? {
                        TypeInner::Vector { size, .. } => Some(size),
                        _ => None,
                    };
                    match (left_size, self.type_of(right, fun)?) {
                        (Some(size), &TypeInner::Scalar { .. }) => {
                            right = self.emit_expr(
                                crate::Expression::Splat { size, value: right },
                                bin.rhs.span,
                                b,
                                fun,
                            );
                        }
                        (None, &TypeInner::Vector { size, .. }) => {
                            left = self.emit_expr(
                                crate::Expression::Splat { size, value: left },
                                bin.lhs.span,
                                b,
                                fun,
                            );
                        }
                        _ => {}
                    }
                }

                (
                    crate::Expression::Binary {
                        left,
                        right,
                        op: bin.op,
                    },
                    false,
                )
            }
            ExprKind::Call(ref call) => {
                return match self.call(call, e.span, b, fun) {
                    Some(expr) => Some(RefExpression {
                        handle: expr,
                        is_ref: false,
                    }),
                    None => {
                        self.errors.push(
                            WgslError::new("function does not return any value").marker(e.span),
                        );
                        None
                    }
                }
            }
            ExprKind::Index(ref base, ref index) => {
                let base = self.expr_base(base, b, fun)?;
                let index = self.expr(index, b, fun)?;
                (
                    crate::Expression::Access {
                        base: base.handle,
                        index,
                    },
                    base.is_ref,
                )
            }
            ExprKind::AddrOf(ref e) => {
                let expr = self.expr_base(&e, b, fun)?;
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
                let expr = self.expr(&e, b, fun)?;
                if let Some(ty) = self.type_of(expr, fun) {
                    if ty.pointer_space().is_none() {
                        self.errors.push(
                            WgslError::new("cannot dereference this expression")
                                .label(e.span, "expected pointer"),
                        );
                        return None;
                    }
                }

                return Some(RefExpression {
                    handle: expr,
                    is_ref: true,
                });
            }
            ExprKind::Member(ref e, m) => {
                let expr = self.expr_base(&e, b, fun)?;
                let ty = self.type_handle_of(expr.handle, fun)?;
                let ty = if expr.is_ref {
                    match self.module.types[ty].inner {
                        TypeInner::Pointer { base, .. } => base,
                        TypeInner::ValuePointer {
                            size: Some(size),
                            kind,
                            width,
                            ..
                        } => self.register_type(TypeInner::Vector { size, kind, width }),
                        _ => unreachable!("got reference without pointer type"),
                    }
                } else {
                    ty
                };

                let error = |this: &mut Self| {
                    this.errors.push(
                        WgslError::new(format!("unknown field of type `{}`", this.fmt_type(ty)))
                            .marker(m.span),
                    );
                };

                match self.module.types[ty].inner {
                    TypeInner::Vector { .. } => {
                        let mem = self.intern.resolve(m.name);
                        return if mem.len() == 1 {
                            let index = match mem.chars().next().unwrap() {
                                'x' => 0,
                                'y' => 1,
                                'z' => 2,
                                'w' => 3,
                                'r' => 0,
                                'g' => 1,
                                'b' => 2,
                                'a' => 3,
                                _ => {
                                    error(self);
                                    return None;
                                }
                            };
                            Some(RefExpression {
                                handle: self.emit_expr(
                                    crate::Expression::AccessIndex {
                                        base: expr.handle,
                                        index,
                                    },
                                    e.span,
                                    b,
                                    fun,
                                ),
                                is_ref: expr.is_ref,
                            })
                        } else if mem.len() > 4 {
                            self.errors.push(
                                WgslError::new("vector swizzle must be between 1 and 4 elements")
                                    .marker(m.span),
                            );
                            None
                        } else {
                            let size = match mem.len() {
                                2 => crate::VectorSize::Bi,
                                3 => crate::VectorSize::Tri,
                                4 => crate::VectorSize::Quad,
                                _ => unreachable!(),
                            };
                            let mut pattern = [crate::SwizzleComponent::X; 4];

                            let mut is_pattern_xyzw = None;

                            for (i, char) in mem.chars().enumerate() {
                                let (comp, used_xyzw) = match char {
                                    'x' => (crate::SwizzleComponent::X, true),
                                    'y' => (crate::SwizzleComponent::Y, true),
                                    'z' => (crate::SwizzleComponent::Z, true),
                                    'w' => (crate::SwizzleComponent::W, true),
                                    'r' => (crate::SwizzleComponent::X, false),
                                    'g' => (crate::SwizzleComponent::Y, false),
                                    'b' => (crate::SwizzleComponent::Z, false),
                                    'a' => (crate::SwizzleComponent::W, false),
                                    _ => {
                                        error(self);
                                        return None;
                                    }
                                };
                                pattern[i] = comp;

                                match is_pattern_xyzw {
                                    Some(true) if !used_xyzw => {
                                        self.errors.push(
                                            WgslError::new(
                                                "cannot mix xyzw and rgba swizzle components",
                                            )
                                            .marker(m.span),
                                        );
                                        return None;
                                    }
                                    Some(false) if used_xyzw => {
                                        self.errors.push(
                                            WgslError::new(
                                                "cannot mix xyzw and rgba swizzle components",
                                            )
                                            .marker(m.span),
                                        );
                                        return None;
                                    }
                                    None => {
                                        is_pattern_xyzw = Some(used_xyzw);
                                    }
                                    _ => {}
                                }
                            }

                            return Some(RefExpression {
                                handle: self.emit_expr(
                                    crate::Expression::Swizzle {
                                        size,
                                        vector: expr.handle,
                                        pattern,
                                    },
                                    e.span,
                                    b,
                                    fun,
                                ),
                                is_ref: false,
                            });
                        };
                    }
                    TypeInner::Struct { ref members, .. } => {
                        for (i, member) in members.iter().enumerate() {
                            if self.intern.resolve(m.name) == member.name.as_ref().unwrap().as_str()
                            {
                                return Some(RefExpression {
                                    handle: self.emit_expr(
                                        crate::Expression::AccessIndex {
                                            base: expr.handle,
                                            index: i as _,
                                        },
                                        e.span,
                                        b,
                                        fun,
                                    ),
                                    is_ref: expr.is_ref,
                                });
                            }
                        }

                        error(self);
                    }
                    _ => {
                        let mut error = WgslError::new(format!(
                            "unknown field of type `{}`",
                            self.fmt_type(ty)
                        ))
                        .marker(m.span);

                        if self.module.types[ty].inner.pointer_space().is_some() {
                            error
                                .notes
                                .push("consider dereferencing the pointer first".to_string());
                        }

                        self.errors.push(error);
                    }
                }

                return None;
            }
        };
        let handle = self.emit_expr(expr, e.span, b, fun);

        Some(RefExpression { handle, is_ref })
    }

    fn emit_expr(
        &mut self,
        expr: crate::Expression,
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Handle<crate::Expression> {
        let needs_emit = !expr.needs_pre_emit();
        let start = fun.expressions.len();
        let handle = fun.expressions.append(expr, span);

        if needs_emit {
            b.push(
                crate::Statement::Emit(fun.expressions.range_from(start)),
                span,
            );
        }

        handle
    }

    fn call(
        &mut self,
        call: &CallExpr,
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        match call.target {
            FnTarget::InbuiltType(ref ty) => match **ty {
                InbuiltType::Scalar { kind, width } => {
                    if call.args.len() != 1 {
                        self.errors.push(
                            WgslError::new(format!(
                                "expected 1 argument, found {}",
                                call.args.len()
                            ))
                            .marker(span),
                        );
                        return None;
                    }

                    let expr = crate::Expression::As {
                        expr: self.expr(&call.args[0], b, fun)?,
                        kind,
                        convert: Some(width),
                    };
                    Some(self.emit_expr(expr, span, b, fun))
                }
                InbuiltType::Vector { size, kind, width } => {
                    let expr = if call.args.len() == 1 {
                        crate::Expression::Splat {
                            size,
                            value: self.expr(&call.args[0], b, fun)?,
                        }
                    } else {
                        crate::Expression::Compose {
                            ty: self.register_type(TypeInner::Vector { size, kind, width }),
                            components: call
                                .args
                                .iter()
                                .filter_map(|arg| self.expr(arg, b, fun))
                                .collect(),
                        }
                    };

                    Some(self.emit_expr(expr, span, b, fun))
                }
                InbuiltType::Matrix {
                    columns,
                    rows,
                    width,
                } => {
                    let expr = crate::Expression::Compose {
                        ty: self.register_type(TypeInner::Matrix {
                            columns,
                            rows,
                            width,
                        }),
                        components: call
                            .args
                            .iter()
                            .filter_map(|arg| self.expr(arg, b, fun))
                            .collect(),
                    };
                    Some(self.emit_expr(expr, span, b, fun))
                }
                InbuiltType::Array { ref of, ref len } => {
                    let (len, lspan) = len
                        .as_ref()
                        .and_then(|x| self.eval.as_positive_int(x).map(|l| (l, x.span)))
                        .unwrap_or((call.args.len() as _, span));
                    let size = self.module.constants.append(
                        crate::Constant {
                            name: None,
                            inner: crate::ConstantInner::Scalar {
                                value: crate::ScalarValue::Uint(len as _),
                                width: 4,
                            },
                            specialization: None,
                        },
                        lspan,
                    );

                    let base = self.ty(&of);

                    let first = call.args.first().and_then(|x| self.expr(x, b, fun));
                    let (base, first) = if let Some(base) = base {
                        (base, first)
                    } else {
                        if let Some(first) = first {
                            let base = self.type_handle_of(first, fun)?;
                            (base, Some(first))
                        } else {
                            self.errors.push(
                                WgslError::new("cannot infer the type of an empty array")
                                    .marker(span),
                            );
                            return None;
                        }
                    };

                    let _ = self
                        .layouter
                        .update(&self.module.types, &self.module.constants);
                    let ty = self.register_type(TypeInner::Array {
                        base,
                        size: crate::ArraySize::Constant(size),
                        stride: self.layouter[base].to_stride(),
                    });

                    let expr = crate::Expression::Compose {
                        ty,
                        components: first
                            .into_iter()
                            .chain(
                                call.args
                                    .iter()
                                    .skip(1)
                                    .filter_map(|arg| self.expr(arg, b, fun)),
                            )
                            .collect(),
                    };
                    Some(self.emit_expr(expr, span, b, fun))
                }
                _ => unreachable!("got unconstructible inbuilt type"),
            },
            FnTarget::Decl(id) => match self.decl_map[&id] {
                DeclData::Function(function) => {
                    let result = if self.module.functions[function].result.is_some() {
                        Some(self.emit_expr(crate::Expression::CallResult(function), span, b, fun))
                    } else {
                        None
                    };
                    let stmt = crate::Statement::Call {
                        function,
                        arguments: call
                            .args
                            .iter()
                            .filter_map(|arg| self.expr(arg, b, fun))
                            .collect(),
                        result,
                    };
                    b.push(stmt, span);
                    result
                }
                DeclData::Const(_) | DeclData::Global(_) => {
                    self.errors.push(
                        WgslError::new("cannot call a value").label(span, "expected function"),
                    );
                    None
                }
                DeclData::Type(ty) => {
                    let expr = crate::Expression::Compose {
                        ty,
                        components: call
                            .args
                            .iter()
                            .filter_map(|arg| self.expr(arg, b, fun))
                            .collect(),
                    };
                    Some(self.emit_expr(expr, span, b, fun))
                }
                DeclData::EntryPoint => {
                    self.errors
                        .push(WgslError::new("cannot call entry point").marker(span));
                    None
                }
                DeclData::Assert | DeclData::Override | DeclData::Error => None,
            },
            FnTarget::InbuiltFunction(inbuilt, ref generics) => {
                self.inbuilt_function(inbuilt, generics, &call.args, span, b, fun)
            }
            FnTarget::Error => None,
        }
    }

    fn binding(
        &mut self,
        binding: &Binding,
        span: crate::Span,
        ty: Handle<crate::Type>,
    ) -> Option<crate::Binding> {
        match *binding {
            Binding::Builtin(b) => Some(crate::Binding::BuiltIn(b)),
            Binding::Location {
                ref location,
                interpolation,
                sampling,
            } => {
                if let Some(loc) = location {
                    let mut binding = crate::Binding::Location {
                        location: self.eval.as_positive_int(loc)?,
                        interpolation,
                        sampling,
                    };
                    binding.apply_default_interpolation(&self.module.types[ty].inner);

                    Some(binding)
                } else {
                    self.errors.push(
                        WgslError::new("location must be specified for all bindings").marker(span),
                    );
                    None
                }
            }
        }
    }

    fn type_of(
        &mut self,
        expr: Handle<crate::Expression>,
        fun: &crate::Function,
    ) -> Option<&TypeInner> {
        match self.typifier.grow(
            expr,
            &fun.expressions,
            &ResolveContext {
                constants: &self.module.constants,
                types: &self.module.types,
                global_vars: &self.module.global_variables,
                local_vars: &fun.local_variables,
                functions: &self.module.functions,
                arguments: &fun.arguments,
            },
        ) {
            Ok(_) => {}
            Err(e) => {
                self.errors.push(
                    WgslError::new(format!("type error: {:?}", e))
                        .marker(fun.expressions.get_span(expr)),
                );
                return None;
            }
        }

        Some(self.typifier.get(expr, &self.module.types))
    }

    fn type_handle_of(
        &mut self,
        expr: Handle<crate::Expression>,
        fun: &crate::Function,
    ) -> Option<Handle<crate::Type>> {
        let _ = self.type_of(expr, fun);
        match self.typifier.resolutions.get(expr.index()) {
            Some(TypeResolution::Handle(h)) => Some(*h),
            Some(TypeResolution::Value(inner)) => {
                let inner = inner.clone();
                Some(self.register_type(inner))
            }
            None => None,
        }
    }

    fn fmt_type(&self, ty: Handle<crate::Type>) -> impl Display + '_ {
        self.module.types[ty]
            .name
            .as_ref()
            .expect("type without name")
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

                Some(self.register_type(inner))
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

    fn register_type(&mut self, inner: TypeInner) -> Handle<crate::Type> {
        self.module.types.insert(
            crate::Type {
                name: Some(
                    TypeInnerFormatter {
                        ty: &inner,
                        types: &self.module.types,
                        constants: &self.module.constants,
                    }
                    .to_string(),
                ),
                inner,
            },
            crate::Span::UNDEFINED,
        )
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
