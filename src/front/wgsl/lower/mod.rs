use crate::front::wgsl::lower::const_eval::{Evaluator, ScalarValue, Value};
use crate::front::wgsl::lower::format::{Pluralize, TypeInnerFormatter};
use crate::front::wgsl::parse::ast::Literal;
use crate::front::wgsl::resolve::ir::{
    Arg, AssignTarget, Binding, Block, CallExpr, CallTarget, CaseSelector, Constructible, Decl,
    DeclId, DeclKind, Expr, ExprKind, ExprStatementKind, Fn, InbuiltType, Let, LocalId,
    ShaderStage, Stmt, StmtKind, Struct, TranslationUnit, Type, TypeKind, Var, VarDeclKind,
};
use crate::front::wgsl::text::Interner;
use crate::front::wgsl::WgslError;
use crate::front::Typifier;
use crate::proc::{Alignment, Layouter, ResolveContext, TypeResolution};
use crate::{proc, FastHashMap, Handle, TypeInner};
use std::convert::TryInto;
use std::fmt::Display;

mod const_eval;
mod construction;
mod format;
mod inbuilt_function;

struct EvaluationData<'a> {
    tu: &'a TranslationUnit,
    decl_map: FastHashMap<DeclId, DeclData>,
    module: crate::Module,
}

pub struct Lowerer<'a> {
    eval: Evaluator,
    intern: &'a Interner,
    errors: Vec<WgslError>,
    layouter: Layouter,
    typifier: Typifier,
    locals: FastHashMap<LocalId, LocalData>,
    data: EvaluationData<'a>,
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
    handle: InferenceExpression,
    is_ref: bool,
}

#[derive(Copy, Clone)]
enum InferenceExpression {
    Concrete(Handle<crate::Expression>),
    AbstractInt(i64),
    AbstractFloat(f64),
}

enum CallError {
    NoReturn,
    Error,
}

impl<'a> Lowerer<'a> {
    pub fn new(module: &'a TranslationUnit, intern: &'a Interner) -> Self {
        Self {
            eval: Evaluator::new(),
            intern,
            errors: Vec::new(),
            layouter: Layouter::default(),
            typifier: Typifier::default(),
            locals: FastHashMap::default(),
            data: EvaluationData {
                tu: module,
                decl_map: FastHashMap::default(),
                module: crate::Module::default(),
            },
        }
    }

    pub fn lower(mut self) -> Result<crate::Module, Vec<WgslError>> {
        for (id, decl) in self.data.tu.decls_ordered() {
            let data = self.decl(decl);
            self.data.decl_map.insert(id, data);
        }

        let eval_errors = self.eval.finish();
        if self.errors.is_empty() && eval_errors.is_empty() {
            Ok(self.data.module)
        } else {
            self.errors.extend(eval_errors);
            Err(self.errors)
        }
    }

    fn decl(&mut self, decl: &Decl) -> DeclData {
        match decl.kind {
            DeclKind::Fn(ref f) => {
                let handle = self.fn_(f, decl.span);
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
                let handle = self.var(v, decl.span);
                handle.map(DeclData::Global).unwrap_or(DeclData::Error)
            }
            DeclKind::Const(ref c) => {
                let handle = self.const_(c, decl.span);
                handle.map(DeclData::Const).unwrap_or(DeclData::Error)
            }
            DeclKind::StaticAssert(ref expr) => {
                let value = self.eval.as_bool(&self.data, expr).unwrap_or(true);
                if !value {
                    self.errors
                        .push(WgslError::new("static assertion failed").marker(decl.span));
                }
                DeclData::Assert
            }
            DeclKind::Struct(ref s) => {
                let handle = self.struct_(s, decl.span);
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
        self.typifier.reset();
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
                return Some(self.data.module.functions.append(fun, span));
            }
            ShaderStage::Vertex => crate::EntryPoint {
                name,
                stage: crate::ShaderStage::Vertex,
                early_depth_test: None,
                workgroup_size: [0, 0, 0],
                function: fun,
            },
            ShaderStage::Fragment(early_depth_test) => crate::EntryPoint {
                name,
                stage: crate::ShaderStage::Fragment,
                early_depth_test,
                workgroup_size: [0, 0, 0],
                function: fun,
            },
            ShaderStage::Compute(ref x, ref y, ref z) => crate::EntryPoint {
                name,
                stage: crate::ShaderStage::Compute,
                early_depth_test: None,
                workgroup_size: [
                    x.as_ref()
                        .and_then(|x| self.eval.as_positive_int(&self.data, x))
                        .unwrap_or(1),
                    y.as_ref()
                        .and_then(|y| self.eval.as_positive_int(&self.data, y))
                        .unwrap_or(1),
                    z.as_ref()
                        .and_then(|z| self.eval.as_positive_int(&self.data, z))
                        .unwrap_or(1),
                ],
                function: fun,
            },
        };

        self.data.module.entry_points.push(entry);
        None
    }

    fn var(&mut self, v: &Var, span: crate::Span) -> Option<Handle<crate::GlobalVariable>> {
        let name = self.intern.resolve(v.inner.name.name).to_string();
        let init = v
            .inner
            .val
            .as_ref()
            .and_then(|x| self.eval.eval(&self.data, x).map(move |v| (v, x.span)));

        let ty = self
            .ty(&v.inner.ty)
            .or_else(|| init.as_ref().map(|&(ref init, _)| self.val_to_ty(init)));

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
                let binding = self.eval.as_positive_int(&self.data, binding).unwrap_or(0);
                let group = self.eval.as_positive_int(&self.data, group).unwrap_or(0);
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
            init: init.map(|(v, span)| self.val_to_const(v, span)),
        };

        Some(self.data.module.global_variables.append(var, span))
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
                .and_then(|x| self.binding(x, field.name.span, ty));

            self.layouter
                .update(&self.data.module.types, &self.data.module.constants)
                .unwrap();

            let min_align = self.layouter[ty].alignment;
            let min_size = self.layouter[ty].size;

            let align = field
                .attribs
                .align
                .as_ref()
                .and_then(|x| self.eval.as_positive_int(&self.data, x));
            let size = field
                .attribs
                .size
                .as_ref()
                .and_then(|x| self.eval.as_positive_int(&self.data, x))
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

        Some(self.data.module.types.insert(ty, span))
    }

    fn const_(&mut self, c: &Let, span: crate::Span) -> Option<Handle<crate::Constant>> {
        let ident = self.intern.resolve(c.name.name).to_string();
        let value = self.eval.eval(&self.data, &c.val)?;
        let ty = self.ty(&c.ty);

        if let Some(ty) = ty {
            let inferred = self.val_to_ty(&value);
            if !self.data.module.types[ty].inner.equivalent(
                &self.data.module.types[inferred].inner,
                &self.data.module.types,
            ) {
                self.errors.push(
                    WgslError::new("mismatched types")
                        .label(c.ty.span, format!("expected {}", self.fmt_type(ty)))
                        .label(c.val.span, format!("found {}", self.fmt_type(inferred))),
                );
            }
        }

        let constant = crate::Constant {
            name: Some(ident),
            specialization: None,
            inner: self.val_to_const_inner(value, span),
        };
        Some(self.data.module.constants.append(constant, span))
    }

    fn arg(&mut self, arg: &Arg) -> Option<crate::FunctionArgument> {
        let ty = self.ty(&arg.ty)?;
        Some(crate::FunctionArgument {
            name: Some(self.intern.resolve(arg.name.name).to_string()),
            ty,
            binding: arg
                .binding
                .as_ref()
                .and_then(|x| self.binding(x, arg.span, ty)),
        })
    }

    fn block(&mut self, b: &Block, fun: &mut crate::Function) -> crate::Block {
        let mut block = crate::Block::with_capacity(b.stmts.len());

        for stmt in b.stmts.iter() {
            self.stmt(stmt, &mut block, fun);
        }

        block
    }

    fn stmt(&mut self, s: &Stmt, b: &mut crate::Block, fun: &mut crate::Function) {
        let stmt = match s.kind {
            StmtKind::Expr(ref k) => return self.expr_stmt(k, s.span, b, fun),
            StmtKind::Block(ref b) => crate::Statement::Block(self.block(b, fun)),
            StmtKind::Break => crate::Statement::Break,
            StmtKind::Continue => crate::Statement::Continue,
            StmtKind::Discard => crate::Statement::Kill,
            StmtKind::For(ref f) => {
                let mut block = crate::Block::with_capacity(2);
                if let Some(ref x) = f.init {
                    self.expr_stmt(&x.kind, x.span, &mut block, fun);
                }

                let mut body = crate::Block::with_capacity(2);
                if let Some(ref x) = f.cond {
                    if let Some(condition) = self.expr(x, &mut body, fun) {
                        body.push(
                            crate::Statement::If {
                                condition,
                                accept: crate::Block::new(),
                                reject: {
                                    let mut b = crate::Block::new();
                                    b.push(crate::Statement::Break, x.span);
                                    b
                                },
                            },
                            x.span,
                        );
                    }
                }
                body.push(
                    crate::Statement::Block(self.block(&f.block, fun)),
                    f.block.span,
                );

                let mut continuing = crate::Block::new();
                if let Some(ref x) = f.update {
                    self.expr_stmt(&x.kind, x.span, &mut continuing, fun)
                }

                block.push(
                    crate::Statement::Loop {
                        body,
                        continuing,
                        break_if: None,
                    },
                    s.span,
                );

                crate::Statement::Block(block)
            }
            StmtKind::If(ref i) => {
                let condition = if let Some(condition) = self.expr(&i.cond, b, fun) {
                    condition
                } else {
                    return;
                };
                let accept = self.block(&i.block, fun);
                let reject = i
                    .else_
                    .as_ref()
                    .map(|stmt| {
                        let mut b = crate::Block::with_capacity(1);
                        self.stmt(stmt, &mut b, fun);
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
                    .unwrap_or_default();
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
                if let Some(value) = self.eval.as_bool(&self.data, expr) {
                    if !value {
                        self.errors
                            .push(WgslError::new("static assertion failed").marker(expr.span));
                    }
                }

                return;
            }
            StmtKind::Switch(ref s) => {
                let selector = if let Some(selector) = self.expr(&s.expr, b, fun) {
                    selector
                } else {
                    return;
                };
                let cases = s
                    .cases
                    .iter()
                    .flat_map(|x| {
                        x.selectors
                            .iter()
                            .filter_map(|sel| {
                                let value = match *sel {
                                    CaseSelector::Expr(ref e) => {
                                        let value = self.eval.as_int(&self.data, e)?;
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
                if let Some(condition) = self.expr(&w.cond, &mut body, fun) {
                    body.push(
                        crate::Statement::If {
                            condition,
                            accept: crate::Block::new(),
                            reject: {
                                let mut b = crate::Block::new();
                                b.push(crate::Statement::Break, w.cond.span);
                                b
                            },
                        },
                        w.cond.span,
                    );
                }

                let b = self.block(&w.block, fun);
                body.push(crate::Statement::Block(b), w.block.span);

                crate::Statement::Loop {
                    body,
                    continuing: crate::Block::new(),
                    break_if: None,
                }
            }
        };
        b.push(stmt, s.span);
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
                    let ty = self.ty(&v.ty);
                    let inferred = expr.and_then(|x| self.type_handle_of(x, fun));

                    if let Some(ty) = ty {
                        if let Some(inferred) = inferred {
                            if !self.data.module.types[ty].inner.equivalent(
                                &self.data.module.types[inferred].inner,
                                &self.data.module.types,
                            ) {
                                self.errors.push(
                                    WgslError::new("mismatched types")
                                        .label(v.ty.span, format!("expected {}", self.fmt_type(ty)))
                                        .label(
                                            v.val.as_ref().unwrap().span,
                                            format!("found {}", self.fmt_type(inferred)),
                                        ),
                                );
                            }
                        }
                    }

                    if let Some(ty) = ty.or(inferred) {
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
                            if !self.data.module.types[ty].inner.equivalent(
                                &self.data.module.types[inferred].inner,
                                &self.data.module.types,
                            ) {
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
                let _ = self.call(call, span, b, fun);
            }
            ExprStatementKind::Assign(ref assign) => {
                let rhs = if let Some(rhs) = self.expr_load(&assign.value, b, fun) {
                    rhs
                } else {
                    return;
                };
                let lhs = if let AssignTarget::Expr(ref lhs) = assign.target {
                    lhs
                } else {
                    return;
                };

                if let Some(l) = self.expr_base(lhs, b, fun) {
                    let lc = self.concretize(l.handle, lhs.span, b, fun);
                    if !l.is_ref {
                        let mut error = WgslError::new("cannot assign to value").marker(lhs.span);

                        if let crate::Expression::Swizzle { .. } = fun.expressions[lc] {
                            error.notes.push("cannot assign to a swizzle".to_string());
                            error.notes.push(
                                "consider assigning to each component separately".to_string(),
                            );
                        }
                        if fun.named_expressions.contains_key(&lc) {
                            error
                                .notes
                                .push("cannot assign to a `let` binding".to_string());
                            error.notes.push("consider using `var` instead".to_string());
                        }

                        self.errors.push(error);
                    }

                    let value = if let Some(op) = assign.op {
                        let left =
                            self.emit_expr(crate::Expression::Load { pointer: lc }, span, b, fun);
                        let mut right = if let Some(rhs) =
                            self.unify_exprs(left, rhs, assign.value.span, b, fun)
                        {
                            rhs
                        } else {
                            return;
                        };

                        // Insert splats if required (only on the right side).
                        if op != crate::BinaryOperator::Multiply {
                            let left_size = match self.type_of(left, fun) {
                                Some(&TypeInner::Vector { size, .. }) => Some(size),
                                _ => None,
                            };
                            if let (Some(size), Some(&TypeInner::Scalar { .. })) =
                                (left_size, self.type_of(right, fun))
                            {
                                right = self.emit_expr(
                                    crate::Expression::Splat { size, value: right },
                                    assign.value.span,
                                    b,
                                    fun,
                                );
                            }
                        }

                        self.emit_expr(crate::Expression::Binary { left, op, right }, span, b, fun)
                    } else {
                        self.concretize(rhs, assign.value.span, b, fun)
                    };

                    b.push(crate::Statement::Store { pointer: lc, value }, span);
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
        let expr = self.expr_load(e, b, fun)?;
        Some(self.concretize(expr, e.span, b, fun))
    }

    fn expr_load(
        &mut self,
        e: &Expr,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<InferenceExpression> {
        let expr = self.expr_base(e, b, fun)?;
        Some(if expr.is_ref {
            let expr = self.emit_expr(
                crate::Expression::Load {
                    pointer: match expr.handle {
                        InferenceExpression::Concrete(h) => h,
                        _ => unreachable!("abstract values are not references"),
                    },
                },
                e.span,
                b,
                fun,
            );
            InferenceExpression::Concrete(expr)
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
                    Literal::AbstractInt(i) => {
                        return Some(RefExpression {
                            handle: InferenceExpression::AbstractInt(i),
                            is_ref: false,
                        })
                    }
                    Literal::AbstractFloat(f) => {
                        return Some(RefExpression {
                            handle: InferenceExpression::AbstractFloat(f),
                            is_ref: false,
                        })
                    }
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
                    crate::Expression::Constant(self.data.module.constants.fetch_or_append(
                        crate::Constant {
                            name: None,
                            inner,
                            specialization: None,
                        },
                        crate::Span::UNDEFINED,
                    )),
                    false,
                )
            }
            ExprKind::Local(local) => match *self.locals.get(&local)? {
                LocalData::Variable(var) => (crate::Expression::LocalVariable(var), true),
                LocalData::Let(l) => {
                    return Some(RefExpression {
                        handle: InferenceExpression::Concrete(l),
                        is_ref: false,
                    })
                }
                LocalData::FunctionArg(arg) => (crate::Expression::FunctionArgument(arg), false),
            },
            ExprKind::Global(global) => match *self.data.decl_map.get(&global)? {
                DeclData::Function(_) | DeclData::EntryPoint => {
                    self.errors.push(
                        WgslError::new("function cannot be used as an expression")
                            .marker(e.span)
                            .note("all function calls must be resolved statically"),
                    );
                    return None;
                }
                DeclData::Global(var) => {
                    let space = self.data.module.global_variables[var].space;
                    (
                        crate::Expression::GlobalVariable(var),
                        !matches!(space, crate::AddressSpace::Handle),
                    )
                }
                DeclData::Const(constant) => (crate::Expression::Constant(constant), false),
                DeclData::Type(_) => {
                    self.errors
                        .push(WgslError::new("expected value, found type").marker(e.span));
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
                let right = self.expr_load(&bin.rhs, b, fun)?;
                let mut right = self.unify_exprs(left, right, bin.rhs.span, b, fun)?;

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
                    Ok(expr) => Some(RefExpression {
                        handle: InferenceExpression::Concrete(expr),
                        is_ref: false,
                    }),
                    Err(CallError::NoReturn) => {
                        self.errors.push(
                            WgslError::new("function does not return any value").marker(e.span),
                        );
                        None
                    }
                    Err(CallError::Error) => None,
                }
            }
            ExprKind::Index(ref base, ref index) => {
                let base_ref = self.expr_base(base, b, fun)?;
                let base_c = self.concretize(base_ref.handle, base.span, b, fun);
                let index = self.expr(index, b, fun)?;

                let ty = self.type_handle_of(base_c, fun)?;
                if self.data.module.types[ty].inner.pointer_space().is_some() && !base_ref.is_ref {
                    self.errors.push(
                        WgslError::new("cannot index a pointer")
                            .label(base.span, format!("found type `{}`", self.fmt_type(ty)))
                            .note("consider dereferencing first"),
                    );
                    return None;
                }

                // Use `AccessIndex` if possible (`index` is an `Expression::Constant`).
                // TODO: Remove this when the SPIR-V backend can see through constant expressions.
                let expr = if let crate::Expression::Constant(c) = fun.expressions[index] {
                    match self.data.module.constants[c].inner {
                        crate::ConstantInner::Scalar { value, .. } => match value {
                            crate::ScalarValue::Uint(u) => {
                                if let Ok(index) = u.try_into() {
                                    crate::Expression::AccessIndex {
                                        base: base_c,
                                        index,
                                    }
                                } else {
                                    crate::Expression::Access {
                                        base: base_c,
                                        index,
                                    }
                                }
                            }
                            crate::ScalarValue::Sint(i) => {
                                if let Ok(index) = i.try_into() {
                                    crate::Expression::AccessIndex {
                                        base: base_c,
                                        index,
                                    }
                                } else {
                                    crate::Expression::Access {
                                        base: base_c,
                                        index,
                                    }
                                }
                            }
                            _ => crate::Expression::Access {
                                base: base_c,
                                index,
                            },
                        },
                        _ => crate::Expression::Access {
                            base: base_c,
                            index,
                        },
                    }
                } else {
                    crate::Expression::Access {
                        base: base_c,
                        index,
                    }
                };
                (expr, base_ref.is_ref)
            }
            ExprKind::AddrOf(ref e) => {
                let expr = self.expr_base(e, b, fun)?;
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
                let expr = self.expr(e, b, fun)?;
                let ty = self.type_handle_of(expr, fun)?;
                if self.data.module.types[ty].inner.pointer_space().is_none() {
                    self.errors.push(
                        WgslError::new("cannot dereference this expression")
                            .label(e.span, format!("has type `{}`", self.fmt_type(ty))),
                    );
                    return None;
                }

                return Some(RefExpression {
                    handle: InferenceExpression::Concrete(expr),
                    is_ref: true,
                });
            }
            ExprKind::Member(ref e, m) => {
                let expr = self.expr_base(e, b, fun)?;
                let e_c = self.concretize(expr.handle, e.span, b, fun);

                let ty = self.type_handle_of(e_c, fun)?;
                let (ty, is_ref) = if expr.is_ref {
                    match self.data.module.types[ty].inner {
                        TypeInner::Pointer { base, .. } => (base, true),
                        TypeInner::ValuePointer {
                            size: Some(size),
                            kind,
                            width,
                            ..
                        } => (
                            self.register_type(TypeInner::Vector { size, kind, width }),
                            true,
                        ),
                        _ => unreachable!("got reference without pointer type"),
                    }
                } else {
                    (ty, false)
                };

                let error = |this: &mut Self| {
                    this.errors.push(
                        WgslError::new(format!("unknown field of type `{}`", this.fmt_type(ty)))
                            .marker(m.span),
                    );
                };

                match self.data.module.types[ty].inner {
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
                            let out = self.emit_expr(
                                crate::Expression::AccessIndex { base: e_c, index },
                                e.span,
                                b,
                                fun,
                            );
                            Some(RefExpression {
                                handle: InferenceExpression::Concrete(out),
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

                            let concrete = self.concretize(expr.handle, e.span, b, fun);
                            // Load the vector for the swizzle.
                            let expr = if is_ref {
                                self.emit_expr(
                                    crate::Expression::Load { pointer: concrete },
                                    e.span,
                                    b,
                                    fun,
                                )
                            } else {
                                concrete
                            };

                            let swizzle = self.emit_expr(
                                crate::Expression::Swizzle {
                                    size,
                                    vector: expr,
                                    pattern,
                                },
                                e.span,
                                b,
                                fun,
                            );
                            return Some(RefExpression {
                                handle: InferenceExpression::Concrete(swizzle),
                                is_ref: false,
                            });
                        };
                    }
                    TypeInner::Matrix { .. } => {
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
                            let base = self.concretize(expr.handle, e.span, b, fun);
                            let concrete = self.emit_expr(
                                crate::Expression::AccessIndex { base, index },
                                e.span,
                                b,
                                fun,
                            );
                            Some(RefExpression {
                                handle: InferenceExpression::Concrete(concrete),
                                is_ref: expr.is_ref,
                            })
                        } else {
                            self.errors.push(
                                WgslError::new("vector swizzle must be between 1 and 4 elements")
                                    .marker(m.span),
                            );
                            None
                        };
                    }
                    TypeInner::Struct { ref members, .. } => {
                        for (i, member) in members.iter().enumerate() {
                            if self.intern.resolve(m.name) == member.name.as_ref().unwrap().as_str()
                            {
                                let base = self.concretize(expr.handle, e.span, b, fun);
                                let concrete = self.emit_expr(
                                    crate::Expression::AccessIndex {
                                        base,
                                        index: i as _,
                                    },
                                    e.span,
                                    b,
                                    fun,
                                );
                                return Some(RefExpression {
                                    handle: InferenceExpression::Concrete(concrete),
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

                        if self.data.module.types[ty].inner.pointer_space().is_some() {
                            error.notes.push("consider dereferencing first".to_string());
                        }

                        self.errors.push(error);
                    }
                }

                return None;
            }
        };
        let handle = self.emit_expr(expr, e.span, b, fun);

        Some(RefExpression {
            handle: InferenceExpression::Concrete(handle),
            is_ref,
        })
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
    ) -> Result<Handle<crate::Expression>, CallError> {
        match call.target {
            CallTarget::Construction(ref ty) => self
                .construct(ty, &call.args, call.target_span, b, fun)
                .ok_or(CallError::Error),
            CallTarget::Decl(id) => match self.data.decl_map[&id] {
                DeclData::Function(function) => {
                    let result = if self.data.module.functions[function].result.is_some() {
                        let expr = fun
                            .expressions
                            .append(crate::Expression::CallResult(function), span);
                        Some(expr)
                    } else {
                        None
                    };
                    let target_args: Vec<_> = self.data.module.functions[function]
                        .arguments
                        .iter()
                        .map(|x| x.ty)
                        .collect();

                    let spans = call.args.iter().map(|x| x.span);
                    self.check_arg_count(target_args.len(), spans)
                        .ok_or(CallError::Error)?;

                    let stmt = crate::Statement::Call {
                        function,
                        arguments: call
                            .args
                            .iter()
                            .zip(target_args)
                            .filter_map(|(arg, ty)| {
                                let expr = self.expr_load(arg, b, fun)?;
                                self.unify_with_type(ty, expr, arg.span, b, fun)
                            })
                            .collect(),
                        result,
                    };
                    b.push(stmt, span);

                    result.ok_or(CallError::NoReturn)
                }
                DeclData::Const(_) | DeclData::Global(_) => {
                    self.errors.push(
                        WgslError::new("cannot call a value").label(span, "expected function"),
                    );
                    Err(CallError::Error)
                }
                DeclData::Type(ty) => self
                    .construct(&Constructible::Type(ty), &call.args, span, b, fun)
                    .ok_or(CallError::Error),
                DeclData::EntryPoint => {
                    self.errors
                        .push(WgslError::new("cannot call entry point").marker(span));
                    Err(CallError::Error)
                }
                DeclData::Assert | DeclData::Override | DeclData::Error => Err(CallError::Error),
            },
            CallTarget::InbuiltFunction(inbuilt, ref generics) => {
                self.inbuilt_function(inbuilt, generics, &call.args, span, b, fun)
            }
            CallTarget::Error => Err(CallError::Error),
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
                if let Some(ref loc) = *location {
                    let mut binding = crate::Binding::Location {
                        location: self.eval.as_positive_int(&self.data, loc)?,
                        interpolation,
                        sampling,
                    };
                    binding.apply_default_interpolation(&self.data.module.types[ty].inner);

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
                constants: &self.data.module.constants,
                types: &self.data.module.types,
                global_vars: &self.data.module.global_variables,
                local_vars: &fun.local_variables,
                functions: &self.data.module.functions,
                arguments: &fun.arguments,
            },
        ) {
            Ok(_) => {}
            Err(e) => {
                self.errors
                    .push(WgslError::new(format!("type error: {:?}", e)));
                return None;
            }
        }

        Some(self.typifier.get(expr, &self.data.module.types))
    }

    fn type_handle_of(
        &mut self,
        expr: Handle<crate::Expression>,
        fun: &crate::Function,
    ) -> Option<Handle<crate::Type>> {
        let _ = self.type_of(expr, fun);
        match self.typifier.resolutions.get(expr.index()) {
            Some(&TypeResolution::Handle(h)) => Some(h),
            Some(&TypeResolution::Value(ref inner)) => Some(self.register_type(inner.clone())),
            None => None,
        }
    }

    fn fmt_type(&self, ty: Handle<crate::Type>) -> impl Display + '_ {
        self.data.module.types[ty]
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
                        let base = self.ty(of)?;
                        self.layouter
                            .update(&self.data.module.types, &self.data.module.constants)
                            .unwrap();
                        TypeInner::Array {
                            base,
                            size: len
                                .as_ref()
                                .and_then(|x| self.array_size(x))
                                .unwrap_or(crate::ArraySize::Dynamic),
                            stride: self.layouter[base].to_stride(),
                        }
                    }
                    InbuiltType::BindingArray { ref of, ref len } => {
                        let base = self.ty(of)?;
                        TypeInner::BindingArray {
                            base,
                            size: len
                                .as_ref()
                                .and_then(|x| self.array_size(x))
                                .unwrap_or(crate::ArraySize::Dynamic),
                        }
                    }
                    InbuiltType::Pointer { ref to, space } => TypeInner::Pointer {
                        base: self.ty(to)?,
                        space,
                    },
                    InbuiltType::Atomic { kind, width } => TypeInner::Atomic { kind, width },
                    InbuiltType::Infer => return None,
                };

                Some(self.register_type(inner))
            }
            TypeKind::User(ref id) => match self.data.decl_map[id] {
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
        self.data.module.types.insert(
            crate::Type {
                name: Some(
                    TypeInnerFormatter {
                        ty: &inner,
                        types: &self.data.module.types,
                        constants: &self.data.module.constants,
                    }
                    .to_string(),
                ),
                inner,
            },
            crate::Span::UNDEFINED,
        )
    }

    fn constant(&mut self, expr: &Expr) -> Option<Handle<crate::Constant>> {
        let value = self.eval.eval(&self.data, expr)?;
        Some(self.val_to_const(value, expr.span))
    }

    fn val_to_const(&mut self, value: Value, span: crate::Span) -> Handle<crate::Constant> {
        let inner = self.val_to_const_inner(value, span);
        self.data.module.constants.fetch_or_append(
            crate::Constant {
                name: None,
                specialization: None,
                inner,
            },
            crate::Span::UNDEFINED,
        )
    }

    fn val_to_const_inner(&mut self, value: Value, span: crate::Span) -> crate::ConstantInner {
        let scalar_to_const = |this: &mut Self, scalar: ScalarValue| {
            let (width, value) = this.val_to_scalar(scalar);

            crate::ConstantInner::Scalar { width, value }
        };

        let ty = self.val_to_ty(&value);
        match value {
            Value::Scalar(scalar) => scalar_to_const(self, scalar),
            Value::Vector(vector) => crate::ConstantInner::Composite {
                ty,
                components: vector
                    .into_iter()
                    .map(|scalar| {
                        let inner = scalar_to_const(self, scalar);
                        self.data.module.constants.fetch_or_append(
                            crate::Constant {
                                name: None,
                                specialization: None,
                                inner,
                            },
                            crate::Span::UNDEFINED,
                        )
                    })
                    .collect(),
            },
            Value::Struct { handle, values, .. } => crate::ConstantInner::Composite {
                ty: handle,
                components: values
                    .into_iter()
                    .map(|value| self.val_to_const(value, span))
                    .collect(),
            },
        }
    }

    fn val_to_ty(&mut self, value: &Value) -> Handle<crate::Type> {
        match *value {
            Value::Scalar(scalar) => {
                let (width, value) = self.val_to_scalar(scalar);

                self.register_type(TypeInner::Scalar {
                    kind: value.scalar_kind(),
                    width,
                })
            }
            Value::Vector(ref vector) => {
                let (width, value) = self.val_to_scalar(vector[0]);

                self.register_type(TypeInner::Vector {
                    size: match vector.len() {
                        2 => crate::VectorSize::Bi,
                        3 => crate::VectorSize::Tri,
                        4 => crate::VectorSize::Quad,
                        _ => unreachable!(),
                    },
                    kind: value.scalar_kind(),
                    width,
                })
            }
            Value::Struct { handle, .. } => handle,
        }
    }

    fn val_to_scalar(&mut self, value: ScalarValue) -> (crate::Bytes, crate::ScalarValue) {
        match value {
            ScalarValue::Bool(b) => (1, crate::ScalarValue::Bool(b)),
            ScalarValue::AbstractInt(i) => (4, crate::ScalarValue::Sint(i)), // Concretize to `i32`.
            ScalarValue::I32(i) => (4, crate::ScalarValue::Sint(i as _)),
            ScalarValue::U32(u) => (4, crate::ScalarValue::Uint(u as _)),
            ScalarValue::AbstractFloat(f) => (4, crate::ScalarValue::Float(f)), // Concretize to `f32`.
            ScalarValue::F32(f) => (4, crate::ScalarValue::Float(f as _)),
        }
    }

    fn concretize(
        &mut self,
        expr: InferenceExpression,
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Handle<crate::Expression> {
        let inner = match expr {
            InferenceExpression::Concrete(handle) => return handle,
            InferenceExpression::AbstractInt(i) => crate::ConstantInner::Scalar {
                width: 4,
                value: crate::ScalarValue::Sint(i),
            },
            InferenceExpression::AbstractFloat(f) => crate::ConstantInner::Scalar {
                width: 4,
                value: crate::ScalarValue::Float(f),
            },
        };

        let c = self.data.module.constants.fetch_or_append(
            crate::Constant {
                name: None,
                specialization: None,
                inner,
            },
            crate::Span::UNDEFINED,
        );
        self.emit_expr(crate::Expression::Constant(c), span, b, fun)
    }

    fn unify_exprs(
        &mut self,
        with: Handle<crate::Expression>,
        to: InferenceExpression,
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let ty = self.type_handle_of(with, fun)?;
        self.unify_with_type(ty, to, span, b, fun)
    }

    fn unify_with_type(
        &mut self,
        ty: Handle<crate::Type>,
        to: InferenceExpression,
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        match self.data.module.types[ty].inner {
            TypeInner::Scalar { kind, width } => match kind {
                crate::ScalarKind::Float => match to {
                    InferenceExpression::Concrete(handle) => Some(handle),
                    InferenceExpression::AbstractInt(i) => {
                        let c = self.data.module.constants.fetch_or_append(
                            crate::Constant {
                                name: None,
                                specialization: None,
                                inner: crate::ConstantInner::Scalar {
                                    width,
                                    value: crate::ScalarValue::Float(i as _),
                                },
                            },
                            crate::Span::UNDEFINED,
                        );
                        Some(self.emit_expr(crate::Expression::Constant(c), span, b, fun))
                    }
                    InferenceExpression::AbstractFloat(f) => {
                        let c = self.data.module.constants.fetch_or_append(
                            crate::Constant {
                                name: None,
                                specialization: None,
                                inner: crate::ConstantInner::Scalar {
                                    width,
                                    value: crate::ScalarValue::Float(f),
                                },
                            },
                            crate::Span::UNDEFINED,
                        );
                        Some(self.emit_expr(crate::Expression::Constant(c), span, b, fun))
                    }
                },
                crate::ScalarKind::Uint => match to {
                    InferenceExpression::Concrete(handle) => Some(handle),
                    InferenceExpression::AbstractInt(i) => {
                        let value = match i.try_into() {
                            Ok(value) => value,
                            Err(_) => {
                                self.errors.push(
                                    WgslError::new("expected a positive integer").marker(span),
                                );
                                return None;
                            }
                        };
                        let c = self.data.module.constants.fetch_or_append(
                            crate::Constant {
                                name: None,
                                specialization: None,
                                inner: crate::ConstantInner::Scalar {
                                    width,
                                    value: crate::ScalarValue::Uint(value),
                                },
                            },
                            crate::Span::UNDEFINED,
                        );
                        Some(self.emit_expr(crate::Expression::Constant(c), span, b, fun))
                    }
                    InferenceExpression::AbstractFloat(_) => {
                        Some(self.concretize(to, span, b, fun))
                    }
                },
                crate::ScalarKind::Sint | crate::ScalarKind::Bool => {
                    Some(self.concretize(to, span, b, fun))
                }
            },
            _ => Some(self.concretize(to, span, b, fun)),
        }
    }

    fn check_arg_count(
        &mut self,
        expected: usize,
        spans: impl ExactSizeIterator<Item = crate::Span>,
    ) -> Option<()> {
        if spans.len() != expected {
            let extra = spans.len() as isize - expected as isize;
            let span = if spans.len() < expected {
                crate::Span::total_span(spans)
            } else {
                crate::Span::total_span(spans.skip(expected))
            };
            let expected = expected as isize;

            self.errors.push(
                WgslError::new(format!(
                    "expected {} {}",
                    expected,
                    "argument".pluralize(expected),
                ))
                .label(
                    span,
                    if extra < 0 {
                        format!("missing {}", "argument".pluralize(-extra))
                    } else {
                        format!("extra {}", "argument".pluralize(extra))
                    },
                )
                .note(if extra < 0 {
                    format!(
                        "consider adding {} more {}",
                        -extra,
                        "argument".pluralize(-extra)
                    )
                } else {
                    format!(
                        "consider removing the {} extra {}",
                        extra,
                        "argument".pluralize(extra)
                    )
                }),
            );
            return None;
        }

        Some(())
    }
}
