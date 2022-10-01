use crate::front::wgsl::lower::Lowerer;
use crate::front::wgsl::resolve::ir::{Constructible, Expr};
use crate::front::wgsl::WgslError;
use crate::{
    ArraySize, Bytes, ConstantInner, Handle, ScalarKind, ScalarValue, Type, TypeInner, VectorSize,
};

impl Lowerer<'_> {
    pub fn array_size(&mut self, len: &Expr) -> Option<ArraySize> {
        let value = self.eval.as_positive_int(&self.data, len)?;
        let size = self.data.module.constants.fetch_or_append(
            crate::Constant {
                name: None,
                inner: ConstantInner::Scalar {
                    value: ScalarValue::Uint(value as _),
                    width: 4,
                },
                specialization: None,
            },
            len.span,
        );
        Some(ArraySize::Constant(size))
    }

    pub fn construct(
        &mut self,
        ty: &Constructible,
        args: &[Expr],
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let args = self.make_constructor_args(args, b, fun);

        match *ty {
            Constructible::Scalar { kind, width } => {
                let this_ty = self.make_ty(ty, span).unwrap();
                match args {
                    ConstructorArgs::None => self.make_zero_value(this_ty, span, b, fun),
                    ConstructorArgs::One { expr, .. } => {
                        let expr = crate::Expression::As {
                            expr,
                            kind,
                            convert: Some(width),
                        };
                        Some(self.emit_expr(expr, span, b, fun))
                    }
                    ConstructorArgs::Many { spans, .. } => {
                        self.check_arg_count(1, &spans, this_ty);
                        None
                    }
                }
            }
            Constructible::Vector { kind, width, size } => {
                let this_ty = self.make_ty(ty, span).unwrap();
                match args {
                    ConstructorArgs::None => self.make_zero_value(this_ty, span, b, fun),
                    _ => self.construct_vector(args, span, this_ty, size, kind, width, b, fun),
                }
            }
            Constructible::Matrix {
                columns,
                rows,
                width,
            } => {
                let this_ty = self.make_ty(ty, span).unwrap();
                match args {
                    ConstructorArgs::None => self.make_zero_value(this_ty, span, b, fun),
                    _ => self.construct_matrix(args, span, this_ty, rows, columns, width, b, fun),
                }
            }
            Constructible::Array { .. } => {
                let this_ty = self.make_ty(ty, span)?;

                match args {
                    ConstructorArgs::None => self.make_zero_value(this_ty, span, b, fun),
                    ConstructorArgs::One { .. } | ConstructorArgs::Many { .. } => {
                        let components = args.into_vec();
                        let expr = crate::Expression::Compose {
                            ty: this_ty,
                            components,
                        };
                        Some(self.emit_expr(expr, span, b, fun))
                    }
                }
            }
            Constructible::PartialVector { size } => {
                match args {
                    ConstructorArgs::None => {
                        self.make_ty(ty, span); // Errors with an inference error.
                        None
                    }
                    ConstructorArgs::One { ty: base, .. }
                    | ConstructorArgs::Many { ty: base, .. } => {
                        let (kind, width) = match self.data.module.types[base].inner {
                            TypeInner::Scalar { kind, width }
                            | TypeInner::Vector { kind, width, .. } => (kind, width),
                            _ => {
                                self.errors.push(
                                    WgslError::new("expected scalar or vector to construct vector")
                                        .label(span, format!("found `{}`", self.fmt_type(base))),
                                );
                                return None;
                            }
                        };

                        let this_ty = self.register_type(TypeInner::Vector { kind, width, size });
                        self.construct_vector(args, span, this_ty, size, kind, width, b, fun)
                    }
                }
            }
            Constructible::PartialMatrix { columns, rows } => {
                match args {
                    ConstructorArgs::None => {
                        self.make_ty(ty, span); // Errors with an inference error.
                        None
                    }
                    ConstructorArgs::One { ty: base, .. }
                    | ConstructorArgs::Many { ty: base, .. } => {
                        let width = match self.data.module.types[base].inner {
                            TypeInner::Scalar {
                                kind: ScalarKind::Float,
                                width,
                            }
                            | TypeInner::Vector {
                                kind: ScalarKind::Float,
                                width,
                                ..
                            }
                            | TypeInner::Matrix { width, .. } => width,
                            _ => {
                                self.errors.push(
                                    WgslError::new("expected floating-point scalar, vector, or matrix to construct matrix")
                                        .label(span, format!("found `{}`", self.fmt_type(base))),
                                );
                                return None;
                            }
                        };

                        let this_ty = self.register_type(TypeInner::Matrix {
                            rows,
                            columns,
                            width,
                        });
                        self.construct_matrix(args, span, this_ty, rows, columns, width, b, fun)
                    }
                }
            }
            Constructible::PartialArray => {
                match args {
                    ConstructorArgs::None => {
                        self.make_ty(ty, span); // Errors with an inference error.
                        None
                    }
                    ConstructorArgs::One { ty: base, .. }
                    | ConstructorArgs::Many { ty: base, .. } => {
                        let args = args.into_vec();
                        let len = args.len() as u32;

                        let size = crate::Constant {
                            name: None,
                            specialization: None,
                            inner: ConstantInner::Scalar {
                                value: ScalarValue::Uint(len as _),
                                width: 4,
                            },
                        };
                        let size = self.data.module.constants.fetch_or_append(size, span);

                        self.layouter
                            .update(&self.data.module.types, &self.data.module.constants)
                            .unwrap();
                        let ty = self.register_type(TypeInner::Array {
                            base,
                            size: ArraySize::Constant(size),
                            stride: self.layouter[base].to_stride(),
                        });

                        let expr = crate::Expression::Compose {
                            ty,
                            components: args,
                        };
                        Some(self.emit_expr(expr, span, b, fun))
                    }
                }
            }
            Constructible::Type(ty) => match args {
                ConstructorArgs::None => self.make_zero_value(ty, span, b, fun),
                _ => {
                    let components = args.into_vec();
                    let expr = crate::Expression::Compose { ty, components };
                    Some(self.emit_expr(expr, span, b, fun))
                }
            },
        }
    }

    fn make_constructor_args(
        &mut self,
        args: &[Expr],
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> ConstructorArgs {
        let mut out = ConstructorArgs::None;
        for arg in args {
            if let Some(expr) = self.expr(arg, b, fun) {
                if let Some(ty) = self.type_handle_of(expr, fun) {
                    match out {
                        ConstructorArgs::None => {
                            out = ConstructorArgs::One {
                                expr,
                                ty,
                                span: arg.span,
                            };
                        }
                        ConstructorArgs::One {
                            expr: old_expr,
                            ty,
                            span,
                        } => {
                            out = ConstructorArgs::Many {
                                exprs: vec![old_expr, expr],
                                spans: vec![span, arg.span],
                                ty,
                            };
                        }
                        ConstructorArgs::Many {
                            ref mut exprs,
                            ref mut spans,
                            ..
                        } => {
                            exprs.push(expr);
                            spans.push(arg.span);
                        }
                    }
                }
            }
        }

        out
    }

    fn make_ty(&mut self, ty: &Constructible, span: crate::Span) -> Option<Handle<Type>> {
        Some(match *ty {
            Constructible::Scalar { kind, width } => {
                self.register_type(TypeInner::Scalar { kind, width })
            }
            Constructible::Vector { kind, size, width } => {
                self.register_type(TypeInner::Vector { kind, size, width })
            }
            Constructible::Matrix {
                rows,
                columns,
                width,
            } => self.register_type(TypeInner::Matrix {
                rows,
                columns,
                width,
            }),
            Constructible::Array { ref base, ref len } => {
                let base = self.ty(base)?;
                self.layouter
                    .update(&self.data.module.types, &self.data.module.constants)
                    .unwrap();

                let size = self.array_size(len)?;
                self.register_type(TypeInner::Array {
                    base,
                    size,
                    stride: self.layouter[base].to_stride(),
                })
            }
            Constructible::Type(ty) => ty,
            Constructible::PartialVector { .. }
            | Constructible::PartialMatrix { .. }
            | Constructible::PartialArray => {
                self.errors.push(
                    WgslError::new("cannot infer generics")
                        .marker(span)
                        .note("consider annotating the generics with `<...>`"),
                );
                return None;
            }
        })
    }

    fn make_zero_value(
        &mut self,
        ty: Handle<Type>,
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let c = self.make_zero_value_of_type(ty, span)?;
        Some(self.emit_expr(crate::Expression::Constant(c), span, b, fun))
    }

    fn make_zero_value_of_type(
        &mut self,
        ty: Handle<Type>,
        span: crate::Span,
    ) -> Option<Handle<crate::Constant>> {
        let inner = match self.data.module.types[ty].inner {
            TypeInner::Scalar { kind, width } => ConstantInner::Scalar {
                width,
                value: match kind {
                    ScalarKind::Float => ScalarValue::Float(0.0),
                    ScalarKind::Sint => ScalarValue::Sint(0),
                    ScalarKind::Uint => ScalarValue::Uint(0),
                    ScalarKind::Bool => ScalarValue::Bool(false),
                },
            },
            TypeInner::Vector { size, kind, width } => {
                let zero = self.register_type(TypeInner::Scalar { width, kind });
                ConstantInner::Composite {
                    ty,
                    components: std::iter::repeat(
                        self.make_zero_value_of_type(zero, span).unwrap(),
                    )
                    .take(size as usize)
                    .collect(),
                }
            }
            TypeInner::Matrix {
                rows,
                columns,
                width,
            } => {
                let zero = self.register_type(TypeInner::Vector {
                    size: rows,
                    kind: ScalarKind::Float,
                    width,
                });
                ConstantInner::Composite {
                    ty,
                    components: std::iter::repeat(self.make_zero_value_of_type(zero, span)?)
                        .take(columns as usize)
                        .collect(),
                }
            }
            TypeInner::Array { base, size, .. } => {
                let len = match size {
                    ArraySize::Constant(c) => c,
                    ArraySize::Dynamic => return None,
                };
                let len = self.data.module.constants[len].to_array_length()? as usize;
                self.layouter
                    .update(&self.data.module.types, &self.data.module.constants)
                    .unwrap();

                ConstantInner::Composite {
                    ty,
                    components: std::iter::repeat(self.make_zero_value_of_type(base, span)?)
                        .take(len)
                        .collect(),
                }
            }
            TypeInner::Struct { ref members, .. } => {
                let tys: Vec<_> = members.iter().map(|x| x.ty).collect();
                ConstantInner::Composite {
                    ty,
                    components: tys
                        .iter()
                        .map(|&ty| self.make_zero_value_of_type(ty, span))
                        .collect::<Option<_>>()?,
                }
            }
            TypeInner::Atomic { .. }
            | TypeInner::Pointer { .. }
            | TypeInner::ValuePointer { .. }
            | TypeInner::Image { .. }
            | TypeInner::Sampler { .. }
            | TypeInner::BindingArray { .. } => return None,
        };

        Some(self.data.module.constants.fetch_or_append(
            crate::Constant {
                name: None,
                inner,
                specialization: None,
            },
            span,
        ))
    }

    fn check_arg_count(
        &mut self,
        expected: usize,
        spans: &[crate::Span],
        ty: Handle<Type>,
    ) -> Option<()> {
        if spans.len() != expected {
            let span = if spans.len() < expected {
                crate::Span::total_span(spans.iter().copied())
            } else {
                crate::Span::total_span(spans[expected..].iter().copied())
            };
            self.errors.push(
                WgslError::new(format!(
                    "expected {} arguments for `{}` constructor",
                    expected,
                    self.fmt_type(ty)
                ))
                .marker(span)
                .note(if spans.len() < expected {
                    format!("consider adding {} more arguments", expected - spans.len())
                } else {
                    "consider removing the extra arguments".to_string()
                }),
            );
            return None;
        }

        Some(())
    }

    #[allow(clippy::too_many_arguments)]
    fn construct_vector(
        &mut self,
        args: ConstructorArgs,
        span: crate::Span,
        this_ty: Handle<Type>,
        size: VectorSize,
        kind: ScalarKind,
        width: Bytes,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let expr = match args {
            ConstructorArgs::None => unreachable!("should be handled by the caller"),
            ConstructorArgs::One {
                expr,
                ty: arg_ty,
                span: arg_span,
            } => match self.data.module.types[arg_ty].inner {
                TypeInner::Vector {
                    size: from_size, ..
                } => {
                    if size != from_size {
                        self.errors.push(
                            WgslError::new("cannot cast between vectors of different sizes")
                                .label(span, format!("expected `{}`", self.fmt_type(this_ty)))
                                .label(arg_span, format!("found `{}`", self.fmt_type(arg_ty))),
                        );
                        return None;
                    }

                    crate::Expression::As {
                        expr,
                        kind,
                        convert: Some(width),
                    }
                }
                _ => crate::Expression::Splat { size, value: expr },
            },
            ConstructorArgs::Many { exprs, .. } => {
                // self.check_arg_count(size as usize, &spans, this_ty)?;

                crate::Expression::Compose {
                    ty: this_ty,
                    components: exprs,
                }
            }
        };

        Some(self.emit_expr(expr, span, b, fun))
    }

    #[allow(clippy::too_many_arguments)]
    fn construct_matrix(
        &mut self,
        args: ConstructorArgs,
        span: crate::Span,
        this_ty: Handle<Type>,
        rows: VectorSize,
        columns: VectorSize,
        width: Bytes,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let expr =
            match args {
                ConstructorArgs::None => unreachable!("should be handled by the caller"),
                ConstructorArgs::One {
                    expr,
                    ty: arg_ty,
                    span: arg_span,
                } => match self.data.module.types[arg_ty].inner {
                    TypeInner::Matrix {
                        rows: from_rows,
                        columns: from_columns,
                        width,
                    } => {
                        if rows != from_rows || columns != from_columns {
                            self.errors.push(
                                WgslError::new("cannot cast between matrices of different sizes")
                                    .label(span, format!("expected `{}`", self.fmt_type(this_ty)))
                                    .label(arg_span, format!("found `{}`", self.fmt_type(arg_ty))),
                            );
                            return None;
                        }

                        crate::Expression::As {
                            expr,
                            kind: ScalarKind::Float,
                            convert: Some(width),
                        }
                    }
                    _ => {
                        self.errors.push(
                            WgslError::new("expected matrix to cast")
                                .label(arg_span, format!("found `{}`", self.fmt_type(arg_ty))),
                        );
                        return None;
                    }
                },
                ConstructorArgs::Many {
                    exprs,
                    ty: arg_ty,
                    spans,
                } => match self.data.module.types[arg_ty].inner {
                    TypeInner::Scalar { .. } => {
                        self.check_arg_count(columns as usize * rows as usize, &spans, this_ty)?;

                        let column_ty = self.register_type(TypeInner::Vector {
                            kind: ScalarKind::Float,
                            width,
                            size: rows,
                        });
                        let rows = rows as usize;
                        let columns = exprs.chunks(rows).zip(spans.chunks(rows)).map(
                            |(components, spans)| {
                                let span = crate::Span::total_span(spans.iter().copied());
                                let expr = crate::Expression::Compose {
                                    ty: column_ty,
                                    components: components.to_vec(),
                                };
                                self.emit_expr(expr, span, b, fun)
                            },
                        );
                        crate::Expression::Compose {
                            ty: this_ty,
                            components: columns.collect(),
                        }
                    }
                    TypeInner::Vector { .. } => {
                        self.check_arg_count(columns as usize, &spans, this_ty)?;
                        crate::Expression::Compose {
                            ty: this_ty,
                            components: exprs,
                        }
                    }
                    _ => {
                        self.errors.push(
                            WgslError::new("expected scalar or vector to construct matrix")
                                .label(spans[0], format!("found `{}`", self.fmt_type(arg_ty))),
                        );
                        return None;
                    }
                },
            };

        Some(self.emit_expr(expr, span, b, fun))
    }
}

enum ConstructorArgs {
    None,
    One {
        expr: Handle<crate::Expression>,
        ty: Handle<Type>,
        span: crate::Span,
    },
    Many {
        exprs: Vec<Handle<crate::Expression>>,
        spans: Vec<crate::Span>,
        ty: Handle<Type>,
    },
}

impl ConstructorArgs {
    fn into_vec(self) -> Vec<Handle<crate::Expression>> {
        match self {
            ConstructorArgs::None => Vec::new(),
            ConstructorArgs::One { expr, .. } => vec![expr],
            ConstructorArgs::Many { exprs, .. } => exprs,
        }
    }
}
