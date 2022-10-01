use crate::front::wgsl::lower::Lowerer;
use crate::front::wgsl::resolve::inbuilt_functions::InbuiltFunction;
use crate::front::wgsl::resolve::ir::{Expr, Type};
use crate::front::wgsl::WgslError;
use crate::{Handle, ImageClass, TypeInner};

impl Lowerer<'_> {
    pub fn inbuilt_function(
        &mut self,
        f: InbuiltFunction,
        generics: &[Type],
        args: &[Expr],
        span: crate::Span,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<Handle<crate::Expression>> {
        let require_args = |this: &mut Self, n| {
            if args.len() != n {
                this.errors.push(
                    WgslError::new(format!(
                        "expected {} argument{}, found {}",
                        n,
                        if n == 1 { "" } else { "s" },
                        args.len()
                    ))
                    .marker(span),
                );
                return None;
            }
            Some(())
        };

        let require_generics = |this: &mut Self, n| {
            if generics.len() != n {
                this.errors.push(
                    WgslError::new(format!(
                        "expected {} generic argument{}, found {}",
                        n,
                        if n == 1 { "" } else { "s" },
                        generics.len()
                    ))
                    .marker(span),
                );

                if generics.len() < n {
                    return None;
                }
            }
            Some(())
        };

        let math_function = |this: &mut Self,
                             fun: crate::MathFunction,
                             b: &mut crate::Block,
                             f: &mut crate::Function| {
            require_args(this, fun.argument_count())?;
            require_generics(this, 0)?;

            let mut args = args.iter().filter_map(|arg| this.expr(arg, b, f));
            Some(crate::Expression::Math {
                fun,
                arg: args.next()?,
                arg1: args.next(),
                arg2: args.next(),
                arg3: args.next(),
            })
        };

        let texture_query = |this: &mut Self,
                             query: crate::ImageQuery,
                             b: &mut crate::Block,
                             f: &mut crate::Function| {
            require_args(this, 1)?;
            require_generics(this, 0)?;

            Some(crate::Expression::ImageQuery {
                image: this.expr(&args[0], b, f)?,
                query,
            })
        };

        let get_atomic_ptr =
            |this: &mut Self, pointer: Handle<crate::Expression>, f: &crate::Function| {
                let ptr_ty = this.type_handle_of(pointer, f)?;
                let ty = match this.data.module.types[ptr_ty].inner {
                    TypeInner::Pointer { base, .. } => base,
                    _ => {
                        this.errors.push(
                            WgslError::new("expected pointer to atomic")
                                .marker(args[0].span)
                                .label(args[0].span, format!("found `{}`", this.fmt_type(ptr_ty))),
                        );
                        return None;
                    }
                };

                match this.data.module.types[ty].inner {
                    TypeInner::Atomic { kind, width } => Some((kind, width)),
                    _ => {
                        this.errors.push(
                            WgslError::new("expected pointer to atomic")
                                .marker(args[0].span)
                                .label(
                                    args[0].span,
                                    format!("found pointer to `{}`", this.fmt_type(ty)),
                                ),
                        );
                        None
                    }
                }
            };

        let atomic_function = |this: &mut Self,
                               fun: crate::AtomicFunction,
                               b: &mut crate::Block,
                               f: &mut crate::Function| {
            require_args(this, 2)?;
            require_generics(this, 0)?;

            let pointer = this.expr(&args[0], b, f)?;
            let value = this.expr(&args[1], b, f)?;

            let (kind, width) = get_atomic_ptr(this, pointer, f)?;
            let result = crate::Expression::AtomicResult {
                kind,
                width,
                comparison: false,
            };
            let result = f.expressions.append(result, span);

            let stmt = crate::Statement::Atomic {
                pointer,
                fun,
                value,
                result,
            };
            b.push(stmt, span);
            Some(result)
        };

        let expr = match f {
            InbuiltFunction::Bitcast => {
                require_args(self, 1)?;
                require_generics(self, 1)?;

                let to = self.ty(&generics[0])?;
                let kind = match self.data.module.types[to].inner {
                    TypeInner::Scalar { kind, .. } => kind,
                    TypeInner::Vector { kind, .. } => kind,
                    _ => {
                        self.errors.push(
                            WgslError::new("invalid `bitcast` type")
                                .label(generics[0].span, "expected scalar or vector"),
                        );
                        return None;
                    }
                };

                crate::Expression::As {
                    expr: self.expr(&args[0], b, fun)?,
                    kind,
                    convert: None,
                }
            }
            InbuiltFunction::All => {
                require_args(self, 1)?;
                require_generics(self, 0)?;

                crate::Expression::Relational {
                    fun: crate::RelationalFunction::All,
                    argument: self.expr(&args[0], b, fun)?,
                }
            }
            InbuiltFunction::Any => {
                require_args(self, 1)?;
                require_generics(self, 0)?;

                crate::Expression::Relational {
                    fun: crate::RelationalFunction::Any,
                    argument: self.expr(&args[0], b, fun)?,
                }
            }
            InbuiltFunction::Select => {
                require_args(self, 3)?;
                require_generics(self, 0)?;

                crate::Expression::Select {
                    condition: self.expr(&args[2], b, fun)?,
                    accept: self.expr(&args[1], b, fun)?,
                    reject: self.expr(&args[0], b, fun)?,
                }
            }
            InbuiltFunction::ArrayLength => {
                require_args(self, 1)?;
                require_generics(self, 0)?;

                crate::Expression::ArrayLength(self.expr(&args[0], b, fun)?)
            }
            InbuiltFunction::Abs => math_function(self, crate::MathFunction::Abs, b, fun)?,
            InbuiltFunction::Acos => math_function(self, crate::MathFunction::Acos, b, fun)?,
            InbuiltFunction::Acosh => math_function(self, crate::MathFunction::Acosh, b, fun)?,
            InbuiltFunction::Asin => math_function(self, crate::MathFunction::Asin, b, fun)?,
            InbuiltFunction::Asinh => math_function(self, crate::MathFunction::Asinh, b, fun)?,
            InbuiltFunction::Atan => math_function(self, crate::MathFunction::Atan, b, fun)?,
            InbuiltFunction::Atanh => math_function(self, crate::MathFunction::Atanh, b, fun)?,
            InbuiltFunction::Atan2 => math_function(self, crate::MathFunction::Atan2, b, fun)?,
            InbuiltFunction::Ceil => math_function(self, crate::MathFunction::Ceil, b, fun)?,
            InbuiltFunction::Clamp => math_function(self, crate::MathFunction::Clamp, b, fun)?,
            InbuiltFunction::Cos => math_function(self, crate::MathFunction::Cos, b, fun)?,
            InbuiltFunction::Cosh => math_function(self, crate::MathFunction::Cosh, b, fun)?,
            InbuiltFunction::CountOneBits => {
                math_function(self, crate::MathFunction::CountOneBits, b, fun)?
            }
            InbuiltFunction::Cross => math_function(self, crate::MathFunction::Cross, b, fun)?,
            InbuiltFunction::Degrees => math_function(self, crate::MathFunction::Degrees, b, fun)?,
            InbuiltFunction::Determinant => {
                math_function(self, crate::MathFunction::Determinant, b, fun)?
            }
            InbuiltFunction::Distance => {
                math_function(self, crate::MathFunction::Distance, b, fun)?
            }
            InbuiltFunction::Dot => math_function(self, crate::MathFunction::Dot, b, fun)?,
            InbuiltFunction::Exp => math_function(self, crate::MathFunction::Exp, b, fun)?,
            InbuiltFunction::Exp2 => math_function(self, crate::MathFunction::Exp2, b, fun)?,
            InbuiltFunction::ExtractBits => {
                math_function(self, crate::MathFunction::ExtractBits, b, fun)?
            }
            InbuiltFunction::FaceForward => {
                math_function(self, crate::MathFunction::FaceForward, b, fun)?
            }
            InbuiltFunction::FirstLeadingBit => {
                math_function(self, crate::MathFunction::FindMsb, b, fun)?
            }
            InbuiltFunction::FirstTrailingBit => {
                math_function(self, crate::MathFunction::FindLsb, b, fun)?
            }
            InbuiltFunction::Floor => math_function(self, crate::MathFunction::Floor, b, fun)?,
            InbuiltFunction::Fma => math_function(self, crate::MathFunction::Fma, b, fun)?,
            InbuiltFunction::Fract => math_function(self, crate::MathFunction::Fract, b, fun)?,
            InbuiltFunction::Frexp => math_function(self, crate::MathFunction::Frexp, b, fun)?,
            InbuiltFunction::InsertBits => {
                math_function(self, crate::MathFunction::InsertBits, b, fun)?
            }
            InbuiltFunction::InverseSqrt => {
                math_function(self, crate::MathFunction::InverseSqrt, b, fun)?
            }
            InbuiltFunction::Ldexp => math_function(self, crate::MathFunction::Ldexp, b, fun)?,
            InbuiltFunction::Length => math_function(self, crate::MathFunction::Length, b, fun)?,
            InbuiltFunction::Log => math_function(self, crate::MathFunction::Log, b, fun)?,
            InbuiltFunction::Log2 => math_function(self, crate::MathFunction::Log2, b, fun)?,
            InbuiltFunction::Max => math_function(self, crate::MathFunction::Max, b, fun)?,
            InbuiltFunction::Min => math_function(self, crate::MathFunction::Min, b, fun)?,
            InbuiltFunction::Mix => math_function(self, crate::MathFunction::Mix, b, fun)?,
            InbuiltFunction::Modf => math_function(self, crate::MathFunction::Modf, b, fun)?,
            InbuiltFunction::Normalize => {
                math_function(self, crate::MathFunction::Normalize, b, fun)?
            }
            InbuiltFunction::Pow => math_function(self, crate::MathFunction::Pow, b, fun)?,
            InbuiltFunction::Radians => math_function(self, crate::MathFunction::Radians, b, fun)?,
            InbuiltFunction::Reflect => math_function(self, crate::MathFunction::Reflect, b, fun)?,
            InbuiltFunction::Refract => math_function(self, crate::MathFunction::Refract, b, fun)?,
            InbuiltFunction::ReverseBits => {
                math_function(self, crate::MathFunction::ReverseBits, b, fun)?
            }
            InbuiltFunction::Round => math_function(self, crate::MathFunction::Round, b, fun)?,
            InbuiltFunction::Saturate => {
                math_function(self, crate::MathFunction::Saturate, b, fun)?
            }
            InbuiltFunction::Sign => math_function(self, crate::MathFunction::Sign, b, fun)?,
            InbuiltFunction::Sin => math_function(self, crate::MathFunction::Sin, b, fun)?,
            InbuiltFunction::Sinh => math_function(self, crate::MathFunction::Sinh, b, fun)?,
            InbuiltFunction::Smoothstep => {
                math_function(self, crate::MathFunction::SmoothStep, b, fun)?
            }
            InbuiltFunction::Sqrt => math_function(self, crate::MathFunction::Sqrt, b, fun)?,
            InbuiltFunction::Step => math_function(self, crate::MathFunction::Step, b, fun)?,
            InbuiltFunction::Tan => math_function(self, crate::MathFunction::Tan, b, fun)?,
            InbuiltFunction::Tanh => math_function(self, crate::MathFunction::Tanh, b, fun)?,
            InbuiltFunction::Transpose => {
                math_function(self, crate::MathFunction::Transpose, b, fun)?
            }
            InbuiltFunction::Trunc => math_function(self, crate::MathFunction::Trunc, b, fun)?,
            InbuiltFunction::Dpdx | InbuiltFunction::DpdxCoarse | InbuiltFunction::DpdxFine => {
                require_args(self, 1)?;
                require_generics(self, 0)?;

                crate::Expression::Derivative {
                    axis: crate::DerivativeAxis::X,
                    expr: self.expr(&args[0], b, fun)?,
                }
            }
            InbuiltFunction::Dpdy | InbuiltFunction::DpdyCoarse | InbuiltFunction::DpdyFine => {
                require_args(self, 1)?;
                require_generics(self, 0)?;

                crate::Expression::Derivative {
                    axis: crate::DerivativeAxis::Y,
                    expr: self.expr(&args[0], b, fun)?,
                }
            }
            InbuiltFunction::Fwidth
            | InbuiltFunction::FwidthCoarse
            | InbuiltFunction::FwidthFine => {
                require_args(self, 1)?;
                require_generics(self, 0)?;

                crate::Expression::Derivative {
                    axis: crate::DerivativeAxis::Width,
                    expr: self.expr(&args[0], b, fun)?,
                }
            }
            InbuiltFunction::TextureDimensions => {
                require_generics(self, 0)?;
                if args.is_empty() || args.len() > 2 {
                    self.errors.push(
                        WgslError::new(format!("expected 1 or 2 arguments, found {}", args.len()))
                            .marker(span),
                    );
                    return None;
                }

                let image = self.expr(&args[0], b, fun)?;
                let level = args.get(1).and_then(|e| self.expr(e, b, fun));
                crate::Expression::ImageQuery {
                    image,
                    query: crate::ImageQuery::Size { level },
                }
            }
            InbuiltFunction::TextureNumLayers => {
                texture_query(self, crate::ImageQuery::NumLayers, b, fun)?
            }
            InbuiltFunction::TextureNumLevels => {
                texture_query(self, crate::ImageQuery::NumLevels, b, fun)?
            }
            InbuiltFunction::TextureNumSamples => {
                texture_query(self, crate::ImageQuery::NumSamples, b, fun)?
            }
            InbuiltFunction::TextureLoad => {
                if args.len() < 2 {
                    self.errors.push(
                        WgslError::new(format!(
                            "expected at least 2 arguments, found {}",
                            args.len()
                        ))
                        .marker(span),
                    );
                    return None;
                }

                // Argument order: image, coordinate, array_index?, (level | sample)?.

                let image = self.expr(&args[0], b, fun)?;
                let coordinate = self.expr(&args[1], b, fun)?;

                let (arrayed, multi) = self.get_texture_data(image, args[0].span, fun)?;
                let (array_index, sample, level) = if arrayed {
                    let array_index = args.get(2).and_then(|x| self.expr(x, b, fun));
                    let level = args.get(3).and_then(|x| self.expr(x, b, fun));
                    if args.len() > 4 {
                        self.errors
                            .push(WgslError::new("too many arguments").marker(span));
                        return None;
                    }
                    (array_index, None, level)
                } else {
                    let (sample, level) = if multi {
                        let sample = args.get(2).and_then(|x| self.expr(x, b, fun));
                        if args.len() > 3 {
                            self.errors
                                .push(WgslError::new("too many arguments").marker(span));
                            return None;
                        }
                        (sample, None)
                    } else {
                        let level = args.get(2).and_then(|x| self.expr(x, b, fun));
                        if args.len() > 3 {
                            self.errors
                                .push(WgslError::new("too many arguments").marker(span));
                            return None;
                        }
                        (None, level)
                    };
                    (None, sample, level)
                };

                crate::Expression::ImageLoad {
                    image,
                    coordinate,
                    array_index,
                    sample,
                    level,
                }
            }
            InbuiltFunction::TextureSample => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_sample_base(span, &mut args, b, fun)?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: None,
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Auto,
                    depth_ref: None,
                }
            }
            InbuiltFunction::TextureSampleBias => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_sample_base(span, &mut args, b, fun)?;
                let bias =
                    self.require_arg(span, "bias", &mut args, |this, x| this.expr(x, b, fun))?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: None,
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Bias(bias),
                    depth_ref: None,
                }
            }
            InbuiltFunction::TextureSampleCompare => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_sample_base(span, &mut args, b, fun)?;
                let depth_ref =
                    self.require_arg(span, "depth_ref", &mut args, |this, x| this.expr(x, b, fun))?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: None,
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Auto,
                    depth_ref: Some(depth_ref),
                }
            }
            InbuiltFunction::TextureSampleCompareLevel => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_sample_base(span, &mut args, b, fun)?;
                let depth_ref =
                    self.require_arg(span, "depth_ref", &mut args, |this, x| this.expr(x, b, fun))?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: None,
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Zero,
                    depth_ref: Some(depth_ref),
                }
            }
            InbuiltFunction::TextureSampleGrad => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_sample_base(span, &mut args, b, fun)?;
                let x = self.require_arg(span, "ddx", &mut args, |this, x| this.expr(x, b, fun))?;
                let y = self.require_arg(span, "ddy", &mut args, |this, x| this.expr(x, b, fun))?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: None,
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Gradient { x, y },
                    depth_ref: None,
                }
            }
            InbuiltFunction::TextureSampleLevel => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_sample_base(span, &mut args, b, fun)?;
                let level =
                    self.require_arg(span, "level", &mut args, |this, x| this.expr(x, b, fun))?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: None,
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Exact(level),
                    depth_ref: None,
                }
            }
            InbuiltFunction::TextureGather => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_gather_base(span, &mut args, b, fun)?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: Some(sample.component),
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Zero,
                    depth_ref: None,
                }
            }
            InbuiltFunction::TextureGatherCompare => {
                require_generics(self, 0)?;
                let mut args = args.iter();
                let sample = self.texture_gather_base(span, &mut args, b, fun)?;
                let depth_ref =
                    self.require_arg(span, "depth_ref", &mut args, |this, x| this.expr(x, b, fun))?;
                let offset = self.texture_sample_offset(&mut args);

                if args.next().is_some() {
                    self.errors
                        .push(WgslError::new("too many arguments").marker(span));
                }

                crate::Expression::ImageSample {
                    image: sample.image,
                    sampler: sample.sampler,
                    gather: Some(sample.component),
                    coordinate: sample.coordinate,
                    array_index: sample.array_index,
                    offset,
                    level: crate::SampleLevel::Zero,
                    depth_ref: Some(depth_ref),
                }
            }
            InbuiltFunction::TextureStore => {
                if args.len() != 3 && args.len() != 4 {
                    self.errors.push(
                        WgslError::new(format!("expected 3 or 4 arguments, found {}", args.len()))
                            .marker(span),
                    );
                    return None;
                }
                require_generics(self, 0)?;

                let image = self.expr(&args[0], b, fun)?;
                let coordinate = self.expr(&args[1], b, fun)?;
                let array_index = if args.len() == 4 {
                    Some(self.expr(&args[2], b, fun)?)
                } else {
                    None
                };
                let value = self.expr(args.last().unwrap(), b, fun)?;

                let stmt = crate::Statement::ImageStore {
                    image,
                    coordinate,
                    array_index,
                    value,
                };
                b.push(stmt, span);
                return None;
            }
            InbuiltFunction::AtomicLoad => {
                require_args(self, 1)?;
                require_generics(self, 0)?;

                let pointer = self.expr(&args[0], b, fun)?;
                get_atomic_ptr(self, pointer, fun)?;

                crate::Expression::Load { pointer }
            }
            InbuiltFunction::AtomicStore => {
                require_args(self, 2)?;
                require_generics(self, 0)?;

                let pointer = self.expr(&args[0], b, fun)?;
                let value = self.expr(&args[1], b, fun)?;
                get_atomic_ptr(self, pointer, fun)?;

                let stmt = crate::Statement::Store { pointer, value };
                b.push(stmt, span);
                return None;
            }
            InbuiltFunction::AtomicAdd => {
                return atomic_function(self, crate::AtomicFunction::Add, b, fun);
            }
            InbuiltFunction::AtomicSub => {
                return atomic_function(self, crate::AtomicFunction::Subtract, b, fun);
            }
            InbuiltFunction::AtomicMax => {
                return atomic_function(self, crate::AtomicFunction::Max, b, fun);
            }
            InbuiltFunction::AtomicMin => {
                return atomic_function(self, crate::AtomicFunction::Min, b, fun);
            }
            InbuiltFunction::AtomicAnd => {
                return atomic_function(self, crate::AtomicFunction::And, b, fun);
            }
            InbuiltFunction::AtomicOr => {
                return atomic_function(self, crate::AtomicFunction::InclusiveOr, b, fun);
            }
            InbuiltFunction::AtomicXor => {
                return atomic_function(self, crate::AtomicFunction::ExclusiveOr, b, fun);
            }
            InbuiltFunction::AtomicExchange => {
                return atomic_function(
                    self,
                    crate::AtomicFunction::Exchange { compare: None },
                    b,
                    fun,
                );
            }
            InbuiltFunction::AtomicCompareExchangeWeak => {
                require_args(self, 3)?;
                require_generics(self, 0)?;

                let pointer = self.expr(&args[0], b, fun)?;
                let compare = self.expr(&args[1], b, fun);
                let value = self.expr(&args[2], b, fun)?;

                let (kind, width) = get_atomic_ptr(self, pointer, fun)?;
                let result = crate::Expression::AtomicResult {
                    kind,
                    width,
                    comparison: true,
                };
                let result = fun.expressions.append(result, span);

                let stmt = crate::Statement::Atomic {
                    pointer,
                    fun: crate::AtomicFunction::Exchange { compare },
                    value,
                    result,
                };
                b.push(stmt, span);
                return Some(result);
            }
            InbuiltFunction::Pack4x8Snorm => {
                math_function(self, crate::MathFunction::Pack4x8snorm, b, fun)?
            }
            InbuiltFunction::Pack4x8Unorm => {
                math_function(self, crate::MathFunction::Pack4x8unorm, b, fun)?
            }
            InbuiltFunction::Pack2x16Snorm => {
                math_function(self, crate::MathFunction::Pack2x16snorm, b, fun)?
            }
            InbuiltFunction::Pack2x16Unorm => {
                math_function(self, crate::MathFunction::Pack2x16unorm, b, fun)?
            }
            InbuiltFunction::Pack2x16Float => {
                math_function(self, crate::MathFunction::Pack2x16float, b, fun)?
            }
            InbuiltFunction::Unpack4x8Snorm => {
                math_function(self, crate::MathFunction::Unpack4x8snorm, b, fun)?
            }
            InbuiltFunction::Unpack4x8Unorm => {
                math_function(self, crate::MathFunction::Unpack4x8unorm, b, fun)?
            }
            InbuiltFunction::Unpack2x16Snorm => {
                math_function(self, crate::MathFunction::Unpack2x16snorm, b, fun)?
            }
            InbuiltFunction::Unpack2x16Unorm => {
                math_function(self, crate::MathFunction::Unpack2x16unorm, b, fun)?
            }
            InbuiltFunction::Unpack2x16Float => {
                math_function(self, crate::MathFunction::Unpack2x16float, b, fun)?
            }
            InbuiltFunction::StorageBarrier => {
                let stmt = crate::Statement::Barrier(crate::Barrier::STORAGE);
                b.push(stmt, span);
                return None;
            }
            InbuiltFunction::WorkgroupBarrier => {
                let stmt = crate::Statement::Barrier(crate::Barrier::WORK_GROUP);
                b.push(stmt, span);
                return None;
            }
            _ => {
                self.errors
                    .push(WgslError::new("unimplemented inbuilt function").marker(span));
                return None;
            }
        };

        Some(self.emit_expr(expr, span, b, fun))
    }

    fn get_texture_data(
        &mut self,
        image: Handle<crate::Expression>,
        span: crate::Span,
        fun: &crate::Function,
    ) -> Option<(bool, bool)> {
        let ty = self.type_handle_of(image, fun)?;
        match self.data.module.types[ty].inner {
            TypeInner::Image { class, arrayed, .. } => Some(match class {
                ImageClass::Sampled { multi, .. } | ImageClass::Depth { multi } => (arrayed, multi),
                ImageClass::Storage { .. } => (arrayed, false),
            }),
            _ => {
                self.errors.push(
                    WgslError::new("expected a texture")
                        .label(span, format!("found `{}`", self.fmt_type(ty))),
                );
                None
            }
        }
    }

    fn texture_sample_base<'a>(
        &mut self,
        span: crate::Span,
        args: &mut impl ExactSizeIterator<Item = &'a Expr>,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<ImageSampleBase> {
        if args.len() < 3 {
            self.errors.push(
                WgslError::new(format!(
                    "expected at least 3 arguments, found {}",
                    args.len()
                ))
                .marker(span),
            );
            return None;
        }

        // Argument order: image, sampler, coordinate, array_index?.

        let img = args.next().unwrap();
        let image = self.expr(img, b, fun)?;
        let sampler = self.expr(args.next().unwrap(), b, fun)?;
        let coordinate = self.expr(args.next().unwrap(), b, fun)?;

        let (arrayed, _) = self.get_texture_data(image, img.span, fun)?;
        let array_index = if arrayed {
            args.next().and_then(|x| self.expr(x, b, fun))
        } else {
            None
        };

        Some(ImageSampleBase {
            image,
            sampler,
            coordinate,
            array_index,
        })
    }

    fn texture_gather_base<'a>(
        &mut self,
        span: crate::Span,
        args: &mut impl ExactSizeIterator<Item = &'a Expr>,
        b: &mut crate::Block,
        fun: &mut crate::Function,
    ) -> Option<ImageGatherBase> {
        if args.len() < 3 {
            self.errors.push(
                WgslError::new(format!(
                    "expected at least 3 arguments, found {}",
                    args.len()
                ))
                .marker(span),
            );
            return None;
        }

        // Argument order: component?, image, sampler, coordinate, array_index?.

        let component_or_img = args.next().unwrap();
        let img_or_sampler = args.next().unwrap();

        let img_or_sampler_span = img_or_sampler.span;
        let img_or_sampler = self.expr(img_or_sampler, b, fun)?;

        let (component, (image, image_span), sampler) = match *self.type_of(img_or_sampler, fun)? {
            TypeInner::Sampler { .. } => {
                // component not is present.
                let image = self.expr(component_or_img, b, fun)?;
                let sampler = img_or_sampler;
                (0, (image, component_or_img.span), sampler)
            }
            _ => {
                // component not present.
                let component = self.eval.as_positive_int(&self.data, component_or_img)?;
                let image = (img_or_sampler, img_or_sampler_span);
                let sampler = self.expr(args.next().unwrap(), b, fun)?;
                (component, image, sampler)
            }
        };
        let coordinate = self.expr(args.next().unwrap(), b, fun)?;

        let (arrayed, _) = self.get_texture_data(image, image_span, fun)?;
        let array_index = if arrayed {
            args.next().and_then(|x| self.expr(x, b, fun))
        } else {
            None
        };

        let component = match component {
            0 => crate::SwizzleComponent::X,
            1 => crate::SwizzleComponent::Y,
            2 => crate::SwizzleComponent::Z,
            3 => crate::SwizzleComponent::W,
            _ => {
                self.errors.push(
                    WgslError::new(format!("invalid component {}", component))
                        .label(component_or_img.span, "expected 0, 1, 2, or 3"),
                );
                return None;
            }
        };

        Some(ImageGatherBase {
            component,
            image,
            sampler,
            coordinate,
            array_index,
        })
    }

    fn texture_sample_offset<'a>(
        &mut self,
        args: &mut impl ExactSizeIterator<Item = &'a Expr>,
    ) -> Option<Handle<crate::Constant>> {
        args.next().and_then(|x| self.constant(x))
    }

    fn require_arg<'a, T>(
        &mut self,
        span: crate::Span,
        name: &str,
        args: &mut impl ExactSizeIterator<Item = &'a Expr>,
        f: impl FnOnce(&mut Self, &'a Expr) -> Option<T>,
    ) -> Option<T> {
        match args.next() {
            Some(arg) => f(self, arg),
            None => {
                self.errors.push(
                    WgslError::new("missing an argument")
                        .label(span, format!("missing `{}`", name)),
                );
                None
            }
        }
    }
}

struct ImageSampleBase {
    image: Handle<crate::Expression>,
    sampler: Handle<crate::Expression>,
    coordinate: Handle<crate::Expression>,
    array_index: Option<Handle<crate::Expression>>,
}

struct ImageGatherBase {
    component: crate::SwizzleComponent,
    image: Handle<crate::Expression>,
    sampler: Handle<crate::Expression>,
    coordinate: Handle<crate::Expression>,
    array_index: Option<Handle<crate::Expression>>,
}
