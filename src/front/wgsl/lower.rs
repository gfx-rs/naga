use crate::front::wgsl::const_eval::{Evaluator, FloatValue, ScalarValue, Value, VecWidth};
use crate::front::wgsl::WgslError;
use crate::proc::{Alignment, Layouter};
use crate::{FastHashMap, Handle};
use wgsl::resolve::inbuilt::{
    AccessMode, AddressSpace, Builtin, ConservativeDepth, DepthTextureType, InterpolationSample,
    InterpolationType, MatType, PrimitiveType, SampledTextureType, SamplerType, StorageTextureType,
    TexelFormat, VecType,
};
use wgsl::resolve::ir::{
    Arg, ArgAttribs, Decl, DeclId, DeclKind, Expr, FloatType, Fn, FnAttribs, InbuiltType, Let,
    SampleType, Struct, TranslationUnit, Type, TypeKind, Var,
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

        let fun = crate::Function {
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

        let var = crate::GlobalVariable {
            name: Some(name),
            space: self.address_space(v.inner.address_space, v.inner.access_mode),
            binding: None,
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
            offset += size;

            members.push(crate::StructMember {
                name: Some(name),
                binding,
                ty,
                offset,
            });
        }

        let ty = crate::Type {
            name: Some(name),
            inner: crate::TypeInner::Struct {
                members,
                span: alignment.round_up(offset),
            },
        };

        Some(self.module.types.insert(ty, span))
    }

    fn const_(&mut self, c: &Let, span: crate::Span) -> Option<Handle<crate::Constant>> {
        let ident = self.intern.resolve(c.name.name).to_string();
        let value = self.eval.eval(&c.val)?;
        let init = self.val_to_const_inner(value, c.val.span.into());

        let constant = crate::Constant {
            name: Some(ident),
            specialization: None,
            inner: init,
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

    fn primitive_ty(&mut self, ty: &PrimitiveType) -> (crate::ScalarKind, crate::Bytes) {
        match ty {
            PrimitiveType::I32 => (crate::ScalarKind::Sint, 4),
            PrimitiveType::U32 => (crate::ScalarKind::Uint, 4),
            PrimitiveType::F64 => (crate::ScalarKind::Float, 8),
            PrimitiveType::F32 => (crate::ScalarKind::Float, 4),
            PrimitiveType::F16 => (crate::ScalarKind::Float, 2),
            PrimitiveType::Bool => (crate::ScalarKind::Bool, 1),
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
                    self.module
                        .types
                        .insert(crate::Type { name: None, inner }, crate::Span::UNDEFINED),
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
        let value = crate::Constant {
            name: None,
            specialization: None,
            inner: self.val_to_const_inner(value, span),
        };

        self.module.constants.append(value, span)
    }

    fn val_to_const_inner(&mut self, value: Value, span: crate::Span) -> crate::ConstantInner {
        match value {
            Value::Scalar(s) => {
                let (width, value) = self.scalar_value(s);

                crate::ConstantInner::Scalar { width, value }
            }
            Value::Vector(v) => {
                let (width, value) = self.scalar_value(*v.get_any());
                let kind = value.scalar_kind();

                match v {
                    VecWidth::W2(v) => crate::ConstantInner::Composite {
                        ty: self.module.types.insert(
                            crate::Type {
                                name: None,
                                inner: crate::TypeInner::Vector {
                                    size: crate::VectorSize::Bi,
                                    kind,
                                    width,
                                },
                            },
                            crate::Span::UNDEFINED,
                        ),
                        components: v
                            .iter()
                            .map(|s| self.val_to_const(Value::Scalar(*s), span))
                            .collect(),
                    },
                    VecWidth::W3(v) => crate::ConstantInner::Composite {
                        ty: self.module.types.insert(
                            crate::Type {
                                name: None,
                                inner: crate::TypeInner::Vector {
                                    size: crate::VectorSize::Tri,
                                    kind,
                                    width,
                                },
                            },
                            crate::Span::UNDEFINED,
                        ),
                        components: v
                            .iter()
                            .map(|s| self.val_to_const(Value::Scalar(*s), span))
                            .collect(),
                    },
                    VecWidth::W4(v) => crate::ConstantInner::Composite {
                        ty: self.module.types.insert(
                            crate::Type {
                                name: None,
                                inner: crate::TypeInner::Vector {
                                    size: crate::VectorSize::Quad,
                                    kind,
                                    width,
                                },
                            },
                            crate::Span::UNDEFINED,
                        ),
                        components: v
                            .iter()
                            .map(|s| self.val_to_const(Value::Scalar(*s), span))
                            .collect(),
                    },
                }
            }
        }
    }

    fn val_to_ty(&mut self, value: Value, span: crate::Span) -> Handle<crate::Type> {
        let ty = crate::Type {
            name: None,
            inner: match value {
                Value::Scalar(s) => {
                    let (width, value) = self.scalar_value(s);

                    crate::TypeInner::Scalar {
                        kind: value.scalar_kind(),
                        width,
                    }
                }
                Value::Vector(v) => {
                    let (width, value) = self.scalar_value(*v.get_any());
                    let kind = value.scalar_kind();

                    match v {
                        VecWidth::W2(_) => crate::TypeInner::Vector {
                            size: crate::VectorSize::Bi,
                            kind,
                            width,
                        },
                        VecWidth::W3(_) => crate::TypeInner::Vector {
                            size: crate::VectorSize::Tri,
                            kind,
                            width,
                        },
                        VecWidth::W4(_) => crate::TypeInner::Vector {
                            size: crate::VectorSize::Quad,
                            kind,
                            width,
                        },
                    }
                }
            },
        };

        self.module.types.insert(ty, span)
    }

    fn scalar_value(&mut self, value: ScalarValue) -> (crate::Bytes, crate::ScalarValue) {
        match value {
            ScalarValue::Bool(b) => (1, crate::ScalarValue::Bool(b)),
            ScalarValue::AbstractInt(i) => (4, crate::ScalarValue::Sint(i)), // Concretize to `i32`.
            ScalarValue::I32(i) => (4, crate::ScalarValue::Sint(i as _)),
            ScalarValue::U32(u) => (4, crate::ScalarValue::Uint(u as _)),
            ScalarValue::Float(f) => self.float_value(f),
        }
    }

    fn float_value(&mut self, value: FloatValue) -> (crate::Bytes, crate::ScalarValue) {
        match value {
            FloatValue::AbstractFloat(f) => (4, crate::ScalarValue::Float(f)), // Concretize to `f32`.
            FloatValue::F32(f) => (4, crate::ScalarValue::Float(f as _)),
            FloatValue::F64(f) => (8, crate::ScalarValue::Float(f)),
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
