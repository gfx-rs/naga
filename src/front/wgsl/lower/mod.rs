use crate::front::wgsl::error::{Error, ExpectedToken, InvalidAssignmentType};
use crate::front::wgsl::index::Index;
use crate::front::wgsl::parse::number::Number;
use crate::front::wgsl::parse::{ast, conv};
use crate::front::{Emitter, Typifier};
use crate::proc::{ensure_block_returns, Alignment, Layouter, ResolveContext, TypeResolution};
use crate::{Arena, FastHashMap, Handle, Span};
use indexmap::IndexMap;

mod construction;

/// State for constructing a `crate::Module`.
pub struct OutputContext<'source, 'temp, 'out> {
    /// The `TranslationUnit`'s expressions arena.
    ast_expressions: &'temp Arena<ast::Expression<'source>>,

    /// The `TranslationUnit`'s types arena.
    types: &'temp Arena<ast::Type<'source>>,

    // Naga IR values.
    /// The map from the names of module-scope declarations to the Naga IR
    /// `Handle`s we have built for them, owned by `Lowerer::lower`.
    globals: &'temp mut FastHashMap<&'source str, LoweredGlobalDecl>,

    /// The module we're constructing.
    module: &'out mut crate::Module,
}

impl<'source> OutputContext<'source, '_, '_> {
    fn reborrow(&mut self) -> OutputContext<'source, '_, '_> {
        OutputContext {
            ast_expressions: self.ast_expressions,
            globals: self.globals,
            types: self.types,
            module: self.module,
        }
    }

    fn ensure_type_exists(&mut self, inner: crate::TypeInner) -> Handle<crate::Type> {
        self.module
            .types
            .insert(crate::Type { inner, name: None }, Span::UNDEFINED)
    }
}

/// State for lowering a statement within a function.
pub struct StatementContext<'source, 'temp, 'out> {
    // WGSL AST values.
    /// A reference to [`TranslationUnit::expressions`] for the translation unit
    /// we're lowering.
    ///
    /// [`TranslationUnit::expressions`]: ast::TranslationUnit::expressions
    ast_expressions: &'temp Arena<ast::Expression<'source>>,

    /// A reference to [`TranslationUnit::types`] for the translation unit
    /// we're lowering.
    ///
    /// [`TranslationUnit::types`]: ast::TranslationUnit::types
    types: &'temp Arena<ast::Type<'source>>,

    // Naga IR values.
    /// The map from the names of module-scope declarations to the Naga IR
    /// `Handle`s we have built for them, owned by `Lowerer::lower`.
    globals: &'temp mut FastHashMap<&'source str, LoweredGlobalDecl>,

    /// A map from `ast::Local` handles to the Naga expressions we've built for them.
    ///
    /// The Naga expressions are either [`LocalVariable`] or
    /// [`FunctionArgument`] expressions.
    ///
    /// [`LocalVariable`]: crate::Expression::LocalVariable
    /// [`FunctionArgument`]: crate::Expression::FunctionArgument
    local_table: &'temp mut FastHashMap<Handle<ast::Local>, TypedExpression>,

    typifier: &'temp mut Typifier,
    variables: &'out mut Arena<crate::LocalVariable>,
    naga_expressions: &'out mut Arena<crate::Expression>,
    /// Stores the names of expressions that are assigned in `let` statement
    /// Also stores the spans of the names, for use in errors.
    named_expressions: &'out mut IndexMap<Handle<crate::Expression>, (String, Span)>,
    arguments: &'out [crate::FunctionArgument],
    module: &'out mut crate::Module,
}

impl<'a, 'temp> StatementContext<'a, 'temp, '_> {
    fn reborrow(&mut self) -> StatementContext<'a, '_, '_> {
        StatementContext {
            local_table: self.local_table,
            globals: self.globals,
            types: self.types,
            ast_expressions: self.ast_expressions,
            typifier: self.typifier,
            variables: self.variables,
            naga_expressions: self.naga_expressions,
            named_expressions: self.named_expressions,
            arguments: self.arguments,
            module: self.module,
        }
    }

    fn as_expression<'t>(
        &'t mut self,
        block: &'t mut crate::Block,
        emitter: &'t mut Emitter,
    ) -> ExpressionContext<'a, 't, '_>
    where
        'temp: 't,
    {
        ExpressionContext {
            local_table: self.local_table,
            globals: self.globals,
            types: self.types,
            ast_expressions: self.ast_expressions,
            typifier: self.typifier,
            naga_expressions: self.naga_expressions,
            module: self.module,
            local_vars: self.variables,
            arguments: self.arguments,
            block,
            emitter,
        }
    }

    fn as_output(&mut self) -> OutputContext<'a, '_, '_> {
        OutputContext {
            ast_expressions: self.ast_expressions,
            globals: self.globals,
            types: self.types,
            module: self.module,
        }
    }

    fn invalid_assignment_type(&self, expr: Handle<crate::Expression>) -> InvalidAssignmentType {
        if let Some(&(_, span)) = self.named_expressions.get(&expr) {
            InvalidAssignmentType::ImmutableBinding(span)
        } else {
            match self.naga_expressions[expr] {
                crate::Expression::Swizzle { .. } => InvalidAssignmentType::Swizzle,
                crate::Expression::Access { base, .. } => self.invalid_assignment_type(base),
                crate::Expression::AccessIndex { base, .. } => self.invalid_assignment_type(base),
                _ => InvalidAssignmentType::Other,
            }
        }
    }
}

/// State for lowering an `ast::Expression` to Naga IR.
///
/// Not to be confused with `parser::ExpressionContext`.
pub struct ExpressionContext<'source, 'temp, 'out> {
    // WGSL AST values.
    local_table: &'temp mut FastHashMap<Handle<ast::Local>, TypedExpression>,
    ast_expressions: &'temp Arena<ast::Expression<'source>>,
    types: &'temp Arena<ast::Type<'source>>,

    // Naga IR values.
    /// The map from the names of module-scope declarations to the Naga IR
    /// `Handle`s we have built for them, owned by `Lowerer::lower`.
    globals: &'temp mut FastHashMap<&'source str, LoweredGlobalDecl>,

    typifier: &'temp mut Typifier,
    naga_expressions: &'out mut Arena<crate::Expression>,
    local_vars: &'out Arena<crate::LocalVariable>,
    arguments: &'out [crate::FunctionArgument],
    module: &'out mut crate::Module,
    block: &'temp mut crate::Block,
    emitter: &'temp mut Emitter,
}

impl<'a> ExpressionContext<'a, '_, '_> {
    fn reborrow(&mut self) -> ExpressionContext<'a, '_, '_> {
        ExpressionContext {
            local_table: self.local_table,
            globals: self.globals,
            types: self.types,
            ast_expressions: self.ast_expressions,
            typifier: self.typifier,
            naga_expressions: self.naga_expressions,
            module: self.module,
            local_vars: self.local_vars,
            arguments: self.arguments,
            block: self.block,
            emitter: self.emitter,
        }
    }

    fn as_output(&mut self) -> OutputContext<'a, '_, '_> {
        OutputContext {
            ast_expressions: self.ast_expressions,
            globals: self.globals,
            types: self.types,
            module: self.module,
        }
    }

    /// Determine the type of `handle`, and add it to the module's arena.
    ///
    /// If you just need a `TypeInner` for `handle`'s type, use
    /// [`grow_types`] and [`resolved_inner`] instead. This function
    /// should only be used when the type of `handle` needs to appear
    /// in the module's final `Arena<Type>`, for example, if you're
    /// creating a [`LocalVariable`] whose type is inferred from its
    /// initializer.
    ///
    /// [`grow_types`]: Self::grow_types
    /// [`resolved_inner`]: Self::resolved_inner
    /// [`LocalVariable`]: crate::LocalVariable
    fn register_type(
        &mut self,
        handle: Handle<crate::Expression>,
    ) -> Result<Handle<crate::Type>, Error<'a>> {
        self.grow_types(handle)?;
        Ok(self.typifier.register_type(handle, &mut self.module.types))
    }

    /// Resolve the types of all expressions up through `handle`.
    ///
    /// Ensure that [`self.typifier`] has a [`TypeResolution`] for
    /// every expression in [`self.naga_expressions`].
    ///
    /// This does not add types to any arena. The [`Typifier`]
    /// documentation explains the steps we take to avoid filling
    /// arenas with intermediate types.
    ///
    /// This function takes `&mut self`, so it can't conveniently
    /// return a shared reference to the resulting `TypeResolution`:
    /// the shared reference would extend the mutable borrow, and you
    /// wouldn't be able to use `self` for anything else. Instead, you
    /// should call `grow_types` to cover the handles you need, and
    /// then use `self.typifier[handle]` or
    /// [`self.resolved_inner(handle)`] to get at their resolutions.
    ///
    /// [`self.typifier`]: ExpressionContext::typifier
    /// [`self.resolved_inner(handle)`]: ExpressionContext::resolved_inner
    /// [`Typifier`]: Typifier
    fn grow_types(&mut self, handle: Handle<crate::Expression>) -> Result<&mut Self, Error<'a>> {
        let resolve_ctx = ResolveContext::with_locals(self.module, self.local_vars, self.arguments);
        self.typifier
            .grow(handle, self.naga_expressions, &resolve_ctx)
            .map_err(Error::InvalidResolve)?;
        Ok(self)
    }

    fn resolved_inner(&self, handle: Handle<crate::Expression>) -> &crate::TypeInner {
        self.typifier[handle].inner_with(&self.module.types)
    }

    fn image_data(
        &mut self,
        image: Handle<crate::Expression>,
        span: Span,
    ) -> Result<(crate::ImageClass, bool), Error<'a>> {
        self.grow_types(image)?;
        match *self.resolved_inner(image) {
            crate::TypeInner::Image { class, arrayed, .. } => Ok((class, arrayed)),
            _ => Err(Error::BadTexture(span)),
        }
    }

    fn prepare_args<'b>(
        &mut self,
        args: &'b [Handle<ast::Expression<'a>>],
        min_args: u32,
        span: Span,
    ) -> ArgumentContext<'b, 'a> {
        ArgumentContext {
            args: args.iter(),
            min_args,
            args_used: 0,
            total_args: args.len() as u32,
            span,
        }
    }

    /// Insert splats, if needed by the non-'*' operations.
    fn binary_op_splat(
        &mut self,
        op: crate::BinaryOperator,
        left: &mut Handle<crate::Expression>,
        right: &mut Handle<crate::Expression>,
    ) -> Result<(), Error<'a>> {
        if op != crate::BinaryOperator::Multiply {
            self.grow_types(*left)?.grow_types(*right)?;

            let left_size = match *self.resolved_inner(*left) {
                crate::TypeInner::Vector { size, .. } => Some(size),
                _ => None,
            };

            match (left_size, self.resolved_inner(*right)) {
                (Some(size), &crate::TypeInner::Scalar { .. }) => {
                    *right = self.naga_expressions.append(
                        crate::Expression::Splat {
                            size,
                            value: *right,
                        },
                        self.naga_expressions.get_span(*right),
                    );
                }
                (None, &crate::TypeInner::Vector { size, .. }) => {
                    *left = self.naga_expressions.append(
                        crate::Expression::Splat { size, value: *left },
                        self.naga_expressions.get_span(*left),
                    );
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Add a single expression to the expression table that is not covered by `self.emitter`.
    ///
    /// This is useful for `CallResult` and `AtomicResult` expressions, which should not be covered by
    /// `Emit` statements.
    fn interrupt_emitter(
        &mut self,
        expression: crate::Expression,
        span: Span,
    ) -> Handle<crate::Expression> {
        self.block
            .extend(self.emitter.finish(self.naga_expressions));
        let result = self.naga_expressions.append(expression, span);
        self.emitter.start(self.naga_expressions);
        result
    }

    /// Apply the WGSL Load Rule to `expr`.
    ///
    /// If `expr` is has type `ref<SC, T, A>`, perform a load to produce a value of type
    /// `T`. Otherwise, return `expr` unchanged.
    fn apply_load_rule(&mut self, expr: TypedExpression) -> Handle<crate::Expression> {
        if expr.is_reference {
            let load = crate::Expression::Load {
                pointer: expr.handle,
            };
            let span = self.naga_expressions.get_span(expr.handle);
            self.naga_expressions.append(load, span)
        } else {
            expr.handle
        }
    }

    /// Creates a zero value constant of type `ty`
    ///
    /// Returns `None` if the given `ty` is not a constructible type
    fn create_zero_value_constant(
        &mut self,
        ty: Handle<crate::Type>,
    ) -> Option<Handle<crate::Constant>> {
        let inner = match self.module.types[ty].inner {
            crate::TypeInner::Scalar { kind, width } => {
                let value = match kind {
                    crate::ScalarKind::Sint => crate::ScalarValue::Sint(0),
                    crate::ScalarKind::Uint => crate::ScalarValue::Uint(0),
                    crate::ScalarKind::Float => crate::ScalarValue::Float(0.),
                    crate::ScalarKind::Bool => crate::ScalarValue::Bool(false),
                };
                crate::ConstantInner::Scalar { width, value }
            }
            crate::TypeInner::Vector { size, kind, width } => {
                let scalar_ty = self.ensure_type_exists(crate::TypeInner::Scalar { width, kind });
                let component = self.create_zero_value_constant(scalar_ty)?;
                crate::ConstantInner::Composite {
                    ty,
                    components: (0..size as u8).map(|_| component).collect(),
                }
            }
            crate::TypeInner::Matrix {
                columns,
                rows,
                width,
            } => {
                let vec_ty = self.ensure_type_exists(crate::TypeInner::Vector {
                    width,
                    kind: crate::ScalarKind::Float,
                    size: rows,
                });
                let component = self.create_zero_value_constant(vec_ty)?;
                crate::ConstantInner::Composite {
                    ty,
                    components: (0..columns as u8).map(|_| component).collect(),
                }
            }
            crate::TypeInner::Array {
                base,
                size: crate::ArraySize::Constant(size),
                ..
            } => {
                let size = self.module.constants[size].to_array_length()?;
                let component = self.create_zero_value_constant(base)?;
                crate::ConstantInner::Composite {
                    ty,
                    components: (0..size).map(|_| component).collect(),
                }
            }
            crate::TypeInner::Struct { ref members, .. } => {
                let members = members.clone();
                crate::ConstantInner::Composite {
                    ty,
                    components: members
                        .iter()
                        .map(|member| self.create_zero_value_constant(member.ty))
                        .collect::<Option<_>>()?,
                }
            }
            _ => return None,
        };

        let constant = self.module.constants.fetch_or_append(
            crate::Constant {
                name: None,
                specialization: None,
                inner,
            },
            Span::UNDEFINED,
        );
        Some(constant)
    }

    fn format_typeinner(&self, inner: &crate::TypeInner) -> String {
        inner.to_wgsl(&self.module.types, &self.module.constants)
    }

    fn format_type(&self, handle: Handle<crate::Type>) -> String {
        let ty = &self.module.types[handle];
        match ty.name {
            Some(ref name) => name.clone(),
            None => self.format_typeinner(&ty.inner),
        }
    }

    fn format_type_resolution(&self, resolution: &TypeResolution) -> String {
        match *resolution {
            TypeResolution::Handle(handle) => self.format_type(handle),
            TypeResolution::Value(ref inner) => self.format_typeinner(inner),
        }
    }

    fn ensure_type_exists(&mut self, inner: crate::TypeInner) -> Handle<crate::Type> {
        self.as_output().ensure_type_exists(inner)
    }
}

struct ArgumentContext<'ctx, 'source> {
    args: std::slice::Iter<'ctx, Handle<ast::Expression<'source>>>,
    min_args: u32,
    args_used: u32,
    total_args: u32,
    span: Span,
}

impl<'source> ArgumentContext<'_, 'source> {
    pub fn finish(self) -> Result<(), Error<'source>> {
        if self.args.len() == 0 {
            Ok(())
        } else {
            Err(Error::WrongArgumentCount {
                found: self.total_args,
                expected: self.min_args..self.args_used + 1,
                span: self.span,
            })
        }
    }

    pub fn next(&mut self) -> Result<Handle<ast::Expression<'source>>, Error<'source>> {
        match self.args.next().copied() {
            Some(arg) => {
                self.args_used += 1;
                Ok(arg)
            }
            None => Err(Error::WrongArgumentCount {
                found: self.total_args,
                expected: self.min_args..self.args_used + 1,
                span: self.span,
            }),
        }
    }
}

/// A Naga [`Expression`] handle, with WGSL type information.
///
/// Naga and WGSL types are very close, but Naga lacks WGSL's 'reference' types,
/// which we need to know to apply the Load Rule. This struct carries a Naga
/// `Handle<Expression>` along with enough information to determine its WGSL type.
///
/// [`Expression`]: crate::Expression
#[derive(Debug, Copy, Clone)]
struct TypedExpression {
    /// The handle of the Naga expression.
    handle: Handle<crate::Expression>,

    /// True if this expression's WGSL type is a reference.
    ///
    /// When this is true, `handle` must be a pointer.
    is_reference: bool,
}

impl TypedExpression {
    const fn non_reference(handle: Handle<crate::Expression>) -> TypedExpression {
        TypedExpression {
            handle,
            is_reference: false,
        }
    }
}

enum Composition {
    Single(u32),
    Multi(crate::VectorSize, [crate::SwizzleComponent; 4]),
}

impl Composition {
    const fn letter_component(letter: char) -> Option<crate::SwizzleComponent> {
        use crate::SwizzleComponent as Sc;
        match letter {
            'x' | 'r' => Some(Sc::X),
            'y' | 'g' => Some(Sc::Y),
            'z' | 'b' => Some(Sc::Z),
            'w' | 'a' => Some(Sc::W),
            _ => None,
        }
    }

    fn extract_impl(name: &str, name_span: Span) -> Result<u32, Error> {
        let ch = name.chars().next().ok_or(Error::BadAccessor(name_span))?;
        match Self::letter_component(ch) {
            Some(sc) => Ok(sc as u32),
            None => Err(Error::BadAccessor(name_span)),
        }
    }

    fn make(name: &str, name_span: Span) -> Result<Self, Error> {
        if name.len() > 1 {
            let mut components = [crate::SwizzleComponent::X; 4];
            for (comp, ch) in components.iter_mut().zip(name.chars()) {
                *comp = Self::letter_component(ch).ok_or(Error::BadAccessor(name_span))?;
            }

            let size = match name.len() {
                2 => crate::VectorSize::Bi,
                3 => crate::VectorSize::Tri,
                4 => crate::VectorSize::Quad,
                _ => return Err(Error::BadAccessor(name_span)),
            };
            Ok(Composition::Multi(size, components))
        } else {
            Self::extract_impl(name, name_span).map(Composition::Single)
        }
    }
}

/// An `ast::GlobalDecl` for which we have built the Naga IR equivalent.
enum LoweredGlobalDecl {
    Function(Handle<crate::Function>),
    Var(Handle<crate::GlobalVariable>),
    Const(Handle<crate::Constant>),
    Type(Handle<crate::Type>),
    EntryPoint,
}

enum ConstantOrInner {
    Constant(Handle<crate::Constant>),
    Inner(crate::ConstantInner),
}

enum Texture {
    Gather,
    GatherCompare,

    Sample,
    SampleBias,
    SampleCompare,
    SampleCompareLevel,
    SampleGrad,
    SampleLevel,
    // SampleBaseClampToEdge,
}

impl Texture {
    pub fn map(word: &str) -> Option<Self> {
        Some(match word {
            "textureGather" => Self::Gather,
            "textureGatherCompare" => Self::GatherCompare,

            "textureSample" => Self::Sample,
            "textureSampleBias" => Self::SampleBias,
            "textureSampleCompare" => Self::SampleCompare,
            "textureSampleCompareLevel" => Self::SampleCompareLevel,
            "textureSampleGrad" => Self::SampleGrad,
            "textureSampleLevel" => Self::SampleLevel,
            // "textureSampleBaseClampToEdge" => Some(Self::SampleBaseClampToEdge),
            _ => return None,
        })
    }

    pub const fn min_argument_count(&self) -> u32 {
        match *self {
            Self::Gather => 3,
            Self::GatherCompare => 4,

            Self::Sample => 3,
            Self::SampleBias => 5,
            Self::SampleCompare => 5,
            Self::SampleCompareLevel => 5,
            Self::SampleGrad => 6,
            Self::SampleLevel => 5,
            // Self::SampleBaseClampToEdge => 3,
        }
    }
}

pub struct Lowerer<'source, 'temp> {
    index: &'temp Index<'source>,
    layouter: Layouter,
}

impl<'source, 'temp> Lowerer<'source, 'temp> {
    pub fn new(index: &'temp Index<'source>) -> Self {
        Self {
            index,
            layouter: Layouter::default(),
        }
    }

    pub fn lower(
        &mut self,
        tu: &'temp ast::TranslationUnit<'source>,
    ) -> Result<crate::Module, Error<'source>> {
        let mut module = crate::Module::default();

        let mut ctx = OutputContext {
            ast_expressions: &tu.expressions,
            globals: &mut FastHashMap::default(),
            types: &tu.types,
            module: &mut module,
        };

        for decl_handle in self.index.visit_ordered() {
            let span = tu.decls.get_span(decl_handle);
            let decl = &tu.decls[decl_handle];

            match decl.kind {
                ast::GlobalDeclKind::Fn(ref f) => {
                    let lowered_decl = self.function(f, span, ctx.reborrow())?;
                    ctx.globals.insert(f.name.name, lowered_decl);
                }
                ast::GlobalDeclKind::Var(ref v) => {
                    let ty = self.resolve_ast_type(v.ty, ctx.reborrow())?;

                    let init = v
                        .init
                        .map(|init| self.constant(init, ctx.reborrow()))
                        .transpose()?;

                    let handle = ctx.module.global_variables.append(
                        crate::GlobalVariable {
                            name: Some(v.name.name.to_string()),
                            space: v.space,
                            binding: v.binding.clone(),
                            ty,
                            init,
                        },
                        span,
                    );

                    ctx.globals
                        .insert(v.name.name, LoweredGlobalDecl::Var(handle));
                }
                ast::GlobalDeclKind::Const(ref c) => {
                    let inner = self.constant_inner(c.init, ctx.reborrow())?;
                    let inner = match inner {
                        ConstantOrInner::Constant(c) => ctx.module.constants[c].inner.clone(),
                        ConstantOrInner::Inner(inner) => inner,
                    };

                    let inferred_type = match inner {
                        crate::ConstantInner::Scalar { width, value } => {
                            ctx.ensure_type_exists(crate::TypeInner::Scalar {
                                width,
                                kind: value.scalar_kind(),
                            })
                        }
                        crate::ConstantInner::Composite { ty, .. } => ty,
                    };

                    let handle = ctx.module.constants.append(
                        crate::Constant {
                            name: Some(c.name.name.to_string()),
                            specialization: None,
                            inner,
                        },
                        span,
                    );

                    let explicit_ty =
                        c.ty.map(|ty| self.resolve_ast_type(ty, ctx.reborrow()))
                            .transpose()?;

                    if let Some(explicit) = explicit_ty {
                        if explicit != inferred_type {
                            let ty = &ctx.module.types[explicit];
                            let explicit = ty.name.clone().unwrap_or_else(|| {
                                ty.inner.to_wgsl(&ctx.module.types, &ctx.module.constants)
                            });

                            let ty = &ctx.module.types[inferred_type];
                            let inferred = ty.name.clone().unwrap_or_else(|| {
                                ty.inner.to_wgsl(&ctx.module.types, &ctx.module.constants)
                            });

                            return Err(Error::InitializationTypeMismatch(
                                c.name.span,
                                explicit,
                                inferred,
                            ));
                        }
                    }

                    ctx.globals
                        .insert(c.name.name, LoweredGlobalDecl::Const(handle));
                }
                ast::GlobalDeclKind::Struct(ref s) => {
                    let handle = self.r#struct(s, span, ctx.reborrow())?;
                    ctx.globals
                        .insert(s.name.name, LoweredGlobalDecl::Type(handle));
                }
                ast::GlobalDeclKind::Type(ref alias) => {
                    let ty = self.resolve_ast_type(alias.ty, ctx.reborrow())?;
                    ctx.globals
                        .insert(alias.name.name, LoweredGlobalDecl::Type(ty));
                }
            }
        }

        Ok(module)
    }

    fn function(
        &mut self,
        f: &ast::Function<'source>,
        span: Span,
        mut ctx: OutputContext<'source, '_, '_>,
    ) -> Result<LoweredGlobalDecl, Error<'source>> {
        let mut local_table = FastHashMap::default();
        let mut local_variables = Arena::new();
        let mut expressions = Arena::new();
        let mut named_expressions = IndexMap::default();

        let arguments = f
            .arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let ty = self.resolve_ast_type(arg.ty, ctx.reborrow())?;
                let expr = expressions
                    .append(crate::Expression::FunctionArgument(i as u32), arg.name.span);
                local_table.insert(arg.handle, TypedExpression::non_reference(expr));
                named_expressions.insert(expr, (arg.name.name.to_string(), arg.name.span));

                Ok(crate::FunctionArgument {
                    name: Some(arg.name.name.to_string()),
                    ty,
                    binding: self.interpolate_default(&arg.binding, ty, ctx.reborrow()),
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let result = f
            .result
            .as_ref()
            .map(|res| {
                self.resolve_ast_type(res.ty, ctx.reborrow())
                    .map(|ty| crate::FunctionResult {
                        ty,
                        binding: self.interpolate_default(&res.binding, ty, ctx.reborrow()),
                    })
            })
            .transpose()?;

        let mut typifier = Typifier::default();
        let mut body = self.block(
            &f.body,
            StatementContext {
                local_table: &mut local_table,
                globals: ctx.globals,
                ast_expressions: ctx.ast_expressions,
                typifier: &mut typifier,
                variables: &mut local_variables,
                naga_expressions: &mut expressions,
                named_expressions: &mut named_expressions,
                types: ctx.types,
                module: ctx.module,
                arguments: &arguments,
            },
        )?;
        ensure_block_returns(&mut body);

        let function = crate::Function {
            name: Some(f.name.name.to_string()),
            arguments,
            result,
            local_variables,
            expressions,
            named_expressions: named_expressions
                .into_iter()
                .map(|(key, (name, _))| (key, name))
                .collect(),
            body,
        };

        if let Some(ref entry) = f.entry_point {
            ctx.module.entry_points.push(crate::EntryPoint {
                name: f.name.name.to_string(),
                stage: entry.stage,
                early_depth_test: entry.early_depth_test,
                workgroup_size: entry.workgroup_size,
                function,
            });
            Ok(LoweredGlobalDecl::EntryPoint)
        } else {
            let handle = ctx.module.functions.append(function, span);
            Ok(LoweredGlobalDecl::Function(handle))
        }
    }

    fn block(
        &mut self,
        b: &ast::Block<'source>,
        mut ctx: StatementContext<'source, '_, '_>,
    ) -> Result<crate::Block, Error<'source>> {
        let mut block = crate::Block::default();

        for stmt in b.stmts.iter() {
            self.statement(stmt, &mut block, ctx.reborrow())?;
        }

        Ok(block)
    }

    fn statement(
        &mut self,
        stmt: &ast::Statement<'source>,
        block: &mut crate::Block,
        mut ctx: StatementContext<'source, '_, '_>,
    ) -> Result<(), Error<'source>> {
        let out = match stmt.kind {
            ast::StatementKind::Block(ref block) => {
                let block = self.block(block, ctx.reborrow())?;
                crate::Statement::Block(block)
            }
            ast::StatementKind::LocalDecl(ref decl) => match *decl {
                ast::LocalDecl::Let(ref l) => {
                    let mut emitter = Emitter::default();
                    emitter.start(ctx.naga_expressions);

                    let value = self.expression(l.init, ctx.as_expression(block, &mut emitter))?;

                    let explicit_ty =
                        l.ty.map(|ty| self.resolve_ast_type(ty, ctx.as_output()))
                            .transpose()?;

                    if let Some(ty) = explicit_ty {
                        let mut ctx = ctx.as_expression(block, &mut emitter);
                        let init_ty = ctx.register_type(value)?;
                        if !ctx.module.types[ty]
                            .inner
                            .equivalent(&ctx.module.types[init_ty].inner, &ctx.module.types)
                        {
                            return Err(Error::InitializationTypeMismatch(
                                l.name.span,
                                ctx.format_type(ty),
                                ctx.format_type(init_ty),
                            ));
                        }
                    }

                    block.extend(emitter.finish(ctx.naga_expressions));
                    ctx.local_table
                        .insert(l.handle, TypedExpression::non_reference(value));
                    ctx.named_expressions
                        .insert(value, (l.name.name.to_string(), l.name.span));

                    return Ok(());
                }
                ast::LocalDecl::Var(ref v) => {
                    let mut emitter = Emitter::default();
                    emitter.start(ctx.naga_expressions);

                    let initializer = match v.init {
                        Some(init) => {
                            let initializer =
                                self.expression(init, ctx.as_expression(block, &mut emitter))?;
                            ctx.as_expression(block, &mut emitter)
                                .grow_types(initializer)?;
                            Some(initializer)
                        }
                        None => None,
                    };

                    let explicit_ty =
                        v.ty.map(|ty| self.resolve_ast_type(ty, ctx.as_output()))
                            .transpose()?;

                    let ty = match (explicit_ty, initializer) {
                        (Some(explicit), Some(initializer)) => {
                            let ctx = ctx.as_expression(block, &mut emitter);
                            let initializer_ty = ctx.resolved_inner(initializer);
                            if !ctx.module.types[explicit]
                                .inner
                                .equivalent(initializer_ty, &ctx.module.types)
                            {
                                return Err(Error::InitializationTypeMismatch(
                                    v.name.span,
                                    ctx.format_type(explicit),
                                    ctx.format_typeinner(initializer_ty),
                                ));
                            }
                            explicit
                        }
                        (Some(explicit), None) => explicit,
                        (None, Some(initializer)) => ctx
                            .as_expression(block, &mut emitter)
                            .register_type(initializer)?,
                        (None, None) => {
                            return Err(Error::MissingType(v.name.span));
                        }
                    };

                    let var = ctx.variables.append(
                        crate::LocalVariable {
                            name: Some(v.name.name.to_string()),
                            ty,
                            init: None,
                        },
                        stmt.span,
                    );

                    let handle = ctx
                        .as_expression(block, &mut emitter)
                        .interrupt_emitter(crate::Expression::LocalVariable(var), Span::UNDEFINED);
                    block.extend(emitter.finish(ctx.naga_expressions));
                    ctx.local_table.insert(
                        v.handle,
                        TypedExpression {
                            handle,
                            is_reference: true,
                        },
                    );

                    match initializer {
                        Some(initializer) => crate::Statement::Store {
                            pointer: handle,
                            value: initializer,
                        },
                        None => return Ok(()),
                    }
                }
            },
            ast::StatementKind::If {
                condition,
                ref accept,
                ref reject,
            } => {
                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);

                let condition =
                    self.expression(condition, ctx.as_expression(block, &mut emitter))?;
                block.extend(emitter.finish(ctx.naga_expressions));

                let accept = self.block(accept, ctx.reborrow())?;
                let reject = self.block(reject, ctx.reborrow())?;

                crate::Statement::If {
                    condition,
                    accept,
                    reject,
                }
            }
            ast::StatementKind::Switch {
                selector,
                ref cases,
            } => {
                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);

                let mut ectx = ctx.as_expression(block, &mut emitter);
                let selector = self.expression(selector, ectx.reborrow())?;

                ectx.grow_types(selector)?;
                let uint =
                    ectx.resolved_inner(selector).scalar_kind() == Some(crate::ScalarKind::Uint);
                block.extend(emitter.finish(ctx.naga_expressions));

                let cases = cases
                    .iter()
                    .map(|case| {
                        Ok(crate::SwitchCase {
                            value: match case.value {
                                ast::SwitchValue::I32(value) if !uint => {
                                    crate::SwitchValue::I32(value)
                                }
                                ast::SwitchValue::U32(value) if uint => {
                                    crate::SwitchValue::U32(value)
                                }
                                ast::SwitchValue::Default => crate::SwitchValue::Default,
                                _ => {
                                    return Err(Error::InvalidSwitchValue {
                                        uint,
                                        span: case.value_span,
                                    });
                                }
                            },
                            body: self.block(&case.body, ctx.reborrow())?,
                            fall_through: case.fall_through,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                crate::Statement::Switch { selector, cases }
            }
            ast::StatementKind::Loop {
                ref body,
                ref continuing,
                break_if,
            } => {
                let body = self.block(body, ctx.reborrow())?;
                let mut continuing = self.block(continuing, ctx.reborrow())?;

                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);
                let break_if = break_if
                    .map(|expr| self.expression(expr, ctx.as_expression(block, &mut emitter)))
                    .transpose()?;
                continuing.extend(emitter.finish(ctx.naga_expressions));

                crate::Statement::Loop {
                    body,
                    continuing,
                    break_if,
                }
            }
            ast::StatementKind::Break => crate::Statement::Break,
            ast::StatementKind::Continue => crate::Statement::Continue,
            ast::StatementKind::Return { value } => {
                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);

                let value = value
                    .map(|expr| self.expression(expr, ctx.as_expression(block, &mut emitter)))
                    .transpose()?;
                block.extend(emitter.finish(ctx.naga_expressions));

                crate::Statement::Return { value }
            }
            ast::StatementKind::Kill => crate::Statement::Kill,
            ast::StatementKind::Call {
                ref function,
                ref arguments,
            } => {
                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);

                let _ = self.call(
                    stmt.span,
                    function,
                    arguments,
                    ctx.as_expression(block, &mut emitter),
                )?;
                block.extend(emitter.finish(ctx.naga_expressions));
                return Ok(());
            }
            ast::StatementKind::Assign { target, op, value } => {
                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);

                let expr =
                    self.expression_for_reference(target, ctx.as_expression(block, &mut emitter))?;
                let mut value = self.expression(value, ctx.as_expression(block, &mut emitter))?;

                if !expr.is_reference {
                    let ty = ctx.invalid_assignment_type(expr.handle);

                    return Err(Error::InvalidAssignment {
                        span: ctx.ast_expressions.get_span(target),
                        ty,
                    });
                }

                let value = match op {
                    Some(op) => {
                        let mut ctx = ctx.as_expression(block, &mut emitter);
                        let mut left = ctx.apply_load_rule(expr);
                        ctx.binary_op_splat(op, &mut left, &mut value)?;
                        ctx.naga_expressions.append(
                            crate::Expression::Binary {
                                op,
                                left,
                                right: value,
                            },
                            stmt.span,
                        )
                    }
                    None => value,
                };
                block.extend(emitter.finish(ctx.naga_expressions));

                crate::Statement::Store {
                    pointer: expr.handle,
                    value,
                }
            }
            ast::StatementKind::Increment(value) | ast::StatementKind::Decrement(value) => {
                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);

                let op = match stmt.kind {
                    ast::StatementKind::Increment(_) => crate::BinaryOperator::Add,
                    ast::StatementKind::Decrement(_) => crate::BinaryOperator::Subtract,
                    _ => unreachable!(),
                };

                let value_span = ctx.ast_expressions.get_span(value);
                let reference =
                    self.expression_for_reference(value, ctx.as_expression(block, &mut emitter))?;
                let mut ectx = ctx.as_expression(block, &mut emitter);

                ectx.grow_types(reference.handle)?;
                let (kind, width) = match *ectx.resolved_inner(reference.handle) {
                    crate::TypeInner::ValuePointer {
                        size: None,
                        kind,
                        width,
                        ..
                    } => (kind, width),
                    crate::TypeInner::Pointer { base, .. } => match ectx.module.types[base].inner {
                        crate::TypeInner::Scalar { kind, width } => (kind, width),
                        _ => return Err(Error::BadIncrDecrReferenceType(value_span)),
                    },
                    _ => return Err(Error::BadIncrDecrReferenceType(value_span)),
                };
                let constant_inner = crate::ConstantInner::Scalar {
                    width,
                    value: match kind {
                        crate::ScalarKind::Sint => crate::ScalarValue::Sint(1),
                        crate::ScalarKind::Uint => crate::ScalarValue::Uint(1),
                        _ => return Err(Error::BadIncrDecrReferenceType(value_span)),
                    },
                };
                let constant = ectx.module.constants.fetch_or_append(
                    crate::Constant {
                        name: None,
                        specialization: None,
                        inner: constant_inner,
                    },
                    Span::UNDEFINED,
                );

                let left = ectx.naga_expressions.append(
                    crate::Expression::Load {
                        pointer: reference.handle,
                    },
                    value_span,
                );
                let right =
                    ectx.interrupt_emitter(crate::Expression::Constant(constant), Span::UNDEFINED);
                let value = ectx
                    .naga_expressions
                    .append(crate::Expression::Binary { op, left, right }, stmt.span);

                block.extend(emitter.finish(ctx.naga_expressions));
                crate::Statement::Store {
                    pointer: reference.handle,
                    value,
                }
            }
            ast::StatementKind::Ignore(expr) => {
                let mut emitter = Emitter::default();
                emitter.start(ctx.naga_expressions);

                let _ = self.expression(expr, ctx.as_expression(block, &mut emitter))?;
                block.extend(emitter.finish(ctx.naga_expressions));
                return Ok(());
            }
        };

        block.push(out, stmt.span);

        Ok(())
    }

    fn expression(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let expr = self.expression_for_reference(expr, ctx.reborrow())?;
        Ok(ctx.apply_load_rule(expr))
    }

    fn expression_for_reference(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<TypedExpression, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let expr = &ctx.ast_expressions[expr];

        let (expr, is_reference) = match *expr {
            ast::Expression::Literal(literal) => {
                let inner = match literal {
                    ast::Literal::Number(Number::F32(f)) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Float(f as _),
                    },
                    ast::Literal::Number(Number::I32(i)) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Sint(i as _),
                    },
                    ast::Literal::Number(Number::U32(u)) => crate::ConstantInner::Scalar {
                        width: 4,
                        value: crate::ScalarValue::Uint(u as _),
                    },
                    ast::Literal::Number(_) => {
                        unreachable!("got abstract numeric type when not expected");
                    }
                    ast::Literal::Bool(b) => crate::ConstantInner::Scalar {
                        width: 1,
                        value: crate::ScalarValue::Bool(b),
                    },
                };
                let handle = ctx.module.constants.fetch_or_append(
                    crate::Constant {
                        name: None,
                        specialization: None,
                        inner,
                    },
                    Span::UNDEFINED,
                );
                let handle = ctx.interrupt_emitter(crate::Expression::Constant(handle), span);
                return Ok(TypedExpression::non_reference(handle));
            }
            ast::Expression::Ident(ast::IdentExpr::Local(local)) => {
                return Ok(ctx.local_table[&local])
            }
            ast::Expression::Ident(ast::IdentExpr::Unresolved(name)) => {
                return if let Some(global) = ctx.globals.get(name) {
                    let (expr, is_reference) = match *global {
                        LoweredGlobalDecl::Var(handle) => (
                            crate::Expression::GlobalVariable(handle),
                            ctx.module.global_variables[handle].space
                                != crate::AddressSpace::Handle,
                        ),
                        LoweredGlobalDecl::Const(handle) => {
                            (crate::Expression::Constant(handle), false)
                        }
                        _ => {
                            return Err(Error::Unexpected(span, ExpectedToken::Variable));
                        }
                    };

                    let handle = ctx.interrupt_emitter(expr, span);
                    Ok(TypedExpression {
                        handle,
                        is_reference,
                    })
                } else {
                    Err(Error::UnknownIdent(span, name))
                }
            }
            ast::Expression::Construct {
                ref ty,
                ty_span,
                ref components,
            } => {
                let handle = self.construct(span, ty, ty_span, components, ctx.reborrow())?;
                return Ok(TypedExpression::non_reference(handle));
            }
            ast::Expression::Unary { op, expr } => {
                let expr = self.expression(expr, ctx.reborrow())?;
                (crate::Expression::Unary { op, expr }, false)
            }
            ast::Expression::AddrOf(expr) => {
                // The `&` operator simply converts a reference to a pointer. And since a
                // reference is required, the Load Rule is not applied.
                let expr = self.expression_for_reference(expr, ctx.reborrow())?;
                if !expr.is_reference {
                    return Err(Error::NotReference("the operand of the `&` operator", span));
                }

                // No code is generated. We just declare the pointer a reference now.
                return Ok(TypedExpression {
                    is_reference: false,
                    ..expr
                });
            }
            ast::Expression::Deref(expr) => {
                // The pointer we dereference must be loaded.
                let pointer = self.expression(expr, ctx.reborrow())?;

                ctx.grow_types(pointer)?;
                if ctx.resolved_inner(pointer).pointer_space().is_none() {
                    return Err(Error::NotPointer(span));
                }

                return Ok(TypedExpression {
                    handle: pointer,
                    is_reference: true,
                });
            }
            ast::Expression::Binary { op, left, right } => {
                // Load both operands.
                let mut left = self.expression(left, ctx.reborrow())?;
                let mut right = self.expression(right, ctx.reborrow())?;
                ctx.binary_op_splat(op, &mut left, &mut right)?;
                (crate::Expression::Binary { op, left, right }, false)
            }
            ast::Expression::Call {
                ref function,
                ref arguments,
            } => {
                let handle = self
                    .call(span, function, arguments, ctx.reborrow())?
                    .ok_or(Error::FunctionReturnsVoid(function.span))?;
                return Ok(TypedExpression::non_reference(handle));
            }
            ast::Expression::Index { base, index } => {
                let expr = self.expression_for_reference(base, ctx.reborrow())?;
                let index = self.expression(index, ctx.reborrow())?;

                ctx.grow_types(expr.handle)?;
                let wgsl_pointer =
                    ctx.resolved_inner(expr.handle).pointer_space().is_some() && !expr.is_reference;

                if wgsl_pointer {
                    return Err(Error::Pointer(
                        "the value indexed by a `[]` subscripting expression",
                        ctx.ast_expressions.get_span(base),
                    ));
                }

                if let crate::Expression::Constant(constant) = ctx.naga_expressions[index] {
                    let span = ctx.naga_expressions.get_span(index);
                    let index = match ctx.module.constants[constant].inner {
                        crate::ConstantInner::Scalar {
                            value: crate::ScalarValue::Uint(int),
                            ..
                        } => u32::try_from(int).map_err(|_| Error::BadU32Constant(span)),
                        crate::ConstantInner::Scalar {
                            value: crate::ScalarValue::Sint(int),
                            ..
                        } => u32::try_from(int).map_err(|_| Error::BadU32Constant(span)),
                        _ => Err(Error::BadU32Constant(span)),
                    }?;

                    (
                        crate::Expression::AccessIndex {
                            base: expr.handle,
                            index,
                        },
                        expr.is_reference,
                    )
                } else {
                    (
                        crate::Expression::Access {
                            base: expr.handle,
                            index,
                        },
                        expr.is_reference,
                    )
                }
            }
            ast::Expression::Member { base, ref field } => {
                let TypedExpression {
                    handle,
                    is_reference,
                } = self.expression_for_reference(base, ctx.reborrow())?;

                ctx.grow_types(handle)?;
                let temp_inner;
                let (composite, wgsl_pointer) = match *ctx.resolved_inner(handle) {
                    crate::TypeInner::Pointer { base, .. } => {
                        (&ctx.module.types[base].inner, !is_reference)
                    }
                    crate::TypeInner::ValuePointer {
                        size: None,
                        kind,
                        width,
                        ..
                    } => {
                        temp_inner = crate::TypeInner::Scalar { kind, width };
                        (&temp_inner, !is_reference)
                    }
                    crate::TypeInner::ValuePointer {
                        size: Some(size),
                        kind,
                        width,
                        ..
                    } => {
                        temp_inner = crate::TypeInner::Vector { size, kind, width };
                        (&temp_inner, !is_reference)
                    }
                    ref other => (other, false),
                };

                if wgsl_pointer {
                    return Err(Error::Pointer(
                        "the value accessed by a `.member` expression",
                        ctx.ast_expressions.get_span(base),
                    ));
                }

                let access = match *composite {
                    crate::TypeInner::Struct { ref members, .. } => {
                        let index = members
                            .iter()
                            .position(|m| m.name.as_deref() == Some(field.name))
                            .ok_or(Error::BadAccessor(field.span))?
                            as u32;

                        (
                            crate::Expression::AccessIndex {
                                base: handle,
                                index,
                            },
                            is_reference,
                        )
                    }
                    crate::TypeInner::Vector { .. } | crate::TypeInner::Matrix { .. } => {
                        match Composition::make(field.name, field.span)? {
                            Composition::Multi(size, pattern) => {
                                let vector = ctx.apply_load_rule(TypedExpression {
                                    handle,
                                    is_reference,
                                });

                                (
                                    crate::Expression::Swizzle {
                                        size,
                                        vector,
                                        pattern,
                                    },
                                    false,
                                )
                            }
                            Composition::Single(index) => (
                                crate::Expression::AccessIndex {
                                    base: handle,
                                    index,
                                },
                                is_reference,
                            ),
                        }
                    }
                    _ => return Err(Error::BadAccessor(field.span)),
                };

                access
            }
            ast::Expression::Bitcast { expr, to, ty_span } => {
                let expr = self.expression(expr, ctx.reborrow())?;
                let to_resolved = self.resolve_ast_type(to, ctx.as_output())?;

                let kind = match ctx.module.types[to_resolved].inner {
                    crate::TypeInner::Scalar { kind, .. } => kind,
                    crate::TypeInner::Vector { kind, .. } => kind,
                    _ => {
                        let ty = &ctx.typifier[expr];
                        return Err(Error::BadTypeCast {
                            from_type: ctx.format_type_resolution(ty),
                            span: ty_span,
                            to_type: ctx.format_type(to_resolved),
                        });
                    }
                };

                (
                    crate::Expression::As {
                        expr,
                        kind,
                        convert: None,
                    },
                    false,
                )
            }
        };

        let handle = ctx.naga_expressions.append(expr, span);
        Ok(TypedExpression {
            handle,
            is_reference,
        })
    }

    /// Generate Naga IR for call expressions and statements, and type
    /// constructor expressions.
    ///
    /// The "function" being called is simply an `Ident` that we know refers to
    /// some module-scope definition.
    ///
    /// - If it is the name of a type, then the expression is a type constructor
    ///   expression: either constructing a value from components, a conversion
    ///   expression, or a zero value expression.
    ///
    /// - If it is the name of a function, then we're generating a [`Call`]
    ///   statement. We may be in the midst of generating code for an
    ///   expression, in which case we must generate an `Emit` statement to
    ///   force evaluation of the IR expressions we've generated so far, add the
    ///   `Call` statement to the current block, and then resume generating
    ///   expressions.
    ///
    /// [`Call`]: crate::Statement::Call
    fn call(
        &mut self,
        span: Span,
        function: &ast::Ident<'source>,
        arguments: &[Handle<ast::Expression<'source>>],
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<Option<Handle<crate::Expression>>, Error<'source>> {
        match ctx.globals.get(function.name) {
            Some(&LoweredGlobalDecl::Type(ty)) => {
                let handle = self.construct(
                    span,
                    &ast::ConstructorType::Type(ty),
                    function.span,
                    arguments,
                    ctx.reborrow(),
                )?;
                Ok(Some(handle))
            }
            Some(&LoweredGlobalDecl::Const(_) | &LoweredGlobalDecl::Var(_)) => {
                Err(Error::Unexpected(function.span, ExpectedToken::Function))
            }
            Some(&LoweredGlobalDecl::EntryPoint) => Err(Error::CalledEntryPoint(function.span)),
            Some(&LoweredGlobalDecl::Function(function)) => {
                let arguments = arguments
                    .iter()
                    .map(|&arg| self.expression(arg, ctx.reborrow()))
                    .collect::<Result<Vec<_>, _>>()?;

                ctx.block.extend(ctx.emitter.finish(ctx.naga_expressions));
                let result = ctx.module.functions[function].result.is_some().then(|| {
                    ctx.naga_expressions
                        .append(crate::Expression::CallResult(function), span)
                });
                ctx.emitter.start(ctx.naga_expressions);
                ctx.block.push(
                    crate::Statement::Call {
                        function,
                        arguments,
                        result,
                    },
                    span,
                );

                Ok(result)
            }
            None => {
                let span = function.span;
                let expr = if let Some(fun) = conv::map_relational_fun(function.name) {
                    let mut args = ctx.prepare_args(arguments, 1, span);
                    let argument = self.expression(args.next()?, ctx.reborrow())?;
                    args.finish()?;

                    crate::Expression::Relational { fun, argument }
                } else if let Some((axis, ctrl)) = conv::map_derivative(function.name) {
                    let mut args = ctx.prepare_args(arguments, 1, span);
                    let expr = self.expression(args.next()?, ctx.reborrow())?;
                    args.finish()?;

                    crate::Expression::Derivative { axis, ctrl, expr }
                } else if let Some(fun) = conv::map_standard_fun(function.name) {
                    let expected = fun.argument_count() as _;
                    let mut args = ctx.prepare_args(arguments, expected, span);

                    let arg = self.expression(args.next()?, ctx.reborrow())?;
                    let arg1 = args
                        .next()
                        .map(|x| self.expression(x, ctx.reborrow()))
                        .ok()
                        .transpose()?;
                    let arg2 = args
                        .next()
                        .map(|x| self.expression(x, ctx.reborrow()))
                        .ok()
                        .transpose()?;
                    let arg3 = args
                        .next()
                        .map(|x| self.expression(x, ctx.reborrow()))
                        .ok()
                        .transpose()?;

                    args.finish()?;

                    crate::Expression::Math {
                        fun,
                        arg,
                        arg1,
                        arg2,
                        arg3,
                    }
                } else if let Some(fun) = Texture::map(function.name) {
                    self.texture_sample_helper(fun, arguments, span, ctx.reborrow())?
                } else {
                    match function.name {
                        "select" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);

                            let reject = self.expression(args.next()?, ctx.reborrow())?;
                            let accept = self.expression(args.next()?, ctx.reborrow())?;
                            let condition = self.expression(args.next()?, ctx.reborrow())?;

                            args.finish()?;

                            crate::Expression::Select {
                                reject,
                                accept,
                                condition,
                            }
                        }
                        "arrayLength" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let expr = self.expression(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            crate::Expression::ArrayLength(expr)
                        }
                        "atomicLoad" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let pointer = self.atomic_pointer(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            crate::Expression::Load { pointer }
                        }
                        "atomicStore" => {
                            let mut args = ctx.prepare_args(arguments, 2, span);
                            let pointer = self.atomic_pointer(args.next()?, ctx.reborrow())?;
                            let value = self.expression(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            ctx.block.extend(ctx.emitter.finish(ctx.naga_expressions));
                            ctx.emitter.start(ctx.naga_expressions);
                            ctx.block
                                .push(crate::Statement::Store { pointer, value }, span);
                            return Ok(None);
                        }
                        "atomicAdd" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Add,
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicSub" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Subtract,
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicAnd" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::And,
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicOr" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::InclusiveOr,
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicXor" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::ExclusiveOr,
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicMin" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Min,
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicMax" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Max,
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicExchange" => {
                            return Ok(Some(self.atomic_helper(
                                span,
                                crate::AtomicFunction::Exchange { compare: None },
                                arguments,
                                ctx.reborrow(),
                            )?))
                        }
                        "atomicCompareExchangeWeak" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);

                            let pointer = self.atomic_pointer(args.next()?, ctx.reborrow())?;

                            let compare = self.expression(args.next()?, ctx.reborrow())?;

                            let value = args.next()?;
                            let value_span = ctx.ast_expressions.get_span(value);
                            let value = self.expression(value, ctx.reborrow())?;
                            ctx.grow_types(value)?;

                            args.finish()?;

                            let expression = match *ctx.resolved_inner(value) {
                                crate::TypeInner::Scalar { kind, width } => {
                                    crate::Expression::AtomicResult {
                                        //TODO: cache this to avoid generating duplicate types
                                        ty: ctx
                                            .module
                                            .generate_atomic_compare_exchange_result(kind, width),
                                        comparison: true,
                                    }
                                }
                                _ => return Err(Error::InvalidAtomicOperandType(value_span)),
                            };

                            let result = ctx.interrupt_emitter(expression, span);
                            ctx.block.push(
                                crate::Statement::Atomic {
                                    pointer,
                                    fun: crate::AtomicFunction::Exchange {
                                        compare: Some(compare),
                                    },
                                    value,
                                    result,
                                },
                                span,
                            );
                            return Ok(Some(result));
                        }
                        "storageBarrier" => {
                            ctx.prepare_args(arguments, 0, span).finish()?;

                            ctx.block
                                .push(crate::Statement::Barrier(crate::Barrier::STORAGE), span);
                            return Ok(None);
                        }
                        "workgroupBarrier" => {
                            ctx.prepare_args(arguments, 0, span).finish()?;

                            ctx.block
                                .push(crate::Statement::Barrier(crate::Barrier::WORK_GROUP), span);
                            return Ok(None);
                        }
                        "textureStore" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);

                            let image = args.next()?;
                            let image_span = ctx.ast_expressions.get_span(image);
                            let image = self.expression(image, ctx.reborrow())?;

                            let coordinate = self.expression(args.next()?, ctx.reborrow())?;

                            let (_, arrayed) = ctx.image_data(image, image_span)?;
                            let array_index = arrayed
                                .then(|| self.expression(args.next()?, ctx.reborrow()))
                                .transpose()?;

                            let value = self.expression(args.next()?, ctx.reborrow())?;

                            args.finish()?;

                            ctx.block.extend(ctx.emitter.finish(ctx.naga_expressions));
                            ctx.emitter.start(ctx.naga_expressions);
                            let stmt = crate::Statement::ImageStore {
                                image,
                                coordinate,
                                array_index,
                                value,
                            };
                            ctx.block.push(stmt, span);
                            return Ok(None);
                        }
                        "textureLoad" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);

                            let image = args.next()?;
                            let image_span = ctx.ast_expressions.get_span(image);
                            let image = self.expression(image, ctx.reborrow())?;

                            let coordinate = self.expression(args.next()?, ctx.reborrow())?;

                            let (class, arrayed) = ctx.image_data(image, image_span)?;
                            let array_index = arrayed
                                .then(|| self.expression(args.next()?, ctx.reborrow()))
                                .transpose()?;

                            let level = class
                                .is_mipmapped()
                                .then(|| self.expression(args.next()?, ctx.reborrow()))
                                .transpose()?;

                            let sample = class
                                .is_multisampled()
                                .then(|| self.expression(args.next()?, ctx.reborrow()))
                                .transpose()?;

                            args.finish()?;

                            crate::Expression::ImageLoad {
                                image,
                                coordinate,
                                array_index,
                                level,
                                sample,
                            }
                        }
                        "textureDimensions" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx.reborrow())?;
                            let level = args
                                .next()
                                .map(|arg| self.expression(arg, ctx.reborrow()))
                                .ok()
                                .transpose()?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::Size { level },
                            }
                        }
                        "textureNumLevels" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::NumLevels,
                            }
                        }
                        "textureNumLayers" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::NumLayers,
                            }
                        }
                        "textureNumSamples" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let image = self.expression(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            crate::Expression::ImageQuery {
                                image,
                                query: crate::ImageQuery::NumSamples,
                            }
                        }
                        "rayQueryInitialize" => {
                            let mut args = ctx.prepare_args(arguments, 3, span);
                            let query = self.ray_query_pointer(args.next()?, ctx.reborrow())?;
                            let acceleration_structure =
                                self.expression(args.next()?, ctx.reborrow())?;
                            let descriptor = self.expression(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            let _ = ctx.module.generate_ray_desc_type();
                            let fun = crate::RayQueryFunction::Initialize {
                                acceleration_structure,
                                descriptor,
                            };

                            ctx.block.extend(ctx.emitter.finish(ctx.naga_expressions));
                            ctx.emitter.start(ctx.naga_expressions);
                            ctx.block
                                .push(crate::Statement::RayQuery { query, fun }, span);
                            return Ok(None);
                        }
                        "rayQueryProceed" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let query = self.ray_query_pointer(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            ctx.block.extend(ctx.emitter.finish(ctx.naga_expressions));
                            let result = ctx
                                .naga_expressions
                                .append(crate::Expression::RayQueryProceedResult, span);
                            let fun = crate::RayQueryFunction::Proceed { result };

                            ctx.emitter.start(ctx.naga_expressions);
                            ctx.block
                                .push(crate::Statement::RayQuery { query, fun }, span);
                            return Ok(Some(result));
                        }
                        "rayQueryGetCommittedIntersection" => {
                            let mut args = ctx.prepare_args(arguments, 1, span);
                            let query = self.ray_query_pointer(args.next()?, ctx.reborrow())?;
                            args.finish()?;

                            let _ = ctx.module.generate_ray_intersection_type();

                            crate::Expression::RayQueryGetIntersection {
                                query,
                                committed: true,
                            }
                        }
                        "RayDesc" => {
                            let ty = ctx.module.generate_ray_desc_type();
                            let handle = self.construct(
                                span,
                                &ast::ConstructorType::Type(ty),
                                function.span,
                                arguments,
                                ctx.reborrow(),
                            )?;
                            return Ok(Some(handle));
                        }
                        _ => return Err(Error::UnknownIdent(function.span, function.name)),
                    }
                };

                let expr = ctx.naga_expressions.append(expr, span);
                Ok(Some(expr))
            }
        }
    }

    fn atomic_pointer(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let pointer = self.expression(expr, ctx.reborrow())?;

        ctx.grow_types(pointer)?;
        match *ctx.resolved_inner(pointer) {
            crate::TypeInner::Pointer { base, .. } => match ctx.module.types[base].inner {
                crate::TypeInner::Atomic { .. } => Ok(pointer),
                ref other => {
                    log::error!("Pointer type to {:?} passed to atomic op", other);
                    Err(Error::InvalidAtomicPointer(span))
                }
            },
            ref other => {
                log::error!("Type {:?} passed to atomic op", other);
                Err(Error::InvalidAtomicPointer(span))
            }
        }
    }

    fn atomic_helper(
        &mut self,
        span: Span,
        fun: crate::AtomicFunction,
        args: &[Handle<ast::Expression<'source>>],
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let mut args = ctx.prepare_args(args, 2, span);

        let pointer = self.atomic_pointer(args.next()?, ctx.reborrow())?;

        let value = args.next()?;
        let value = self.expression(value, ctx.reborrow())?;
        let ty = ctx.register_type(value)?;

        args.finish()?;

        let result = ctx.interrupt_emitter(
            crate::Expression::AtomicResult {
                ty,
                comparison: false,
            },
            span,
        );
        ctx.block.push(
            crate::Statement::Atomic {
                pointer,
                fun,
                value,
                result,
            },
            span,
        );
        Ok(result)
    }

    fn texture_sample_helper(
        &mut self,
        fun: Texture,
        args: &[Handle<ast::Expression<'source>>],
        span: Span,
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<crate::Expression, Error<'source>> {
        let mut args = ctx.prepare_args(args, fun.min_argument_count(), span);

        let (image, gather) = match fun {
            Texture::Gather => {
                let image_or_component = args.next()?;
                match self.gather_component(image_or_component, ctx.reborrow())? {
                    Some(component) => {
                        let image = args.next()?;
                        (image, Some(component))
                    }
                    None => (image_or_component, Some(crate::SwizzleComponent::X)),
                }
            }
            Texture::GatherCompare => {
                let image = args.next()?;
                (image, Some(crate::SwizzleComponent::X))
            }

            _ => {
                let image = args.next()?;
                (image, None)
            }
        };

        let image_span = ctx.ast_expressions.get_span(image);
        let image = self.expression(image, ctx.reborrow())?;

        let sampler = self.expression(args.next()?, ctx.reborrow())?;

        let coordinate = self.expression(args.next()?, ctx.reborrow())?;

        let (_, arrayed) = ctx.image_data(image, image_span)?;
        let array_index = arrayed
            .then(|| self.expression(args.next()?, ctx.reborrow()))
            .transpose()?;

        let (level, depth_ref) = match fun {
            Texture::Gather => (crate::SampleLevel::Zero, None),
            Texture::GatherCompare => {
                let reference = self.expression(args.next()?, ctx.reborrow())?;
                (crate::SampleLevel::Zero, Some(reference))
            }

            Texture::Sample => (crate::SampleLevel::Auto, None),
            Texture::SampleBias => {
                let bias = self.expression(args.next()?, ctx.reborrow())?;
                (crate::SampleLevel::Bias(bias), None)
            }
            Texture::SampleCompare => {
                let reference = self.expression(args.next()?, ctx.reborrow())?;
                (crate::SampleLevel::Auto, Some(reference))
            }
            Texture::SampleCompareLevel => {
                let reference = self.expression(args.next()?, ctx.reborrow())?;
                (crate::SampleLevel::Zero, Some(reference))
            }
            Texture::SampleGrad => {
                let x = self.expression(args.next()?, ctx.reborrow())?;
                let y = self.expression(args.next()?, ctx.reborrow())?;
                (crate::SampleLevel::Gradient { x, y }, None)
            }
            Texture::SampleLevel => {
                let level = self.expression(args.next()?, ctx.reborrow())?;
                (crate::SampleLevel::Exact(level), None)
            }
        };

        let offset = args
            .next()
            .map(|arg| self.constant(arg, ctx.as_output()))
            .ok()
            .transpose()?;

        args.finish()?;

        Ok(crate::Expression::ImageSample {
            image,
            sampler,
            gather,
            coordinate,
            array_index,
            offset,
            level,
            depth_ref,
        })
    }

    fn gather_component(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<Option<crate::SwizzleComponent>, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);

        let constant = match self.constant_inner(expr, ctx.as_output()).ok() {
            Some(ConstantOrInner::Constant(c)) => ctx.module.constants[c].inner.clone(),
            Some(ConstantOrInner::Inner(inner)) => inner,
            None => return Ok(None),
        };

        let int = match constant {
            crate::ConstantInner::Scalar {
                value: crate::ScalarValue::Sint(i),
                ..
            } if i >= 0 => i as u64,
            crate::ConstantInner::Scalar {
                value: crate::ScalarValue::Uint(i),
                ..
            } => i,
            _ => {
                return Err(Error::InvalidGatherComponent(span));
            }
        };

        crate::SwizzleComponent::XYZW
            .get(int as usize)
            .copied()
            .map(Some)
            .ok_or(Error::InvalidGatherComponent(span))
    }

    fn r#struct(
        &mut self,
        s: &ast::Struct<'source>,
        span: Span,
        mut ctx: OutputContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Type>, Error<'source>> {
        let mut offset = 0;
        let mut struct_alignment = Alignment::ONE;
        let mut members = Vec::with_capacity(s.members.len());

        for member in s.members.iter() {
            let ty = self.resolve_ast_type(member.ty, ctx.reborrow())?;

            self.layouter
                .update(&ctx.module.types, &ctx.module.constants)
                .unwrap();

            let member_min_size = self.layouter[ty].size;
            let member_min_alignment = self.layouter[ty].alignment;

            let member_size = if let Some((size, span)) = member.size {
                if size < member_min_size {
                    return Err(Error::SizeAttributeTooLow(span, member_min_size));
                } else {
                    size
                }
            } else {
                member_min_size
            };

            let member_alignment = if let Some((align, span)) = member.align {
                if let Some(alignment) = Alignment::new(align) {
                    if alignment < member_min_alignment {
                        return Err(Error::AlignAttributeTooLow(span, member_min_alignment));
                    } else {
                        alignment
                    }
                } else {
                    return Err(Error::NonPowerOfTwoAlignAttribute(span));
                }
            } else {
                member_min_alignment
            };

            let binding = self.interpolate_default(&member.binding, ty, ctx.reborrow());

            offset = member_alignment.round_up(offset);
            struct_alignment = struct_alignment.max(member_alignment);

            members.push(crate::StructMember {
                name: Some(member.name.name.to_owned()),
                ty,
                binding,
                offset,
            });

            offset += member_size;
        }

        let size = struct_alignment.round_up(offset);
        let inner = crate::TypeInner::Struct {
            members,
            span: size,
        };

        let handle = ctx.module.types.insert(
            crate::Type {
                name: Some(s.name.name.to_string()),
                inner,
            },
            span,
        );
        Ok(handle)
    }

    /// Return a Naga `Handle<Type>` representing the front-end type `handle`.
    fn resolve_ast_type(
        &mut self,
        handle: Handle<ast::Type<'source>>,
        mut ctx: OutputContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Type>, Error<'source>> {
        let inner = match ctx.types[handle] {
            ast::Type::Scalar { kind, width } => crate::TypeInner::Scalar { kind, width },
            ast::Type::Vector { size, kind, width } => {
                crate::TypeInner::Vector { size, kind, width }
            }
            ast::Type::Matrix {
                rows,
                columns,
                width,
            } => crate::TypeInner::Matrix {
                columns,
                rows,
                width,
            },
            ast::Type::Atomic { kind, width } => crate::TypeInner::Atomic { kind, width },
            ast::Type::Pointer { base, space } => {
                let base = self.resolve_ast_type(base, ctx.reborrow())?;
                crate::TypeInner::Pointer { base, space }
            }
            ast::Type::Array { base, size } => {
                let base = self.resolve_ast_type(base, ctx.reborrow())?;
                self.layouter
                    .update(&ctx.module.types, &ctx.module.constants)
                    .unwrap();

                crate::TypeInner::Array {
                    base,
                    size: match size {
                        ast::ArraySize::Constant(constant) => {
                            let constant = self.constant(constant, ctx.reborrow())?;
                            crate::ArraySize::Constant(constant)
                        }
                        ast::ArraySize::Dynamic => crate::ArraySize::Dynamic,
                    },
                    stride: self.layouter[base].to_stride(),
                }
            }
            ast::Type::Image {
                dim,
                arrayed,
                class,
            } => crate::TypeInner::Image {
                dim,
                arrayed,
                class,
            },
            ast::Type::Sampler { comparison } => crate::TypeInner::Sampler { comparison },
            ast::Type::AccelerationStructure => crate::TypeInner::AccelerationStructure,
            ast::Type::RayQuery => crate::TypeInner::RayQuery,
            ast::Type::BindingArray { base, size } => {
                let base = self.resolve_ast_type(base, ctx.reborrow())?;

                crate::TypeInner::BindingArray {
                    base,
                    size: match size {
                        ast::ArraySize::Constant(constant) => {
                            let constant = self.constant(constant, ctx.reborrow())?;
                            crate::ArraySize::Constant(constant)
                        }
                        ast::ArraySize::Dynamic => crate::ArraySize::Dynamic,
                    },
                }
            }
            ast::Type::RayDesc => {
                return Ok(ctx.module.generate_ray_desc_type());
            }
            ast::Type::RayIntersection => {
                return Ok(ctx.module.generate_ray_intersection_type());
            }
            ast::Type::User(ref ident) => {
                return match ctx.globals.get(ident.name) {
                    Some(&LoweredGlobalDecl::Type(handle)) => Ok(handle),
                    Some(_) => Err(Error::Unexpected(ident.span, ExpectedToken::Type)),
                    None => Err(Error::UnknownType(ident.span)),
                }
            }
        };

        Ok(ctx.ensure_type_exists(inner))
    }

    /// Find or construct a Naga [`Constant`] whose value is `expr`.
    ///
    /// The `ctx` indicates the Naga [`Module`] to which we should add
    /// new `Constant`s or [`Type`]s as needed.
    ///
    /// [`Module`]: crate::Module
    /// [`Constant`]: crate::Constant
    /// [`Type`]: crate::Type
    fn constant(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        mut ctx: OutputContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Constant>, Error<'source>> {
        let inner = match self.constant_inner(expr, ctx.reborrow())? {
            ConstantOrInner::Constant(c) => return Ok(c),
            ConstantOrInner::Inner(inner) => inner,
        };

        let c = ctx.module.constants.fetch_or_append(
            crate::Constant {
                name: None,
                specialization: None,
                inner,
            },
            Span::UNDEFINED,
        );
        Ok(c)
    }

    fn constant_inner(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        mut ctx: OutputContext<'source, '_, '_>,
    ) -> Result<ConstantOrInner, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let inner = match ctx.ast_expressions[expr] {
            ast::Expression::Literal(literal) => match literal {
                ast::Literal::Number(Number::F32(f)) => crate::ConstantInner::Scalar {
                    width: 4,
                    value: crate::ScalarValue::Float(f as _),
                },
                ast::Literal::Number(Number::I32(i)) => crate::ConstantInner::Scalar {
                    width: 4,
                    value: crate::ScalarValue::Sint(i as _),
                },
                ast::Literal::Number(Number::U32(u)) => crate::ConstantInner::Scalar {
                    width: 4,
                    value: crate::ScalarValue::Uint(u as _),
                },
                ast::Literal::Number(_) => {
                    unreachable!("got abstract numeric type when not expected");
                }
                ast::Literal::Bool(b) => crate::ConstantInner::Scalar {
                    width: 1,
                    value: crate::ScalarValue::Bool(b),
                },
            },
            ast::Expression::Ident(ast::IdentExpr::Local(_)) => {
                return Err(Error::Unexpected(span, ExpectedToken::Constant))
            }
            ast::Expression::Ident(ast::IdentExpr::Unresolved(name)) => {
                return if let Some(global) = ctx.globals.get(name) {
                    match *global {
                        LoweredGlobalDecl::Const(handle) => Ok(ConstantOrInner::Constant(handle)),
                        _ => Err(Error::Unexpected(span, ExpectedToken::Constant)),
                    }
                } else {
                    Err(Error::UnknownIdent(span, name))
                }
            }
            ast::Expression::Construct {
                ref ty,
                ref components,
                ..
            } => self.const_construct(span, ty, components, ctx.reborrow())?,
            ast::Expression::Call {
                ref function,
                ref arguments,
            } => match ctx.globals.get(function.name) {
                Some(&LoweredGlobalDecl::Type(ty)) => self.const_construct(
                    span,
                    &ast::ConstructorType::Type(ty),
                    arguments,
                    ctx.reborrow(),
                )?,
                Some(_) => return Err(Error::ConstExprUnsupported(span)),
                None => return Err(Error::UnknownIdent(function.span, function.name)),
            },
            _ => return Err(Error::ConstExprUnsupported(span)),
        };

        Ok(ConstantOrInner::Inner(inner))
    }

    fn interpolate_default(
        &mut self,
        binding: &Option<crate::Binding>,
        ty: Handle<crate::Type>,
        ctx: OutputContext<'source, '_, '_>,
    ) -> Option<crate::Binding> {
        let mut binding = binding.clone();
        if let Some(ref mut binding) = binding {
            binding.apply_default_interpolation(&ctx.module.types[ty].inner);
        }

        binding
    }

    fn ray_query_pointer(
        &mut self,
        expr: Handle<ast::Expression<'source>>,
        mut ctx: ExpressionContext<'source, '_, '_>,
    ) -> Result<Handle<crate::Expression>, Error<'source>> {
        let span = ctx.ast_expressions.get_span(expr);
        let pointer = self.expression(expr, ctx.reborrow())?;

        ctx.grow_types(pointer)?;
        match *ctx.resolved_inner(pointer) {
            crate::TypeInner::Pointer { base, .. } => match ctx.module.types[base].inner {
                crate::TypeInner::RayQuery => Ok(pointer),
                ref other => {
                    log::error!("Pointer type to {:?} passed to ray query op", other);
                    Err(Error::InvalidRayQueryPointer(span))
                }
            },
            ref other => {
                log::error!("Type {:?} passed to ray query op", other);
                Err(Error::InvalidRayQueryPointer(span))
            }
        }
    }
}
