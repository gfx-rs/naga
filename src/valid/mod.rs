/*!
Shader validator.
*/

mod analyzer;
mod compose;
#[cfg(feature = "validate")]
mod expression;
mod function;
mod handles;
mod interface;
mod r#type;

use crate::{
    arena::Handle,
    proc::{IndexableLengthError, LayoutError, Layouter, ResolveError, TypeResolution},
    FastHashSet,
};
use bit_set::BitSet;
use std::ops;

//TODO: analyze the model at the same time as we validate it,
// merge the corresponding matches over expressions and statements.

use crate::span::{AddSpan as _, WithSpan};
pub use analyzer::{ExpressionInfo, FunctionInfo, GlobalUse, Uniformity, UniformityRequirements};
pub use compose::ComposeError;
pub use function::{CallError, FunctionError, LocalVariableError};
pub use interface::{EntryPointError, GlobalVariableError, VaryingError};
pub use r#type::{Disalignment, TypeError, TypeFlags};

use self::handles::InvalidHandleError;

bitflags::bitflags! {
    /// Validation flags.
    ///
    /// If you are working with trusted shaders, then you may be able
    /// to save some time by skipping validation.
    ///
    /// If you do not perform full validation, invalid shaders may
    /// cause Naga to panic. If you do perform full validation and
    /// [`Validator::validate`] returns `Ok`, then Naga promises that
    /// code generation will either succeed or return an error; it
    /// should never panic.
    ///
    /// The default value for `ValidationFlags` is
    /// `ValidationFlags::all()`. If Naga's `"validate"` feature is
    /// enabled, this requests full validation; otherwise, this
    /// requests no validation. (The `"validate"` feature is disabled
    /// by default.)
    #[cfg_attr(feature = "serialize", derive(serde::Serialize))]
    #[cfg_attr(feature = "deserialize", derive(serde::Deserialize))]
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct ValidationFlags: u8 {
        /// Expressions.
        #[cfg(feature = "validate")]
        const EXPRESSIONS = 0x1;
        /// Statements and blocks of them.
        #[cfg(feature = "validate")]
        const BLOCKS = 0x2;
        /// Uniformity of control flow for operations that require it.
        #[cfg(feature = "validate")]
        const CONTROL_FLOW_UNIFORMITY = 0x4;
        /// Host-shareable structure layouts.
        #[cfg(feature = "validate")]
        const STRUCT_LAYOUTS = 0x8;
        /// Constants.
        #[cfg(feature = "validate")]
        const CONSTANTS = 0x10;
        /// Group, binding, and location attributes.
        #[cfg(feature = "validate")]
        const BINDINGS = 0x20;
    }
}

impl Default for ValidationFlags {
    fn default() -> Self {
        Self::all()
    }
}

bitflags::bitflags! {
    /// Allowed IR capabilities.
    #[must_use]
    #[cfg_attr(feature = "serialize", derive(serde::Serialize))]
    #[cfg_attr(feature = "deserialize", derive(serde::Deserialize))]
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct Capabilities: u16 {
        /// Support for [`AddressSpace:PushConstant`].
        const PUSH_CONSTANT = 0x1;
        /// Float values with width = 8.
        const FLOAT64 = 0x2;
        /// Support for [`Builtin:PrimitiveIndex`].
        const PRIMITIVE_INDEX = 0x4;
        /// Support for non-uniform indexing of sampled textures and storage buffer arrays.
        const SAMPLED_TEXTURE_AND_STORAGE_BUFFER_ARRAY_NON_UNIFORM_INDEXING = 0x8;
        /// Support for non-uniform indexing of uniform buffers and storage texture arrays.
        const UNIFORM_BUFFER_AND_STORAGE_TEXTURE_ARRAY_NON_UNIFORM_INDEXING = 0x10;
        /// Support for non-uniform indexing of samplers.
        const SAMPLER_NON_UNIFORM_INDEXING = 0x20;
        /// Support for [`Builtin::ClipDistance`].
        const CLIP_DISTANCE = 0x40;
        /// Support for [`Builtin::CullDistance`].
        const CULL_DISTANCE = 0x80;
        /// Support for 16-bit normalized storage texture formats.
        const STORAGE_TEXTURE_16BIT_NORM_FORMATS = 0x100;
        /// Support for [`BuiltIn::ViewIndex`].
        const MULTIVIEW = 0x200;
        /// Support for `early_depth_test`.
        const EARLY_DEPTH_TEST = 0x400;
        /// Support for [`Builtin::SampleIndex`] and [`Sampling::Sample`].
        const MULTISAMPLED_SHADING = 0x800;
        /// Support for ray queries and acceleration structures.
        const RAY_QUERY = 0x1000;
        /// Support for generating two sources for blending from fragement shaders
        const DUAL_SOURCE_BLENDING = 0x2000;
    }
}

impl Default for Capabilities {
    fn default() -> Self {
        Self::MULTISAMPLED_SHADING
    }
}

bitflags::bitflags! {
    /// Validation flags.
    #[cfg_attr(feature = "serialize", derive(serde::Serialize))]
    #[cfg_attr(feature = "deserialize", derive(serde::Deserialize))]
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct ShaderStages: u8 {
        const VERTEX = 0x1;
        const FRAGMENT = 0x2;
        const COMPUTE = 0x4;
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
#[cfg_attr(feature = "deserialize", derive(serde::Deserialize))]
pub struct ModuleInfo {
    type_flags: Vec<TypeFlags>,
    functions: Vec<FunctionInfo>,
    entry_points: Vec<FunctionInfo>,
    const_expression_types: Box<[TypeResolution]>,
}

impl ops::Index<Handle<crate::Type>> for ModuleInfo {
    type Output = TypeFlags;
    fn index(&self, handle: Handle<crate::Type>) -> &Self::Output {
        &self.type_flags[handle.index()]
    }
}

impl ops::Index<Handle<crate::Function>> for ModuleInfo {
    type Output = FunctionInfo;
    fn index(&self, handle: Handle<crate::Function>) -> &Self::Output {
        &self.functions[handle.index()]
    }
}

impl ops::Index<Handle<crate::Expression>> for ModuleInfo {
    type Output = TypeResolution;
    fn index(&self, handle: Handle<crate::Expression>) -> &Self::Output {
        &self.const_expression_types[handle.index()]
    }
}

#[derive(Debug)]
pub struct Validator {
    flags: ValidationFlags,
    capabilities: Capabilities,
    types: Vec<r#type::TypeInfo>,
    layouter: Layouter,
    location_mask: BitSet,
    bind_group_masks: Vec<BitSet>,
    #[allow(dead_code)]
    switch_values: FastHashSet<crate::SwitchValue>,
    valid_expression_list: Vec<Handle<crate::Expression>>,
    valid_expression_set: BitSet,
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ConstExpressionError {
    #[error("The expression is not a constant expression")]
    NonConst,
    #[error(transparent)]
    Compose(#[from] ComposeError),
    #[error("Type resolution failed")]
    Type(#[from] ResolveError),
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ConstantError {
    #[error("The type doesn't match the constant")]
    InvalidType,
    #[error("The type is not constructible")]
    NonConstructibleType,
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ValidationError {
    #[error(transparent)]
    InvalidHandle(#[from] InvalidHandleError),
    #[error(transparent)]
    Layouter(#[from] LayoutError),
    #[error("Type {handle:?} '{name}' is invalid")]
    Type {
        handle: Handle<crate::Type>,
        name: String,
        source: TypeError,
    },
    #[error("Constant expression {handle:?} is invalid")]
    ConstExpression {
        handle: Handle<crate::Expression>,
        source: ConstExpressionError,
    },
    #[error("Constant {handle:?} '{name}' is invalid")]
    Constant {
        handle: Handle<crate::Constant>,
        name: String,
        source: ConstantError,
    },
    #[error("Global variable {handle:?} '{name}' is invalid")]
    GlobalVariable {
        handle: Handle<crate::GlobalVariable>,
        name: String,
        source: GlobalVariableError,
    },
    #[error("Function {handle:?} '{name}' is invalid")]
    Function {
        handle: Handle<crate::Function>,
        name: String,
        source: FunctionError,
    },
    #[error("Entry point {name} at {stage:?} is invalid")]
    EntryPoint {
        stage: crate::ShaderStage,
        name: String,
        source: EntryPointError,
    },
    #[error("Module is corrupted")]
    Corrupted,
}

#[derive(Clone, Debug, thiserror::Error)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ExpressionError {
    #[error("Doesn't exist")]
    DoesntExist,
    #[error("Used by a statement before it was introduced into the scope by any of the dominating blocks")]
    NotInScope,
    #[error("Base type {0:?} is not compatible with this expression")]
    InvalidBaseType(Handle<crate::Expression>),
    #[error("Accessing with index {0:?} can't be done")]
    InvalidIndexType(Handle<crate::Expression>),
    #[error("Accessing {0:?} via a negative index is invalid")]
    NegativeIndex(Handle<crate::Expression>),
    #[error("Accessing index {1} is out of {0:?} bounds")]
    IndexOutOfBounds(Handle<crate::Expression>, u32),
    #[error("The expression {0:?} may only be indexed by a constant")]
    IndexMustBeConstant(Handle<crate::Expression>),
    #[error("Function argument {0:?} doesn't exist")]
    FunctionArgumentDoesntExist(u32),
    #[error("Loading of {0:?} can't be done")]
    InvalidPointerType(Handle<crate::Expression>),
    #[error("Array length of {0:?} can't be done")]
    InvalidArrayType(Handle<crate::Expression>),
    #[error("Get intersection of {0:?} can't be done")]
    InvalidRayQueryType(Handle<crate::Expression>),
    #[error("Splatting {0:?} can't be done")]
    InvalidSplatType(Handle<crate::Expression>),
    #[error("Swizzling {0:?} can't be done")]
    InvalidVectorType(Handle<crate::Expression>),
    #[error("Swizzle component {0:?} is outside of vector size {1:?}")]
    InvalidSwizzleComponent(crate::SwizzleComponent, crate::VectorSize),
    #[error(transparent)]
    Compose(#[from] ComposeError),
    #[error(transparent)]
    IndexableLength(#[from] IndexableLengthError),
    #[error("Operation {0:?} can't work with {1:?}")]
    InvalidUnaryOperandType(crate::UnaryOperator, Handle<crate::Expression>),
    #[error("Operation {0:?} can't work with {1:?} and {2:?}")]
    InvalidBinaryOperandTypes(
        crate::BinaryOperator,
        Handle<crate::Expression>,
        Handle<crate::Expression>,
    ),
    #[error("Selecting is not possible")]
    InvalidSelectTypes,
    #[error("Relational argument {0:?} is not a boolean vector")]
    InvalidBooleanVector(Handle<crate::Expression>),
    #[error("Relational argument {0:?} is not a float")]
    InvalidFloatArgument(Handle<crate::Expression>),
    #[error("Type resolution failed")]
    Type(#[from] ResolveError),
    #[error("Not a global variable")]
    ExpectedGlobalVariable,
    #[error("Not a global variable or a function argument")]
    ExpectedGlobalOrArgument,
    #[error("Needs to be an binding array instead of {0:?}")]
    ExpectedBindingArrayType(Handle<crate::Type>),
    #[error("Needs to be an image instead of {0:?}")]
    ExpectedImageType(Handle<crate::Type>),
    #[error("Needs to be an image instead of {0:?}")]
    ExpectedSamplerType(Handle<crate::Type>),
    #[error("Unable to operate on image class {0:?}")]
    InvalidImageClass(crate::ImageClass),
    #[error("Derivatives can only be taken from scalar and vector floats")]
    InvalidDerivative,
    #[error("Image array index parameter is misplaced")]
    InvalidImageArrayIndex,
    #[error("Inappropriate sample or level-of-detail index for texel access")]
    InvalidImageOtherIndex,
    #[error("Image array index type of {0:?} is not an integer scalar")]
    InvalidImageArrayIndexType(Handle<crate::Expression>),
    #[error("Image sample or level-of-detail index's type of {0:?} is not an integer scalar")]
    InvalidImageOtherIndexType(Handle<crate::Expression>),
    #[error("Image coordinate type of {1:?} does not match dimension {0:?}")]
    InvalidImageCoordinateType(crate::ImageDimension, Handle<crate::Expression>),
    #[error("Comparison sampling mismatch: image has class {image:?}, but the sampler is comparison={sampler}, and the reference was provided={has_ref}")]
    ComparisonSamplingMismatch {
        image: crate::ImageClass,
        sampler: bool,
        has_ref: bool,
    },
    #[error("Sample offset constant {1:?} doesn't match the image dimension {0:?}")]
    InvalidSampleOffset(crate::ImageDimension, Handle<crate::Expression>),
    #[error("Depth reference {0:?} is not a scalar float")]
    InvalidDepthReference(Handle<crate::Expression>),
    #[error("Depth sample level can only be Auto or Zero")]
    InvalidDepthSampleLevel,
    #[error("Gather level can only be Zero")]
    InvalidGatherLevel,
    #[error("Gather component {0:?} doesn't exist in the image")]
    InvalidGatherComponent(crate::SwizzleComponent),
    #[error("Gather can't be done for image dimension {0:?}")]
    InvalidGatherDimension(crate::ImageDimension),
    #[error("Sample level (exact) type {0:?} is not a scalar float")]
    InvalidSampleLevelExactType(Handle<crate::Expression>),
    #[error("Sample level (bias) type {0:?} is not a scalar float")]
    InvalidSampleLevelBiasType(Handle<crate::Expression>),
    #[error("Sample level (gradient) of {1:?} doesn't match the image dimension {0:?}")]
    InvalidSampleLevelGradientType(crate::ImageDimension, Handle<crate::Expression>),
    #[error("Unable to cast")]
    InvalidCastArgument,
    #[error("Invalid argument count for {0:?}")]
    WrongArgumentCount(crate::MathFunction),
    #[error("Argument [{1}] to {0:?} as expression {2:?} has an invalid type.")]
    InvalidArgumentType(crate::MathFunction, u32, Handle<crate::Expression>),
    #[error("Atomic result type can't be {0:?}")]
    InvalidAtomicResultType(Handle<crate::Type>),
    #[error(
        "workgroupUniformLoad result type can't be {0:?}. It can only be a constructible type."
    )]
    InvalidWorkGroupUniformLoadResultType(Handle<crate::Type>),
    #[error("Shader requires capability {0:?}")]
    MissingCapabilities(Capabilities),
}

impl crate::TypeInner {
    #[cfg(feature = "validate")]
    const fn is_sized(&self) -> bool {
        match *self {
            Self::Scalar { .. }
            | Self::Vector { .. }
            | Self::Matrix { .. }
            | Self::Array {
                size: crate::ArraySize::Constant(_),
                ..
            }
            | Self::Atomic { .. }
            | Self::Pointer { .. }
            | Self::ValuePointer { .. }
            | Self::Struct { .. } => true,
            Self::Array { .. }
            | Self::Image { .. }
            | Self::Sampler { .. }
            | Self::AccelerationStructure
            | Self::RayQuery
            | Self::BindingArray { .. } => false,
        }
    }

    /// Return the `ImageDimension` for which `self` is an appropriate coordinate.
    #[cfg(feature = "validate")]
    const fn image_storage_coordinates(&self) -> Option<crate::ImageDimension> {
        match *self {
            Self::Scalar {
                kind: crate::ScalarKind::Sint | crate::ScalarKind::Uint,
                ..
            } => Some(crate::ImageDimension::D1),
            Self::Vector {
                size: crate::VectorSize::Bi,
                kind: crate::ScalarKind::Sint | crate::ScalarKind::Uint,
                ..
            } => Some(crate::ImageDimension::D2),
            Self::Vector {
                size: crate::VectorSize::Tri,
                kind: crate::ScalarKind::Sint | crate::ScalarKind::Uint,
                ..
            } => Some(crate::ImageDimension::D3),
            _ => None,
        }
    }
}

impl Validator {
    /// Construct a new validator instance.
    pub fn new(flags: ValidationFlags, capabilities: Capabilities) -> Self {
        Validator {
            flags,
            capabilities,
            types: Vec::new(),
            layouter: Layouter::default(),
            location_mask: BitSet::new(),
            bind_group_masks: Vec::new(),
            switch_values: FastHashSet::default(),
            valid_expression_list: Vec::new(),
            valid_expression_set: BitSet::new(),
        }
    }

    /// Reset the validator internals
    pub fn reset(&mut self) {
        self.types.clear();
        self.layouter.clear();
        self.location_mask.clear();
        self.bind_group_masks.clear();
        self.switch_values.clear();
        self.valid_expression_list.clear();
        self.valid_expression_set.clear();
    }

    #[cfg(feature = "validate")]
    fn validate_constant(
        &self,
        handle: Handle<crate::Constant>,
        gctx: crate::proc::GlobalCtx,
        mod_info: &ModuleInfo,
    ) -> Result<(), ConstantError> {
        let con = &gctx.constants[handle];

        let type_info = &self.types[con.ty.index()];
        if !type_info.flags.contains(TypeFlags::CONSTRUCTIBLE) {
            return Err(ConstantError::NonConstructibleType);
        }

        let decl_ty = &gctx.types[con.ty].inner;
        let init_ty = mod_info[con.init].inner_with(gctx.types);
        if !decl_ty.equivalent(init_ty, gctx.types) {
            return Err(ConstantError::InvalidType);
        }

        Ok(())
    }

    /// Check the given module to be valid.
    pub fn validate(
        &mut self,
        module: &crate::Module,
    ) -> Result<ModuleInfo, WithSpan<ValidationError>> {
        self.reset();
        self.reset_types(module.types.len());

        #[cfg(feature = "validate")]
        Self::validate_module_handles(module).map_err(|e| e.with_span())?;

        self.layouter.update(module.to_ctx()).map_err(|e| {
            let handle = e.ty;
            ValidationError::from(e).with_span_handle(handle, &module.types)
        })?;

        let placeholder = TypeResolution::Value(crate::TypeInner::Scalar {
            kind: crate::ScalarKind::Bool,
            width: 0,
        });

        let mut mod_info = ModuleInfo {
            type_flags: Vec::with_capacity(module.types.len()),
            functions: Vec::with_capacity(module.functions.len()),
            entry_points: Vec::with_capacity(module.entry_points.len()),
            const_expression_types: vec![placeholder; module.const_expressions.len()]
                .into_boxed_slice(),
        };

        for (handle, ty) in module.types.iter() {
            let ty_info = self
                .validate_type(handle, module.to_ctx())
                .map_err(|source| {
                    ValidationError::Type {
                        handle,
                        name: ty.name.clone().unwrap_or_default(),
                        source,
                    }
                    .with_span_handle(handle, &module.types)
                })?;
            mod_info.type_flags.push(ty_info.flags);
            self.types[handle.index()] = ty_info;
        }

        {
            let t = crate::Arena::new();
            let resolve_context = crate::proc::ResolveContext::with_locals(module, &t, &[]);
            for (handle, _) in module.const_expressions.iter() {
                mod_info
                    .process_const_expression(handle, &resolve_context, module.to_ctx())
                    .map_err(|source| {
                        ValidationError::ConstExpression { handle, source }
                            .with_span_handle(handle, &module.const_expressions)
                    })?
            }
        }

        #[cfg(feature = "validate")]
        if self.flags.contains(ValidationFlags::CONSTANTS) {
            for (handle, _) in module.const_expressions.iter() {
                self.validate_const_expression(handle, module.to_ctx(), &mut mod_info)
                    .map_err(|source| {
                        ValidationError::ConstExpression { handle, source }
                            .with_span_handle(handle, &module.const_expressions)
                    })?
            }

            for (handle, constant) in module.constants.iter() {
                self.validate_constant(handle, module.to_ctx(), &mod_info)
                    .map_err(|source| {
                        ValidationError::Constant {
                            handle,
                            name: constant.name.clone().unwrap_or_default(),
                            source,
                        }
                        .with_span_handle(handle, &module.constants)
                    })?
            }
        }

        #[cfg(feature = "validate")]
        for (var_handle, var) in module.global_variables.iter() {
            self.validate_global_var(var, module.to_ctx(), &mod_info)
                .map_err(|source| {
                    ValidationError::GlobalVariable {
                        handle: var_handle,
                        name: var.name.clone().unwrap_or_default(),
                        source,
                    }
                    .with_span_handle(var_handle, &module.global_variables)
                })?;
        }

        for (handle, fun) in module.functions.iter() {
            match self.validate_function(fun, module, &mod_info, false) {
                Ok(info) => mod_info.functions.push(info),
                Err(error) => {
                    return Err(error.and_then(|source| {
                        ValidationError::Function {
                            handle,
                            name: fun.name.clone().unwrap_or_default(),
                            source,
                        }
                        .with_span_handle(handle, &module.functions)
                    }))
                }
            }
        }

        let mut ep_map = FastHashSet::default();
        for ep in module.entry_points.iter() {
            if !ep_map.insert((ep.stage, &ep.name)) {
                return Err(ValidationError::EntryPoint {
                    stage: ep.stage,
                    name: ep.name.clone(),
                    source: EntryPointError::Conflict,
                }
                .with_span()); // TODO: keep some EP span information?
            }

            match self.validate_entry_point(ep, module, &mod_info) {
                Ok(info) => mod_info.entry_points.push(info),
                Err(error) => {
                    return Err(error.and_then(|source| {
                        ValidationError::EntryPoint {
                            stage: ep.stage,
                            name: ep.name.clone(),
                            source,
                        }
                        .with_span()
                    }));
                }
            }
        }

        Ok(mod_info)
    }
}

#[cfg(feature = "validate")]
fn validate_atomic_compare_exchange_struct(
    types: &crate::UniqueArena<crate::Type>,
    members: &[crate::StructMember],
    scalar_predicate: impl FnOnce(&crate::TypeInner) -> bool,
) -> bool {
    members.len() == 2
        && members[0].name.as_deref() == Some("old_value")
        && scalar_predicate(&types[members[0].ty].inner)
        && members[1].name.as_deref() == Some("exchanged")
        && types[members[1].ty].inner
            == crate::TypeInner::Scalar {
                kind: crate::ScalarKind::Bool,
                width: crate::BOOL_WIDTH,
            }
}
