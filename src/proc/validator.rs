use super::typifier::{ResolveContext, ResolveError, Typifier};
use crate::arena::{Arena, Handle};
use bit_set::BitSet;

const MAX_WORKGROUP_SIZE: u32 = 0x4000;

#[derive(Debug)]
pub struct Validator {
    //Note: this is a bit tricky: some of the front-ends as well as backends
    // already have to use the typifier, so the work here is redundant in a way.
    typifier: Typifier,
    location_in_mask: BitSet,
    location_out_mask: BitSet,
    bind_group_masks: Vec<BitSet>,
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum TypeError {
    #[error("The {0:?} scalar width {1} is not supported")]
    InvalidWidth(crate::ScalarKind, crate::Bytes),
    #[error("The base handle {0:?} can not be resolved")]
    UnresolvedBase(Handle<crate::Type>),
    #[error("The constant {0:?} can not be used for an array size")]
    InvalidArraySizeConstant(Handle<crate::Constant>),
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum ConstantError {
    #[error("The type doesn't match the constant")]
    InvalidType,
    #[error("The component handle {0:?} can not be resolved")]
    UnresolvedComponent(Handle<crate::Constant>),
    #[error("The array size handle {0:?} can not be resolved")]
    UnresolvedSize(Handle<crate::Constant>),
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum GlobalVariableError {
    #[error("Usage isn't compatible with the storage class")]
    InvalidUsage,
    #[error("Type isn't compatible with the storage class")]
    InvalidType,
    #[error("Structure doesn't have a block decoration, can't be host-shared")]
    MissingBlockDecoration,
    #[error("Interpolation is not valid")]
    InvalidInterpolation,
    #[error("Storage access {seen:?} exceed the allowed {allowed:?}")]
    InvalidStorageAccess {
        allowed: crate::StorageAccess,
        seen: crate::StorageAccess,
    },
    #[error("Binding decoration is missing or not applicable")]
    InvalidBinding,
    #[error("BuiltIn type for {0:?} is invalid")]
    InvalidBuiltInType(crate::BuiltIn),
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum LocalVariableError {
    #[error("Initializer doesn't match the variable type")]
    InitializerType,
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum FunctionError {
    #[error(transparent)]
    Resolve(#[from] ResolveError),
    #[error("There are instructions after `return`/`break`/`continue`")]
    InvalidControlFlowExitTail,
    #[error("The global use array has a different length from the global variables")]
    InvalidGlobalUsageLength,
    #[error("Local variable {handle:?} '{name}' is invalid: {error:?}")]
    LocalVariable {
        handle: Handle<crate::LocalVariable>,
        name: String,
        error: LocalVariableError,
    },
    #[error("Argument '{name}' at index {index} has a type that can't be passed into functions.")]
    InvalidArgumentType { index: usize, name: String },
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum EntryPointError {
    #[error("Early depth test is not applicable")]
    UnexpectedEarlyDepthTest,
    #[error("Workgroup size is not applicable")]
    UnexpectedWorkgroupSize,
    #[error("Workgroup size is out of range")]
    OutOfRangeWorkgroupSize,
    #[error("Can't have arguments")]
    UnexpectedArguments,
    #[error("Can't have a return value")]
    UnexpectedReturnValue,
    #[error("Global variable {0:?} is used incorrectly as {1:?}")]
    InvalidGlobalUsage(Handle<crate::GlobalVariable>, crate::GlobalUse),
    #[error("Bindings for {0:?} conflict with other global variables")]
    BindingCollision(Handle<crate::GlobalVariable>),
    #[error("Built-in {0:?} is not applicable to this entry point")]
    InvalidBuiltIn(crate::BuiltIn),
    #[error("Interpolation of an integer has to be flat")]
    InvalidIntegerInterpolation,
    #[error(transparent)]
    Function(#[from] FunctionError),
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum ValidationError {
    #[error("Type {handle:?} '{name}' is invalid: {error:?}")]
    Type {
        handle: Handle<crate::Type>,
        name: String,
        error: TypeError,
    },
    #[error("Constant {handle:?} '{name}' is invalid: {error:?}")]
    Constant {
        handle: Handle<crate::Constant>,
        name: String,
        error: ConstantError,
    },
    #[error("Global variable {handle:?} '{name}' is invalid: {error:?}")]
    GlobalVariable {
        handle: Handle<crate::GlobalVariable>,
        name: String,
        error: GlobalVariableError,
    },
    #[error("Function {0:?} is invalid: {1:?}")]
    Function(Handle<crate::Function>, FunctionError),
    #[error("Entry point {name} at {stage:?} is invalid: {error:?}")]
    EntryPoint {
        stage: crate::ShaderStage,
        name: String,
        error: EntryPointError,
    },
    #[error("Module is corrupted")]
    Corrupted,
}

impl crate::GlobalVariable {
    fn forbid_interpolation(&self) -> Result<(), GlobalVariableError> {
        match self.interpolation {
            Some(_) => Err(GlobalVariableError::InvalidInterpolation),
            None => Ok(()),
        }
    }

    fn check_resource(&self) -> Result<(), GlobalVariableError> {
        match self.binding {
            Some(crate::Binding::Resource { .. }) => {}
            Some(crate::Binding::BuiltIn(_)) | Some(crate::Binding::Location(_)) | None => {
                return Err(GlobalVariableError::InvalidBinding)
            }
        }
        self.forbid_interpolation()
    }

    fn check_varying(&self, types: &Arena<crate::Type>) -> Result<(), GlobalVariableError> {
        match self.binding {
            Some(crate::Binding::BuiltIn(built_in)) => {
                use crate::{BuiltIn as Bi, ScalarKind as Sk, TypeInner as Ti, VectorSize as Vs};
                // Only validate the type here. Whether or not it's legal to access
                // this builtin is up to the entry point.
                let width = 4;
                let ty_inner = match built_in {
                    Bi::BaseInstance
                    | Bi::BaseVertex
                    | Bi::InstanceIndex
                    | Bi::VertexIndex
                    | Bi::SampleIndex
                    | Bi::SampleMaskIn
                    | Bi::SampleMaskOut
                    | Bi::LocalInvocationIndex => Ti::Scalar {
                        kind: Sk::Uint,
                        width,
                    },
                    Bi::ClipDistance | Bi::PointSize | Bi::FragDepth => Ti::Scalar {
                        kind: Sk::Float,
                        width,
                    },
                    Bi::Position | Bi::FragCoord => Ti::Vector {
                        size: Vs::Quad,
                        kind: Sk::Float,
                        width,
                    },
                    Bi::FrontFacing => Ti::Scalar {
                        kind: Sk::Bool,
                        width,
                    },
                    Bi::GlobalInvocationId
                    | Bi::LocalInvocationId
                    | Bi::WorkGroupId
                    | Bi::WorkGroupSize => Ti::Vector {
                        size: Vs::Tri,
                        kind: Sk::Uint,
                        width,
                    },
                };
                if types[self.ty].inner != ty_inner {
                    log::warn!("Wrong builtin type: {:?}", types[self.ty]);
                    return Err(GlobalVariableError::InvalidBuiltInType(built_in));
                }
                self.forbid_interpolation()?
            }
            Some(crate::Binding::Location(_)) => match types[self.ty].inner {
                crate::TypeInner::Scalar { .. }
                | crate::TypeInner::Vector { .. }
                | crate::TypeInner::Matrix { .. } => {}
                _ => return Err(GlobalVariableError::InvalidType),
            },
            Some(crate::Binding::Resource { .. }) => {
                return Err(GlobalVariableError::InvalidBinding)
            }
            None => {
                match types[self.ty].inner {
                    //TODO: check the member types
                    crate::TypeInner::Struct {
                        block: _,
                        members: _,
                    } => self.forbid_interpolation()?,
                    _ => return Err(GlobalVariableError::InvalidType),
                }
            }
        }
        Ok(())
    }
}

impl crate::TypeInner {
    fn check_block(&self) -> Result<(), GlobalVariableError> {
        match *self {
            Self::Struct {
                block: true,
                members: _,
            } => Ok(()),
            Self::Struct {
                block: false,
                members: _,
            } => Err(GlobalVariableError::MissingBlockDecoration),
            _ => Err(GlobalVariableError::InvalidType),
        }
    }
}

fn storage_usage(access: crate::StorageAccess) -> crate::GlobalUse {
    let mut storage_usage = crate::GlobalUse::empty();
    if access.contains(crate::StorageAccess::LOAD) {
        storage_usage |= crate::GlobalUse::READ;
    }
    if access.contains(crate::StorageAccess::STORE) {
        storage_usage |= crate::GlobalUse::WRITE;
    }
    storage_usage
}

fn built_in_usage(built_in: crate::BuiltIn) -> (crate::ShaderStage, crate::GlobalUse) {
    use crate::{BuiltIn as Bi, GlobalUse as Gu, ShaderStage as Ss};
    match built_in {
        Bi::BaseInstance => (Ss::Vertex, Gu::READ),
        Bi::BaseVertex => (Ss::Vertex, Gu::READ),
        Bi::ClipDistance => (Ss::Vertex, Gu::WRITE),
        Bi::InstanceIndex => (Ss::Vertex, Gu::READ),
        Bi::PointSize => (Ss::Vertex, Gu::WRITE),
        Bi::Position => (Ss::Vertex, Gu::WRITE),
        Bi::VertexIndex => (Ss::Vertex, Gu::READ),
        Bi::FragCoord => (Ss::Fragment, Gu::READ),
        Bi::FragDepth => (Ss::Fragment, Gu::READ),
        Bi::FrontFacing => (Ss::Fragment, Gu::READ),
        Bi::SampleIndex => (Ss::Fragment, Gu::READ),
        Bi::SampleMaskIn => (Ss::Fragment, Gu::READ),
        Bi::SampleMaskOut => (Ss::Fragment, Gu::WRITE),
        Bi::GlobalInvocationId => (Ss::Compute, Gu::READ),
        Bi::LocalInvocationId => (Ss::Compute, Gu::READ),
        Bi::LocalInvocationIndex => (Ss::Compute, Gu::READ),
        Bi::WorkGroupId => (Ss::Compute, Gu::READ),
        Bi::WorkGroupSize => (Ss::Compute, Gu::READ),
    }
}

impl Validator {
    /// Construct a new validator instance.
    pub fn new() -> Self {
        Validator {
            typifier: Typifier::new(),
            location_in_mask: BitSet::new(),
            location_out_mask: BitSet::new(),
            bind_group_masks: Vec::new(),
        }
    }

    fn check_width(kind: crate::ScalarKind, width: crate::Bytes) -> bool {
        match kind {
            crate::ScalarKind::Bool => width == 1,
            _ => width == 4,
        }
    }

    fn validate_type(
        &self,
        ty: &crate::Type,
        handle: Handle<crate::Type>,
        constants: &Arena<crate::Constant>,
    ) -> Result<(), TypeError> {
        use crate::TypeInner as Ti;
        match ty.inner {
            Ti::Scalar { kind, width } | Ti::Vector { kind, width, .. } => {
                if !Self::check_width(kind, width) {
                    return Err(TypeError::InvalidWidth(kind, width));
                }
            }
            Ti::Matrix { width, .. } => {
                if !Self::check_width(crate::ScalarKind::Float, width) {
                    return Err(TypeError::InvalidWidth(crate::ScalarKind::Float, width));
                }
            }
            Ti::Pointer { base, class: _ } => {
                if base >= handle {
                    return Err(TypeError::UnresolvedBase(base));
                }
            }
            Ti::Array { base, size, .. } => {
                if base >= handle {
                    return Err(TypeError::UnresolvedBase(base));
                }
                if let crate::ArraySize::Constant(const_handle) = size {
                    match constants.try_get(const_handle) {
                        Some(&crate::Constant {
                            inner:
                                crate::ConstantInner::Scalar {
                                    width: _,
                                    value: crate::ScalarValue::Uint(_),
                                },
                            ..
                        }) => {}
                        _ => {
                            return Err(TypeError::InvalidArraySizeConstant(const_handle));
                        }
                    }
                }
            }
            Ti::Struct {
                block: _,
                ref members,
            } => {
                //TODO: check the offsets
                for member in members {
                    if member.ty >= handle {
                        return Err(TypeError::UnresolvedBase(member.ty));
                    }
                }
            }
            Ti::Image { .. } => {}
            Ti::Sampler { comparison: _ } => {}
        }
        Ok(())
    }

    fn validate_constant(
        &self,
        handle: Handle<crate::Constant>,
        constants: &Arena<crate::Constant>,
        types: &Arena<crate::Type>,
    ) -> Result<(), ConstantError> {
        let con = &constants[handle];
        match con.inner {
            crate::ConstantInner::Scalar { width, ref value } => {
                if !Self::check_width(value.scalar_kind(), width) {
                    return Err(ConstantError::InvalidType);
                }
            }
            crate::ConstantInner::Composite { ty, ref components } => {
                match types[ty].inner {
                    crate::TypeInner::Array {
                        size: crate::ArraySize::Dynamic,
                        ..
                    } => {
                        return Err(ConstantError::InvalidType);
                    }
                    crate::TypeInner::Array {
                        size: crate::ArraySize::Constant(size_handle),
                        ..
                    } => {
                        if handle <= size_handle {
                            return Err(ConstantError::UnresolvedSize(size_handle));
                        }
                    }
                    _ => {} //TODO
                }
                if let Some(&comp) = components.iter().find(|&&comp| handle <= comp) {
                    return Err(ConstantError::UnresolvedComponent(comp));
                }
            }
        }
        Ok(())
    }

    fn validate_global_var(
        &self,
        var: &crate::GlobalVariable,
        types: &Arena<crate::Type>,
    ) -> Result<(), GlobalVariableError> {
        log::debug!("var {:?}", var);
        let allowed_storage_access = match var.class {
            crate::StorageClass::Function => return Err(GlobalVariableError::InvalidUsage),
            crate::StorageClass::Input | crate::StorageClass::Output => {
                var.check_varying(types)?;
                crate::StorageAccess::empty()
            }
            crate::StorageClass::Storage => {
                var.check_resource()?;
                let ty = &types[var.ty];
                ty.inner.check_block()?;
                crate::StorageAccess::all()
            }
            crate::StorageClass::Uniform => {
                var.check_resource()?;
                let ty = &types[var.ty];
                ty.inner.check_block()?;
                crate::StorageAccess::empty()
            }
            crate::StorageClass::Handle => {
                var.check_resource()?;
                match types[var.ty].inner {
                    crate::TypeInner::Image {
                        class: crate::ImageClass::Storage(_),
                        ..
                    } => crate::StorageAccess::all(),
                    _ => crate::StorageAccess::empty(),
                }
            }
            crate::StorageClass::Private | crate::StorageClass::WorkGroup => {
                if var.binding.is_some() {
                    return Err(GlobalVariableError::InvalidBinding);
                }
                var.forbid_interpolation()?;
                crate::StorageAccess::empty()
            }
            crate::StorageClass::PushConstant => {
                //TODO
                return Err(GlobalVariableError::InvalidStorageAccess {
                    allowed: crate::StorageAccess::empty(),
                    seen: crate::StorageAccess::empty(),
                });
            }
        };

        if !allowed_storage_access.contains(var.storage_access) {
            return Err(GlobalVariableError::InvalidStorageAccess {
                allowed: allowed_storage_access,
                seen: var.storage_access,
            });
        }

        Ok(())
    }

    fn validate_local_var(
        &self,
        var: &crate::LocalVariable,
        types: &Arena<crate::Type>,
        constants: &Arena<crate::Constant>,
    ) -> Result<(), LocalVariableError> {
        log::debug!("var {:?}", var);
        if let Some(const_handle) = var.init {
            match constants[const_handle].inner {
                crate::ConstantInner::Scalar { width, ref value } => {
                    let ty_inner = crate::TypeInner::Scalar {
                        width,
                        kind: value.scalar_kind(),
                    };
                    if types[var.ty].inner != ty_inner {
                        return Err(LocalVariableError::InitializerType);
                    }
                }
                crate::ConstantInner::Composite { ty, components: _ } => {
                    if ty != var.ty {
                        return Err(LocalVariableError::InitializerType);
                    }
                }
            }
        }
        Ok(())
    }

    fn validate_function(
        &mut self,
        fun: &crate::Function,
        module: &crate::Module,
    ) -> Result<(), FunctionError> {
        let resolve_ctx = ResolveContext {
            constants: &module.constants,
            global_vars: &module.global_variables,
            local_vars: &fun.local_variables,
            functions: &module.functions,
            arguments: &fun.arguments,
        };
        self.typifier
            .resolve_all(&fun.expressions, &module.types, &resolve_ctx)?;

        if fun.global_usage.len() > module.global_variables.len() {
            return Err(FunctionError::InvalidGlobalUsageLength);
        }

        for (var_handle, var) in fun.local_variables.iter() {
            self.validate_local_var(var, &module.types, &module.constants)
                .map_err(|error| FunctionError::LocalVariable {
                    handle: var_handle,
                    name: var.name.clone().unwrap_or_default(),
                    error,
                })?;
        }

        for (index, argument) in fun.arguments.iter().enumerate() {
            match module.types[argument.ty].inner {
                crate::TypeInner::Image { .. } | crate::TypeInner::Sampler { .. } => {
                    return Err(FunctionError::InvalidArgumentType {
                        index,
                        name: argument.name.clone().unwrap_or_default(),
                    })
                }
                _ => (),
            }
        }

        Ok(())
    }

    fn validate_entry_point(
        &mut self,
        ep: &crate::EntryPoint,
        stage: crate::ShaderStage,
        module: &crate::Module,
    ) -> Result<(), EntryPointError> {
        if ep.early_depth_test.is_some() && stage != crate::ShaderStage::Fragment {
            return Err(EntryPointError::UnexpectedEarlyDepthTest);
        }
        if stage == crate::ShaderStage::Compute {
            if ep
                .workgroup_size
                .iter()
                .any(|&s| s == 0 || s > MAX_WORKGROUP_SIZE)
            {
                return Err(EntryPointError::OutOfRangeWorkgroupSize);
            }
        } else if ep.workgroup_size != [0; 3] {
            return Err(EntryPointError::UnexpectedWorkgroupSize);
        }

        self.location_in_mask.clear();
        self.location_out_mask.clear();
        for bg in self.bind_group_masks.iter_mut() {
            bg.clear();
        }

        for ((var_handle, var), &usage) in module
            .global_variables
            .iter()
            .zip(&ep.function.global_usage)
        {
            if usage.is_empty() {
                continue;
            }

            if let Some(crate::Binding::Location(_)) = var.binding {
                match (stage, var.class) {
                    (crate::ShaderStage::Vertex, crate::StorageClass::Output)
                    | (crate::ShaderStage::Fragment, crate::StorageClass::Input) => {
                        match module.types[var.ty].inner.scalar_kind() {
                            Some(crate::ScalarKind::Float) => {}
                            Some(_) if var.interpolation != Some(crate::Interpolation::Flat) => {
                                return Err(EntryPointError::InvalidIntegerInterpolation);
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }

            let allowed_usage = match var.class {
                crate::StorageClass::Function => unreachable!(),
                crate::StorageClass::Input => {
                    match var.binding {
                        Some(crate::Binding::BuiltIn(built_in)) => {
                            let (allowed_stage, allowed_usage) = built_in_usage(built_in);
                            if allowed_stage != stage
                                || !allowed_usage.contains(crate::GlobalUse::READ)
                            {
                                return Err(EntryPointError::InvalidBuiltIn(built_in));
                            }
                        }
                        Some(crate::Binding::Location(loc)) => {
                            if !self.location_in_mask.insert(loc as usize) {
                                return Err(EntryPointError::BindingCollision(var_handle));
                            }
                        }
                        Some(crate::Binding::Resource { .. }) => unreachable!(),
                        None => (),
                    }
                    crate::GlobalUse::READ
                }
                crate::StorageClass::Output => {
                    match var.binding {
                        Some(crate::Binding::BuiltIn(built_in)) => {
                            let (allowed_stage, allowed_usage) = built_in_usage(built_in);
                            if allowed_stage != stage
                                || !allowed_usage.contains(crate::GlobalUse::WRITE)
                            {
                                return Err(EntryPointError::InvalidBuiltIn(built_in));
                            }
                        }
                        Some(crate::Binding::Location(loc)) => {
                            if !self.location_out_mask.insert(loc as usize) {
                                return Err(EntryPointError::BindingCollision(var_handle));
                            }
                        }
                        Some(crate::Binding::Resource { .. }) => unreachable!(),
                        None => (),
                    }
                    crate::GlobalUse::READ | crate::GlobalUse::WRITE
                }
                crate::StorageClass::Uniform => crate::GlobalUse::READ,
                crate::StorageClass::Storage => storage_usage(var.storage_access),
                crate::StorageClass::Handle => match module.types[var.ty].inner {
                    crate::TypeInner::Image {
                        class: crate::ImageClass::Storage(_),
                        ..
                    } => storage_usage(var.storage_access),
                    _ => crate::GlobalUse::READ,
                },
                crate::StorageClass::Private | crate::StorageClass::WorkGroup => {
                    crate::GlobalUse::all()
                }
                crate::StorageClass::PushConstant => crate::GlobalUse::READ,
            };
            if !allowed_usage.contains(usage) {
                log::warn!("\tUsage error for: {:?}", var);
                log::warn!(
                    "\tAllowed usage: {:?}, requested: {:?}",
                    allowed_usage,
                    usage
                );
                return Err(EntryPointError::InvalidGlobalUsage(var_handle, usage));
            }

            if let Some(crate::Binding::Resource { group, binding }) = var.binding {
                while self.bind_group_masks.len() <= group as usize {
                    self.bind_group_masks.push(BitSet::new());
                }
                if !self.bind_group_masks[group as usize].insert(binding as usize) {
                    return Err(EntryPointError::BindingCollision(var_handle));
                }
            }
        }

        if !ep.function.arguments.is_empty() {
            return Err(EntryPointError::UnexpectedArguments);
        }
        if ep.function.return_type.is_some() {
            return Err(EntryPointError::UnexpectedReturnValue);
        }

        self.validate_function(&ep.function, module)?;
        Ok(())
    }

    /// Check the given module to be valid.
    pub fn validate(&mut self, module: &crate::Module) -> Result<(), ValidationError> {
        self.typifier.clear();

        for (handle, ty) in module.types.iter() {
            self.validate_type(ty, handle, &module.constants)
                .map_err(|error| ValidationError::Type {
                    handle,
                    name: ty.name.clone().unwrap_or_default(),
                    error,
                })?;
        }

        for (handle, constant) in module.constants.iter() {
            self.validate_constant(handle, &module.constants, &module.types)
                .map_err(|error| ValidationError::Constant {
                    handle,
                    name: constant.name.clone().unwrap_or_default(),
                    error,
                })?;
        }
        for (var_handle, var) in module.global_variables.iter() {
            self.validate_global_var(var, &module.types)
                .map_err(|error| ValidationError::GlobalVariable {
                    handle: var_handle,
                    name: var.name.clone().unwrap_or_default(),
                    error,
                })?;
        }

        for (fun_handle, fun) in module.functions.iter() {
            self.validate_function(fun, module)
                .map_err(|e| ValidationError::Function(fun_handle, e))?;
        }

        for (&(stage, ref name), entry_point) in module.entry_points.iter() {
            self.validate_entry_point(entry_point, stage, module)
                .map_err(|error| ValidationError::EntryPoint {
                    stage,
                    name: name.to_string(),
                    error,
                })?;
        }

        Ok(())
    }
}
