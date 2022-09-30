use crate::front::wgsl::lower::DeclData;
use crate::{
    Bytes, ImageClass, ImageDimension, ScalarKind, StorageFormat, Type, TypeInner, VectorSize,
};
use std::fmt::{Display, Formatter};

pub struct TypeInnerFormatter<'a> {
    pub ty: &'a TypeInner,
    pub types: &'a crate::UniqueArena<Type>,
    pub constants: &'a crate::Arena<crate::Constant>,
}

impl Display for TypeInnerFormatter<'_> {
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
                    kind: ScalarKind::Float,
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
            TypeInner::ValuePointer {
                size,
                kind,
                width,
                space,
            } => {
                let space = match space {
                    crate::AddressSpace::Function => "function",
                    crate::AddressSpace::Private => "private",
                    crate::AddressSpace::WorkGroup => "workgroup",
                    crate::AddressSpace::Uniform => "uniform",
                    crate::AddressSpace::Storage { .. } => "storage",
                    crate::AddressSpace::Handle => "handle",
                    crate::AddressSpace::PushConstant => "push_constant",
                };

                if let Some(size) = size {
                    write!(
                        f,
                        "ptr<{}, vec{}<{}>>",
                        space,
                        VectorSizeFormatter { size },
                        ScalarFormatter { kind, width }
                    )
                } else {
                    write!(f, "ptr<{}, {}>", space, ScalarFormatter { kind, width })
                }
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
                            crate::ConstantInner::Scalar { value, .. } =>
                                ScalarValueFormatter { value },
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
                            crate::ConstantInner::Scalar { value, .. } =>
                                ScalarValueFormatter { value },
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
    width: Bytes,
}

impl Display for ScalarFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ScalarKind::Sint => write!(f, "i{}", self.width * 8),
            ScalarKind::Uint => write!(f, "u{}", self.width * 8),
            ScalarKind::Float => write!(f, "f{}", self.width * 8),
            ScalarKind::Bool => write!(f, "bool"),
        }
    }
}

struct VectorSizeFormatter {
    size: VectorSize,
}

impl Display for VectorSizeFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.size {
            VectorSize::Bi => write!(f, "2"),
            VectorSize::Tri => write!(f, "3"),
            VectorSize::Quad => write!(f, "4"),
        }
    }
}

impl Display for DeclData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DeclData::Function(_) => "function",
                DeclData::Global(_) => "variable",
                DeclData::Const(_) => "const",
                DeclData::Type(_) => "type",
                DeclData::Assert => "assert",
                DeclData::Override => "override",
                DeclData::EntryPoint => "entry point",
                DeclData::Error => "error",
            }
        )
    }
}

struct ScalarValueFormatter {
    value: crate::ScalarValue,
}

impl Display for ScalarValueFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.value {
            crate::ScalarValue::Sint(i) => write!(f, "{}", i),
            crate::ScalarValue::Uint(u) => write!(f, "{}", u),
            crate::ScalarValue::Float(v) => write!(f, "{}", v),
            crate::ScalarValue::Bool(b) => write!(f, "{}", b),
        }
    }
}
