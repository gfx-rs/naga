use std::fmt::Display;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use rustc_hash::FxHashMap;
use strum::{EnumIter, IntoEnumIterator};

use crate::{
    front::wgsl::ast::Expr,
    front::wgsl::text::{Interner, Text},
    BuiltIn, Bytes, ImageDimension, Interpolation, Sampling, ScalarKind, Span, StorageFormat,
    VectorSize,
};

pub trait ToStaticString {
    fn to_static_str(&self) -> &'static str;
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum AccessMode {
    Read,
    Write,
    ReadWrite,
}

impl ToStaticString for AccessMode {
    fn to_static_str(&self) -> &'static str {
        match self {
            AccessMode::Read => "read",
            AccessMode::Write => "write",
            AccessMode::ReadWrite => "read_write",
        }
    }
}

impl Display for AccessMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<crate::StorageAccess> for AccessMode {
    fn into(self) -> crate::StorageAccess {
        match self {
            AccessMode::Read => crate::StorageAccess::LOAD,
            AccessMode::Write => crate::StorageAccess::STORE,
            AccessMode::ReadWrite => crate::StorageAccess::LOAD | crate::StorageAccess::STORE,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum AddressSpace {
    Function,
    Private,
    Storage,
    Uniform,
    Workgroup,
    Handle,
    PushConstant,
}

impl ToStaticString for AddressSpace {
    fn to_static_str(&self) -> &'static str {
        match self {
            AddressSpace::Function => "function",
            AddressSpace::Private => "private",
            AddressSpace::Storage => "storage",
            AddressSpace::Uniform => "uniform",
            AddressSpace::Workgroup => "workgroup",
            AddressSpace::Handle => "handle",
            AddressSpace::PushConstant => "push_constant",
        }
    }
}

impl Display for AddressSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<crate::AddressSpace> for AddressSpace {
    fn into(self) -> crate::AddressSpace {
        match self {
            AddressSpace::Function => crate::AddressSpace::Function,
            AddressSpace::Private => crate::AddressSpace::Private,
            AddressSpace::Storage => crate::AddressSpace::Storage {
                access: crate::StorageAccess::LOAD,
            },
            AddressSpace::Uniform => crate::AddressSpace::Uniform,
            AddressSpace::Workgroup => crate::AddressSpace::WorkGroup,
            AddressSpace::Handle => crate::AddressSpace::Handle,
            AddressSpace::PushConstant => crate::AddressSpace::PushConstant,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Attribute {
    pub ty: AttributeType,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum AttributeType {
    Align(Expr),
    Binding(Expr),
    Builtin(Builtin),
    Compute,
    Const,
    Fragment,
    Group(Expr),
    Id(Expr),
    Interpolate(InterpolationType, Option<InterpolationSample>),
    Invariant,
    Location(Expr),
    Size(Expr),
    Vertex,
    WorkgroupSize(Expr, Option<Expr>, Option<Expr>),
    ConservativeDepth(Option<ConservativeDepth>),
}

impl Display for AttributeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AttributeType::Align(_) => write!(f, "align"),
            AttributeType::Binding(_) => write!(f, "binding"),
            AttributeType::Builtin(_) => write!(f, "builtin"),
            AttributeType::Compute => write!(f, "compute"),
            AttributeType::Const => write!(f, "const"),
            AttributeType::Fragment => write!(f, "fragment"),
            AttributeType::Group(_) => write!(f, "group"),
            AttributeType::Id(_) => write!(f, "id"),
            AttributeType::Interpolate(..) => write!(f, "interpolate"),
            AttributeType::Invariant => write!(f, "invariant"),
            AttributeType::Location(_) => write!(f, "location"),
            AttributeType::Size(_) => write!(f, "size"),
            AttributeType::Vertex => write!(f, "vertex"),
            AttributeType::WorkgroupSize(..) => write!(f, "workgroup_size"),
            AttributeType::ConservativeDepth(_) => write!(f, "early_depth_test"),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum Builtin {
    FragDepth,
    FrontFacing,
    GlobalInvocationId,
    InstanceIndex,
    LocalInvocationId,
    LocalInvocationIndex,
    NumWorkgroups,
    Position,
    SampleIndex,
    SampleMask,
    VertexIndex,
    WorkgroupId,
    PrimitiveIndex,
    ViewIndex,
}

impl ToStaticString for Builtin {
    fn to_static_str(&self) -> &'static str {
        match self {
            Builtin::FragDepth => "frag_depth",
            Builtin::FrontFacing => "front_facing",
            Builtin::GlobalInvocationId => "global_invocation_id",
            Builtin::InstanceIndex => "instance_index",
            Builtin::LocalInvocationId => "local_invocation_id",
            Builtin::LocalInvocationIndex => "local_invocation_index",
            Builtin::NumWorkgroups => "num_workgroups",
            Builtin::Position => "position",
            Builtin::SampleIndex => "sample_index",
            Builtin::SampleMask => "sample_mask",
            Builtin::VertexIndex => "vertex_index",
            Builtin::WorkgroupId => "workgroup_id",
            Builtin::PrimitiveIndex => "primitive_index",
            Builtin::ViewIndex => "view_index",
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<BuiltIn> for Builtin {
    fn into(self) -> BuiltIn {
        match self {
            Builtin::FragDepth => BuiltIn::FragDepth,
            Builtin::FrontFacing => BuiltIn::FrontFacing,
            Builtin::GlobalInvocationId => BuiltIn::GlobalInvocationId,
            Builtin::InstanceIndex => BuiltIn::InstanceIndex,
            Builtin::LocalInvocationId => BuiltIn::LocalInvocationId,
            Builtin::LocalInvocationIndex => BuiltIn::LocalInvocationIndex,
            Builtin::NumWorkgroups => BuiltIn::NumWorkGroups,
            Builtin::Position => BuiltIn::Position { invariant: false },
            Builtin::SampleIndex => BuiltIn::SampleIndex,
            Builtin::SampleMask => BuiltIn::SampleMask,
            Builtin::VertexIndex => BuiltIn::VertexIndex,
            Builtin::WorkgroupId => BuiltIn::WorkGroupId,
            Builtin::PrimitiveIndex => BuiltIn::PrimitiveIndex,
            Builtin::ViewIndex => BuiltIn::ViewIndex,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum InterpolationSample {
    Center,
    Centroid,
    Sample,
}

impl ToStaticString for InterpolationSample {
    fn to_static_str(&self) -> &'static str {
        match self {
            InterpolationSample::Center => "center",
            InterpolationSample::Centroid => "centroid",
            InterpolationSample::Sample => "sample",
        }
    }
}

impl Display for InterpolationSample {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<Sampling> for InterpolationSample {
    fn into(self) -> Sampling {
        match self {
            InterpolationSample::Center => Sampling::Center,
            InterpolationSample::Centroid => Sampling::Centroid,
            InterpolationSample::Sample => Sampling::Sample,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum InterpolationType {
    Flat,
    Linear,
    Perspective,
}

impl ToStaticString for InterpolationType {
    fn to_static_str(&self) -> &'static str {
        match self {
            InterpolationType::Flat => "flat",
            InterpolationType::Linear => "linear",
            InterpolationType::Perspective => "perspective",
        }
    }
}

impl Display for InterpolationType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<Interpolation> for InterpolationType {
    fn into(self) -> Interpolation {
        match self {
            InterpolationType::Flat => Interpolation::Flat,
            InterpolationType::Linear => Interpolation::Linear,
            InterpolationType::Perspective => Interpolation::Perspective,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum PrimitiveType {
    I32,
    U32,
    F64,
    F32,
    F16,
    Bool,
}

impl ToStaticString for PrimitiveType {
    fn to_static_str(&self) -> &'static str {
        match self {
            PrimitiveType::I32 => "i32",
            PrimitiveType::U32 => "u32",
            PrimitiveType::F64 => "f64",
            PrimitiveType::F32 => "f32",
            PrimitiveType::F16 => "f16",
            PrimitiveType::Bool => "bool",
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<(ScalarKind, Bytes)> for PrimitiveType {
    fn into(self) -> (ScalarKind, Bytes) {
        match self {
            PrimitiveType::I32 => (ScalarKind::Sint, 4),
            PrimitiveType::U32 => (ScalarKind::Uint, 4),
            PrimitiveType::F64 => (ScalarKind::Float, 8),
            PrimitiveType::F32 => (ScalarKind::Float, 4),
            PrimitiveType::F16 => (ScalarKind::Float, 2),
            PrimitiveType::Bool => (ScalarKind::Bool, 1),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum VecType {
    Vec2,
    Vec3,
    Vec4,
}

impl ToStaticString for VecType {
    fn to_static_str(&self) -> &'static str {
        match self {
            VecType::Vec2 => "vec2",
            VecType::Vec3 => "vec3",
            VecType::Vec4 => "vec4",
        }
    }
}

impl Display for VecType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<VectorSize> for VecType {
    fn into(self) -> VectorSize {
        match self {
            VecType::Vec2 => VectorSize::Bi,
            VecType::Vec3 => VectorSize::Tri,
            VecType::Vec4 => VectorSize::Quad,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum MatType {
    Mat2x2,
    Mat2x3,
    Mat2x4,
    Mat3x2,
    Mat3x3,
    Mat3x4,
    Mat4x2,
    Mat4x3,
    Mat4x4,
}

impl ToStaticString for MatType {
    fn to_static_str(&self) -> &'static str {
        match self {
            MatType::Mat2x2 => "mat2x2",
            MatType::Mat2x3 => "mat2x3",
            MatType::Mat2x4 => "mat2x4",
            MatType::Mat3x2 => "mat3x2",
            MatType::Mat3x3 => "mat3x3",
            MatType::Mat3x4 => "mat3x4",
            MatType::Mat4x2 => "mat4x2",
            MatType::Mat4x3 => "mat4x3",
            MatType::Mat4x4 => "mat4x4",
        }
    }
}

impl Display for MatType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<(VectorSize, VectorSize)> for MatType {
    fn into(self) -> (VectorSize, VectorSize) {
        match self {
            MatType::Mat2x2 => (VectorSize::Bi, VectorSize::Bi),
            MatType::Mat2x3 => (VectorSize::Bi, VectorSize::Tri),
            MatType::Mat2x4 => (VectorSize::Bi, VectorSize::Quad),
            MatType::Mat3x2 => (VectorSize::Tri, VectorSize::Bi),
            MatType::Mat3x3 => (VectorSize::Tri, VectorSize::Tri),
            MatType::Mat3x4 => (VectorSize::Tri, VectorSize::Quad),
            MatType::Mat4x2 => (VectorSize::Quad, VectorSize::Bi),
            MatType::Mat4x3 => (VectorSize::Quad, VectorSize::Tri),
            MatType::Mat4x4 => (VectorSize::Quad, VectorSize::Quad),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum SampledTextureType {
    Texture1d,
    Texture1dArray,
    Texture2d,
    TextureMultisampled2d,
    Texture2dArray,
    Texture3d,
    TextureCube,
    TextureCubeArray,
}

impl ToStaticString for SampledTextureType {
    fn to_static_str(&self) -> &'static str {
        match self {
            SampledTextureType::Texture1d => "texture_1d",
            SampledTextureType::Texture1dArray => "texture_1d_array",
            SampledTextureType::Texture2d => "texture_2d",
            SampledTextureType::TextureMultisampled2d => "texture_multisampled_2d",
            SampledTextureType::Texture2dArray => "texture_2d_array",
            SampledTextureType::Texture3d => "texture_3d",
            SampledTextureType::TextureCube => "texture_cube",
            SampledTextureType::TextureCubeArray => "texture_cube_array",
        }
    }
}

impl Display for SampledTextureType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<(ImageDimension, bool, bool)> for SampledTextureType {
    fn into(self) -> (ImageDimension, bool, bool) {
        match self {
            SampledTextureType::Texture1d => (ImageDimension::D1, false, false),
            SampledTextureType::Texture1dArray => (ImageDimension::D1, true, false),
            SampledTextureType::Texture2d => (ImageDimension::D2, false, false),
            SampledTextureType::TextureMultisampled2d => (ImageDimension::D2, false, true),
            SampledTextureType::Texture2dArray => (ImageDimension::D2, true, false),
            SampledTextureType::Texture3d => (ImageDimension::D3, false, false),
            SampledTextureType::TextureCube => (ImageDimension::Cube, false, false),
            SampledTextureType::TextureCubeArray => (ImageDimension::Cube, true, false),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum DepthTextureType {
    Depth2d,
    Depth2dArray,
    DepthCube,
    DepthCubeArray,
    DepthMultisampled2d,
}

impl ToStaticString for DepthTextureType {
    fn to_static_str(&self) -> &'static str {
        match self {
            DepthTextureType::Depth2d => "texture_depth_2d",
            DepthTextureType::Depth2dArray => "texture_depth_2d_array",
            DepthTextureType::DepthCube => "texture_depth_cube",
            DepthTextureType::DepthCubeArray => "texture_depth_cube_array",
            DepthTextureType::DepthMultisampled2d => "texture_depth_multisampled_2d",
        }
    }
}

impl Display for DepthTextureType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<(ImageDimension, bool, bool)> for DepthTextureType {
    fn into(self) -> (ImageDimension, bool, bool) {
        match self {
            DepthTextureType::Depth2d => (ImageDimension::D2, false, false),
            DepthTextureType::Depth2dArray => (ImageDimension::D2, true, false),
            DepthTextureType::DepthCube => (ImageDimension::Cube, false, false),
            DepthTextureType::DepthCubeArray => (ImageDimension::Cube, true, false),
            DepthTextureType::DepthMultisampled2d => (ImageDimension::D2, false, true),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum SamplerType {
    Sampler,
    SamplerComparison,
}

impl ToStaticString for SamplerType {
    fn to_static_str(&self) -> &'static str {
        match self {
            SamplerType::Sampler => "sampler",
            SamplerType::SamplerComparison => "sampler_comparison",
        }
    }
}

impl Display for SamplerType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<bool> for SamplerType {
    fn into(self) -> bool {
        match self {
            SamplerType::Sampler => false,
            SamplerType::SamplerComparison => true,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum StorageTextureType {
    Storage1d,
    Storage1dArray,
    Storage2d,
    Storage2dArray,
    Storage3d,
}

impl ToStaticString for StorageTextureType {
    fn to_static_str(&self) -> &'static str {
        match self {
            StorageTextureType::Storage1d => "texture_storage_1d",
            StorageTextureType::Storage1dArray => "texture_storage_1d_array",
            StorageTextureType::Storage2d => "texture_storage_2d",
            StorageTextureType::Storage2dArray => "texture_storage_2d_array",
            StorageTextureType::Storage3d => "texture_storage_3d",
        }
    }
}

impl Display for StorageTextureType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<(ImageDimension, bool)> for StorageTextureType {
    fn into(self) -> (ImageDimension, bool) {
        match self {
            StorageTextureType::Storage1d => (ImageDimension::D1, false),
            StorageTextureType::Storage1dArray => (ImageDimension::D1, true),
            StorageTextureType::Storage2d => (ImageDimension::D2, false),
            StorageTextureType::Storage2dArray => (ImageDimension::D2, true),
            StorageTextureType::Storage3d => (ImageDimension::D3, false),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum TexelFormat {
    R32Float,
    R32Sint,
    R32Uint,
    Rg32Float,
    Rg32Sint,
    Rg32Uint,
    Rgba16Float,
    Rgba16Sint,
    Rgba16Uint,
    Rgba32Float,
    Rgba32Sint,
    Rgba32Uint,
    Rgba8Sint,
    Rgba8Uint,
    Rgba8Unorm,
    Rgba8Snorm,
}

impl ToStaticString for TexelFormat {
    fn to_static_str(&self) -> &'static str {
        match self {
            TexelFormat::R32Float => "r32float",
            TexelFormat::R32Sint => "r32sint",
            TexelFormat::R32Uint => "r32uint",
            TexelFormat::Rg32Float => "rg32float",
            TexelFormat::Rg32Sint => "rg32sint",
            TexelFormat::Rg32Uint => "rg32uint",
            TexelFormat::Rgba16Float => "rgba16float",
            TexelFormat::Rgba16Sint => "rgba16sint",
            TexelFormat::Rgba16Uint => "rgba16uint",
            TexelFormat::Rgba32Float => "rgba32float",
            TexelFormat::Rgba32Sint => "rgba32sint",
            TexelFormat::Rgba32Uint => "rgba32uint",
            TexelFormat::Rgba8Sint => "rgba8sint",
            TexelFormat::Rgba8Uint => "rgba8uint",
            TexelFormat::Rgba8Unorm => "rgba8unorm",
            TexelFormat::Rgba8Snorm => "rgba8snorm",
        }
    }
}

impl Display for TexelFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<StorageFormat> for TexelFormat {
    fn into(self) -> StorageFormat {
        match self {
            TexelFormat::R32Float => StorageFormat::R32Float,
            TexelFormat::R32Sint => StorageFormat::R32Sint,
            TexelFormat::R32Uint => StorageFormat::R32Uint,
            TexelFormat::Rg32Float => StorageFormat::Rg32Float,
            TexelFormat::Rg32Sint => StorageFormat::Rg32Sint,
            TexelFormat::Rg32Uint => StorageFormat::Rg32Uint,
            TexelFormat::Rgba16Float => StorageFormat::Rgba16Float,
            TexelFormat::Rgba16Sint => StorageFormat::Rgba16Sint,
            TexelFormat::Rgba16Uint => StorageFormat::Rgba16Uint,
            TexelFormat::Rgba32Float => StorageFormat::Rgba32Float,
            TexelFormat::Rgba32Sint => StorageFormat::Rgba32Sint,
            TexelFormat::Rgba32Uint => StorageFormat::Rgba32Uint,
            TexelFormat::Rgba8Sint => StorageFormat::Rgba8Sint,
            TexelFormat::Rgba8Uint => StorageFormat::Rgba8Uint,
            TexelFormat::Rgba8Unorm => StorageFormat::Rgba8Unorm,
            TexelFormat::Rgba8Snorm => StorageFormat::Rgba8Snorm,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, EnumIter)]
pub enum ConservativeDepth {
    GreaterEqual,
    LessEqual,
    Unchanged,
}

impl ToStaticString for ConservativeDepth {
    fn to_static_str(&self) -> &'static str {
        match self {
            ConservativeDepth::GreaterEqual => "greater_equal",
            ConservativeDepth::LessEqual => "less_equal",
            ConservativeDepth::Unchanged => "unchanged",
        }
    }
}

impl Display for ConservativeDepth {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

impl Into<crate::ConservativeDepth> for ConservativeDepth {
    fn into(self) -> crate::ConservativeDepth {
        match self {
            ConservativeDepth::GreaterEqual => crate::ConservativeDepth::GreaterEqual,
            ConservativeDepth::LessEqual => crate::ConservativeDepth::LessEqual,
            ConservativeDepth::Unchanged => crate::ConservativeDepth::Unchanged,
        }
    }
}

#[derive(Clone)]
pub struct Matcher<T> {
    map: FxHashMap<Text, T>,
}

impl<T: ToStaticString + IntoEnumIterator + Copy> Matcher<T> {
    pub fn new(intern: &mut Interner) -> Self {
        let mut map = FxHashMap::default();

        for variant in T::iter() {
            map.insert(intern.get_static(variant.to_static_str()), variant);
        }

        Self { map }
    }

    pub fn get(&self, text: Text) -> Option<T> {
        self.map.get(&text).copied()
    }
}

pub fn reserved_matcher() -> AhoCorasick {
    AhoCorasickBuilder::new()
        .anchored(true)
        .build(crate::keywords::wgsl::RESERVED)
}
