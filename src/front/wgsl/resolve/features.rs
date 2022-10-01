use std::fmt::{Debug, Display};

use rustc_hash::FxHashSet;
use strum::EnumIter;

use crate::front::wgsl::parse::ast::Enable;
use crate::front::wgsl::{
    resolve::inbuilt::{Matcher, ToStaticString},
    text::Interner,
    WgslError,
};
use crate::Span;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, EnumIter)]
pub enum Feature {
    Float16,
    Float64,
    PrimitiveIndex,
    BindingArray,
    PushConstant,
    StorageImageRead,
    Multiview,
    ConservativeDepth,
}

impl ToStaticString for Feature {
    fn to_static_str(&self) -> &'static str {
        match self {
            Feature::Float16 => "f16",
            Feature::Float64 => "f64",
            Feature::PrimitiveIndex => "primitive_index",
            Feature::BindingArray => "binding_array",
            Feature::PushConstant => "push_constant",
            Feature::StorageImageRead => "storage_image_read",
            Feature::Multiview => "multiview",
            Feature::ConservativeDepth => "conservative_depth",
        }
    }
}

impl Display for Feature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

#[derive(Clone)]
pub struct EnabledFeatures {
    features: FxHashSet<Feature>,
    matcher: Matcher<Feature>,
}

impl Debug for EnabledFeatures {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.features.iter()).finish()
    }
}

impl EnabledFeatures {
    pub fn new(intern: &mut Interner) -> Self {
        Self {
            features: FxHashSet::default(),
            matcher: Matcher::new(intern),
        }
    }

    pub fn is_enabled(&self, feature: Feature) -> bool {
        self.features.contains(&feature)
    }

    pub fn enable(&mut self, enable: Enable, intern: &Interner, diagnostics: &mut Vec<WgslError>) {
        if let Some(feature) = self.matcher.get(enable.name.name) {
            self.features.insert(feature);
        } else {
            diagnostics.push(
                WgslError::new(format!(
                    "unknown feature `{}`",
                    intern.resolve(enable.name.name)
                ))
                .marker(enable.name.span),
            );
        }
    }

    pub fn require(&mut self, feature: Feature, span: Span, diagnostics: &mut Vec<WgslError>) {
        if !self.features.contains(&feature) {
            diagnostics
                .push(WgslError::new(format!("feature `{}` is not enabled", feature)).marker(span));
            self.features.insert(feature); // Only error once.
        }
    }
}
