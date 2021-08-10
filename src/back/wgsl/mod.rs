mod keywords;
mod writer;

use thiserror::Error;

pub use writer::Writer;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    FmtError(#[from] std::fmt::Error),
    #[error("{0}")]
    Custom(String),
    #[error("{0}")]
    Unimplemented(String), // TODO: Error used only during development
    #[error("Unsupported math function: {0:?}")]
    UnsupportedMathFunction(crate::MathFunction),
}

pub fn write_string(
    module: &crate::Module,
    info: &crate::valid::ModuleInfo,
) -> Result<String, Error> {
    let mut w = Writer::new(String::new());
    w.write(module, info)?;
    let output = w.finish();
    Ok(output)
}

impl crate::BinaryOperator {
    fn to_wgsl_atomic_suffix(self) -> &'static str {
        match self {
            Self::Add => "Add",
            Self::And => "And",
            Self::InclusiveOr => "Or",
            Self::ExclusiveOr => "Xor",
            _ => unreachable!(),
        }
    }
}
