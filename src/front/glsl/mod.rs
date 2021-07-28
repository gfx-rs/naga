pub use error::ErrorKind;
pub use token::{SourceMetadata, Token};

use crate::{FastHashMap, Module, ShaderStage};

mod lex;

mod ast;
use ast::Program;

mod error;
pub use error::ParseError;
mod constants;
mod functions;
mod offset;
mod parser;
#[cfg(test)]
mod parser_tests;
mod token;
mod types;
mod variables;

#[derive(Default)]
pub struct Options {
    pub entry_points: FastHashMap<String, ShaderStage>,
    pub defines: FastHashMap<String, String>,
    pub strip_unused_linkages: bool,
}

pub fn parse_str(source: &str, options: &Options) -> Result<Module, ParseError> {
    let mut program = Program::new(&options.entry_points, options.strip_unused_linkages);

    let lex = lex::Lexer::new(source, &options.defines);
    let mut parser = parser::Parser::new(&mut program, lex);
    parser.parse()?;

    Ok(program.module)
}
