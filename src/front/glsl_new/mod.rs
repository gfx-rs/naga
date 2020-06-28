use crate::{Arena, Constant, EntryPoint, Function, GlobalVariable, Header, Module, Type};
use spirv::ExecutionModel;
use std::io::BufReader;

mod lexer;
use lexer::Lexer;
mod error;
use error::{ErrorKind, ParseError};
mod parser;
mod token;

pub fn parse_str(source: &str, entry: String, exec: ExecutionModel) -> Result<Module, ParseError> {
    log::debug!("------ GLSL-pomelo ------");

    let module = Module {
        header: Header {
            version: (1, 0, 0),
            generator: 0,
        },
        types: Arena::<Type>::new(),
        constants: Arena::<Constant>::new(),
        global_variables: Arena::<GlobalVariable>::new(),
        functions: Arena::<Function>::new(),
        entry_points: vec![],
    };

    let buf_rdr = BufReader::new(source.as_bytes());
    let lex = Lexer::new(buf_rdr);
    let mut parser = parser::Parser::new(module);

    for token in lex {
        let token = token?;
        parser.parse(token).map_err(|_| ErrorKind::InvalidInput)?;
    }
    let (_, mut parsed_module) = parser.end_of_input().map_err(|_| ErrorKind::InvalidInput)?;

    // find entry point
    let entry_func = parsed_module
        .functions
        .iter()
        .find(|(_, f)| f.name.as_ref().filter(|n| **n == entry).is_some());
    if let Some((h, _)) = entry_func {
        parsed_module.entry_points.push(EntryPoint {
            exec_model: exec,
            name: entry,
            function: h,
        });
    }

    Ok(parsed_module)
}
