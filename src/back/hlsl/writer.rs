use super::Error;
use crate::back::hlsl::keywords::RESERVED;
use crate::proc::{NameKey, Namer};
use crate::FastHashMap;
use std::io::Write;

const INDENT: &str = "    ";

pub struct Writer<W> {
    out: W,
    names: FastHashMap<NameKey, String>,
    namer: Namer,
}

impl<W: Write> Writer<W> {
    pub fn new(out: W) -> Self {
        Writer {
            out,
            names: FastHashMap::default(),
            namer: Namer::default(),
        }
    }

    pub fn write(&mut self, module: &crate::Module) -> Result<(), Error> {
        self.names.clear();
        self.namer.reset(module, RESERVED, &mut self.names);

        for (ep_index, ep) in module.entry_points.iter().enumerate() {
            let fun = &ep.function;
            let fun_name = &self.names[&NameKey::EntryPoint(ep_index as _)];
            let has_arguments = fun.arguments.len() > 0;
            writeln!(self.out)?;

            let return_type_name = match fun.result {
                None => "void",
                _ => "",
            };

            writeln!(
                self.out,
                "{} {}({}",
                return_type_name,
                fun_name,
                if has_arguments == false { ")" } else { "" }
            )?;

            // TODO Support arguments
            self.write_block(&ep.function.body)?;
        }
        Ok(())
    }

    fn write_block(&mut self, statements: &[crate::Statement]) -> Result<(), Error> {
        writeln!(self.out, "{{")?;

        for statement in statements {
            match *statement {
                crate::Statement::Return { value: None } => {
                    writeln!(self.out, "{}return;", INDENT)?;
                }
                _ => {}
            }
        }

        writeln!(self.out, "}}")?;

        Ok(())
    }

    pub fn finish(self) -> W {
        self.out
    }
}
