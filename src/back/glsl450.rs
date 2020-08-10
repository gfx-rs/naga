use crate::{
    back::glsl_common::{Error, StatementBuilder},
    Binding, BuiltIn, FastHashMap, Module, TypeInner,
};
use std::{
    fmt::{self, Write as FmtWrite},
    io::Write,
};

pub fn write(module: &Module, out: &mut impl Write) -> Result<(), Error> {
    writeln!(out, "#version 450 core")?;

    let mut counter = 0;
    let mut names = FastHashMap::default();

    let mut namer = |name: Option<&String>| {
        if let Some(name) = name {
            names.insert(name.clone(), ());
            name.clone()
        } else {
            counter += 1;
            while names.get(&format!("_{}", counter)).is_some() {
                counter += 1;
            }
            format!("_{}", counter)
        }
    };

    let mut structs = FastHashMap::default();

    // Do a first pass to collect names
    for (handle, ty) in module.types.iter() {
        match &ty.inner {
            TypeInner::Struct { members } => {
                let name = namer(ty.name.as_ref());
                let mut idxs = Vec::new();

                for member in members {
                    idxs.push(namer(member.name.as_ref()))
                }

                structs.insert(handle, (name, idxs));
            }
            _ => continue,
        }
    }

    // Do a second pass to build the structs
    // TODO: glsl is order dependent so we need to build structs in order
    for (handle, ty) in module.types.iter() {
        match &ty.inner {
            TypeInner::Struct { members } => {
                let (name, idxs) = structs.get(&handle).unwrap();

                writeln!(out, "struct {} {{", name)?;
                for (member, name) in members.iter().zip(idxs.iter()) {
                    writeln!(
                        out,
                        "   {} {}",
                        member.ty.write_glsl(&module.types, &structs)?,
                        name
                    )?;
                }
                writeln!(out, "}};")?;
            }
            _ => continue,
        }
    }

    let mut globals_lookup = FastHashMap::default();

    for (handle, global) in module.global_variables.iter() {
        if let Some(Binding::BuiltIn(built_in)) = global.binding {
            match built_in {
                BuiltIn::Position => globals_lookup.insert(handle, String::from("gl_position")),
                BuiltIn::GlobalInvocationId => {
                    globals_lookup.insert(handle, String::from("gl_GlobalInvocationID"))
                }
                BuiltIn::BaseInstance => todo!(),
                BuiltIn::BaseVertex => todo!(),
                BuiltIn::ClipDistance => {
                    globals_lookup.insert(handle, String::from("gl_ClipDistance"))
                }
                BuiltIn::InstanceIndex => {
                    globals_lookup.insert(handle, String::from("gl_InstanceIndex"))
                }
                BuiltIn::VertexIndex => {
                    globals_lookup.insert(handle, String::from("gl_VertexIndex"))
                }
                BuiltIn::PointSize => globals_lookup.insert(handle, String::from("gl_PointSize")),
                BuiltIn::FragCoord => globals_lookup.insert(handle, String::from("gl_FragCoord")),
                BuiltIn::FrontFacing => {
                    globals_lookup.insert(handle, String::from("gl_FrontFacing"))
                }
                BuiltIn::SampleIndex => globals_lookup.insert(handle, String::from("gl_SampleID")),
                BuiltIn::FragDepth => globals_lookup.insert(handle, String::from("gl_FragDepth")),
                BuiltIn::LocalInvocationId => {
                    globals_lookup.insert(handle, String::from("gl_LocalInvocationID"))
                }
                BuiltIn::LocalInvocationIndex => {
                    globals_lookup.insert(handle, String::from("gl_LocalInvocationIndex"))
                }
                BuiltIn::WorkGroupId => {
                    globals_lookup.insert(handle, String::from("gl_WorkGroupID"))
                }
            };
            continue;
        }

        if let Some(ref binding) = global.binding {
            write!(out, "layout({}) ", binding.writer())?;
        }

        let name = namer(global.name.as_ref());

        writeln!(
            out,
            "{}{} {};",
            global.class.write_glsl(),
            global.ty.write_glsl(&module.types, &structs)?,
            name
        )?;

        globals_lookup.insert(handle, name);
    }

    let mut functions = FastHashMap::default();

    // Do a first pass to collect names
    for (handle, func) in module.functions.iter() {
        functions.insert(handle, namer(func.name.as_ref()));
    }

    let mut types = module.types.clone();

    // TODO: glsl is order dependent so we need to build functions in order
    for (handle, func) in module.functions.iter() {
        let name = functions.get(&handle).unwrap();

        writeln!(
            out,
            "{} {}({}) {{",
            func.return_type.map_or(Ok(String::from("void")), |ty| ty
                .write_glsl(&module.types, &structs))?,
            name,
            func.parameter_types
                .iter()
                .map(|ty| ty.write_glsl(&module.types, &structs))
                .collect::<Result<Vec<_>, _>>()?
                .join(","),
        )?;

        let mut builder = StatementBuilder {
            functions: &functions,
            globals: &globals_lookup,
            locals_lookup: &func
                .local_variables
                .iter()
                .map(|(handle, local)| (handle, namer(local.name.as_ref())))
                .collect(),
            structs: &structs,
            args: &func
                .parameter_types
                .iter()
                .enumerate()
                .map(|(pos, _)| (pos as u32, namer(None)))
                .collect(),
            expressions: &func.expressions,
            types: &mut types,
            locals: &func.local_variables,
        };

        for sta in func.body.iter() {
            writeln!(out, "{}", sta.write_glsl(module, &mut builder)?)?;
        }

        writeln!(out, "}}")?;
    }

    Ok(())
}

struct BindingWriter<'a> {
    inner: &'a Binding,
}

impl Binding {
    fn writer<'a>(&'a self) -> BindingWriter<'a> {
        BindingWriter { inner: self }
    }
}

impl<'a> fmt::Display for BindingWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            Binding::BuiltIn(_) => write!(f, ""), // Ignore because they are variables with a predefined name
            Binding::Location(location) => write!(f, "location={}", location),
            Binding::Descriptor { set, binding } => write!(f, "set={},binding={}", set, binding),
        }
    }
}
