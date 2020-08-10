use crate::{
    back::glsl_common::{write_type, Error, StatementBuilder},
    Binding, BuiltIn, FastHashMap, Module, TypeInner,
};
use std::{
    fmt::{self},
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
        match ty.inner {
            TypeInner::Struct { ref members } => {
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
        match ty.inner {
            TypeInner::Struct { ref members } => {
                let (name, idxs) = structs.get(&handle).unwrap();

                writeln!(out, "struct {} {{", name)?;
                for (member, name) in members.iter().zip(idxs.iter()) {
                    writeln!(
                        out,
                        "   {} {}",
                        write_type(member.ty, &module.types, &structs)?,
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
            let semantic = match built_in {
                BuiltIn::Position => "gl_position",
                BuiltIn::GlobalInvocationId => "gl_GlobalInvocationID",
                BuiltIn::BaseInstance => "gl_BaseInstance",
                BuiltIn::BaseVertex => "gl_BaseVertex",
                BuiltIn::ClipDistance => "gl_ClipDistance",
                BuiltIn::InstanceIndex => "gl_InstanceIndex",
                BuiltIn::VertexIndex => "gl_VertexIndex",
                BuiltIn::PointSize => "gl_PointSize",
                BuiltIn::FragCoord => "gl_FragCoord",
                BuiltIn::FrontFacing => "gl_FrontFacing",
                BuiltIn::SampleIndex => "gl_SampleID",
                BuiltIn::FragDepth => "gl_FragDepth",
                BuiltIn::LocalInvocationId => "gl_LocalInvocationID",
                BuiltIn::LocalInvocationIndex => "gl_LocalInvocationIndex",
                BuiltIn::WorkGroupId => "gl_WorkGroupID",
            };

            globals_lookup.insert(handle, String::from(semantic));
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
            write_type(global.ty, &module.types, &structs)?,
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
    let mut typifier = crate::proc::Typifier::new();

    // TODO: glsl is order dependent so we need to build functions in order
    for (handle, func) in module.functions.iter() {
        let name = functions.get(&handle).unwrap();

        writeln!(
            out,
            "{} {}({}) {{",
            func.return_type
                .map_or(Ok(String::from("void")), |ty| write_type(
                    ty, &types, &structs
                ))?,
            name,
            func.parameter_types
                .iter()
                .map(|ty| write_type(*ty, &types, &structs))
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
            typifier: &mut typifier,
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
