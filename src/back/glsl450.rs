use crate::{
    Arena, ArraySize, BinaryOperator, Binding, BuiltIn, Constant, ConstantInner, DerivativeAxis,
    Expression, FastHashMap, Function, FunctionOrigin, GlobalVariable, Handle, ImageDimension,
    ImageFlags, IntrinsicFunction, LocalVariable, Module, ScalarKind, Statement, StorageClass,
    Type, TypeInner, UnaryOperator,
};
use std::{
    fmt,
    io::{self, Write},
};

pub fn write(module: &Module, out: &mut impl Write) -> io::Result<()> {
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
            TypeInner::Struct { .. } => {
                let name = namer(ty.name.as_ref());

                structs.insert(handle, name);
            }
            _ => continue,
        }
    }

    // Do a second pass to build the structs
    // TODO: glsl is order dependent so we need to build structs in order
    for (handle, ty) in module.types.iter() {
        match &ty.inner {
            TypeInner::Struct { members } => {
                let name = structs.get(&handle).unwrap();

                writeln!(out, "struct {} {{", name)?;
                for member in members {
                    writeln!(
                        out,
                        "   {} {}",
                        member.ty.writer(&module.types, &structs),
                        namer(member.name.as_ref())
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
                _ => todo!(),
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
            global.class.writer(),
            global.ty.writer(&module.types, &structs),
            name
        )?;

        globals_lookup.insert(handle, name);
    }

    let mut functions = FastHashMap::default();

    // Do a first pass to collect names
    for (handle, func) in module.functions.iter() {
        functions.insert(handle, namer(func.name.as_ref()));
    }

    // TODO: glsl is order dependent so we need to build functions in order
    for (handle, func) in module.functions.iter() {
        let name = functions.get(&handle).unwrap();

        writeln!(
            out,
            "{} {}({}) {{",
            func.return_type.map_or(String::from("void"), |ty| ty
                .writer(&module.types, &structs)
                .to_string(),),
            name,
            func.parameter_types
                .iter()
                .map(|ty| ty.writer(&module.types, &structs).to_string())
                .collect::<Vec<_>>()
                .join(","),
        )?;

        let builder = StatementBuilder {
            functions: &functions,
            globals: &globals_lookup,
            locals: &func
                .local_variables
                .iter()
                .map(|(handle, local)| (handle, namer(local.name.as_ref())))
                .collect(),
            structs: &structs,
            args: &func
                .parameter_types
                .iter()
                .enumerate()
                .map(|(pos, arg)| (pos as u32, namer(None)))
                .collect(),
            expressions: &func.expressions,
            types: &module.types,
        };

        for sta in func.body.iter() {
            write!(out, "{}", sta.writer(module, &builder))?;
        }

        writeln!(out, "}}")?;
    }

    Ok(())
}

struct StatementBuilder<'a> {
    functions: &'a FastHashMap<Handle<Function>, String>,
    globals: &'a FastHashMap<Handle<GlobalVariable>, String>,
    locals: &'a FastHashMap<Handle<LocalVariable>, String>,
    structs: &'a FastHashMap<Handle<Type>, String>,
    args: &'a FastHashMap<u32, String>,
    expressions: &'a Arena<Expression>,
    types: &'a Arena<Type>,
}

struct StatementWriter<'a> {
    inner: &'a Statement,
    module: &'a Module,
    builder: &'a StatementBuilder<'a>,
}

impl Statement {
    fn writer<'a>(
        &'a self,
        module: &'a Module,
        builder: &'a StatementBuilder<'a>,
    ) -> StatementWriter<'a> {
        StatementWriter {
            inner: self,
            module,
            builder,
        }
    }
}

impl<'a> fmt::Display for StatementWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            Statement::Empty => {}
            Statement::Block(block) => {
                for sta in block {
                    writeln!(f, "{}", sta.writer(self.module, self.builder))?;
                }
            }
            Statement::If {
                condition,
                accept,
                reject,
            } => {
                writeln!(
                    f,
                    "if({}) {{",
                    self.builder.expressions[*condition].writer(self.module, self.builder)
                )?;
                for sta in accept {
                    writeln!(f, "{}", sta.writer(self.module, self.builder))?;
                }
                writeln!(f, "}} else {{")?;
                for sta in reject {
                    writeln!(f, "{}", sta.writer(self.module, self.builder))?;
                }
                writeln!(f, "}}")?;
            }
            Statement::Switch {
                selector,
                cases,
                default,
            } => {
                writeln!(
                    f,
                    "switch({}) {{",
                    self.builder.expressions[*selector].writer(self.module, self.builder)
                )?;

                for (label, (block, fallthrough)) in cases {
                    writeln!(f, "   case {}:", label)?;

                    for sta in block {
                        writeln!(f, "      {}", sta.writer(self.module, self.builder))?;
                    }

                    if fallthrough.is_some() {
                        writeln!(f, "      break;")?;
                    }
                }

                writeln!(f, "   default:")?;

                for sta in default {
                    writeln!(f, "      {}", sta.writer(self.module, self.builder))?;
                }

                writeln!(f, "}}")?;
            }
            Statement::Loop { body, continuing } => {}
            Statement::Break => writeln!(f, "break;")?,
            Statement::Continue => writeln!(f, "continue;")?,
            Statement::Return { value } => writeln!(
                f,
                "return  {};",
                value
                    .map(|expr| self.builder.expressions[expr]
                        .writer(self.module, self.builder)
                        .to_string())
                    .unwrap_or(String::from(""))
            )?,
            Statement::Kill => writeln!(f, "discard;")?,
            Statement::Store { pointer, value } => writeln!(
                f,
                "{} = {};",
                self.builder.expressions[*pointer].writer(self.module, self.builder),
                self.builder.expressions[*value].writer(self.module, self.builder)
            )?,
        };

        Ok(())
    }
}

struct ExpressionWriter<'a> {
    inner: &'a Expression,
    module: &'a Module,
    builder: &'a StatementBuilder<'a>,
}

impl Expression {
    fn writer<'a>(
        &'a self,
        module: &'a Module,
        builder: &'a StatementBuilder<'a>,
    ) -> ExpressionWriter<'a> {
        ExpressionWriter {
            inner: self,
            module,
            builder,
        }
    }
}

impl<'a> fmt::Display for ExpressionWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            Expression::Access { base, index } => write!(
                f,
                "{}[{}]",
                self.builder.expressions[*base].writer(self.module, self.builder),
                self.builder.expressions[*index].writer(self.module, self.builder)
            ),
            Expression::AccessIndex { base, index } => todo!(),
            Expression::Constant(constant) => write!(
                f,
                "{}",
                self.module.constants[*constant].writer(self.module)
            ),
            Expression::Compose { ty, components } => write!(
                f,
                "{}({})",
                match self.module.types[*ty].inner {
                    TypeInner::Scalar { kind, width } => String::from(match kind {
                        ScalarKind::Sint => "int",
                        ScalarKind::Uint => "uint",
                        ScalarKind::Float => match width {
                            4 => "float",
                            8 => "double",
                            _ => todo!(),
                        },
                        ScalarKind::Bool => "bool",
                    }),
                    TypeInner::Vector { size, kind, width } => format!(
                        "{}vec{}",
                        match kind {
                            ScalarKind::Sint => "i",
                            ScalarKind::Uint => "u",
                            ScalarKind::Float => match width {
                                4 => "",
                                8 => "d",
                                _ => todo!(),
                            },
                            ScalarKind::Bool => "b",
                        },
                        size as u8,
                    ),
                    TypeInner::Matrix {
                        columns,
                        rows,
                        kind,
                        width,
                    } => format!(
                        "{}mat{}x{}",
                        match kind {
                            ScalarKind::Sint => "i",
                            ScalarKind::Uint => "u",
                            ScalarKind::Float => match width {
                                4 => "",
                                8 => "d",
                                _ => todo!(),
                            },
                            ScalarKind::Bool => "b",
                        },
                        columns as u8,
                        rows as u8,
                    ),
                    TypeInner::Array { .. } => ty
                        .writer(self.builder.types, self.builder.structs)
                        .to_string(),
                    TypeInner::Struct { .. } => self.builder.structs.get(ty).unwrap().clone(),
                    _ => todo!(),
                },
                components
                    .iter()
                    .map(|arg| self.builder.expressions[*arg]
                        .writer(self.module, self.builder)
                        .to_string())
                    .collect::<Vec<_>>()
                    .join(","),
            ),
            Expression::FunctionParameter(pos) => {
                write!(f, "{}", self.builder.args.get(&pos).unwrap())
            }
            Expression::GlobalVariable(handle) => {
                write!(f, "{}", self.builder.globals.get(&handle).unwrap())
            }
            Expression::LocalVariable(handle) => {
                write!(f, "{}", self.builder.locals.get(&handle).unwrap())
            }
            Expression::Load { pointer } => todo!(),
            Expression::ImageSample {
                image,
                sampler,
                coordinate,
                depth_ref,
            } => todo!(),
            Expression::Unary { op, expr } => write!(
                f,
                "({} {})",
                match op {
                    UnaryOperator::Negate => "-",
                    UnaryOperator::Not => "~",
                },
                self.builder.expressions[*expr].writer(self.module, self.builder)
            ),
            Expression::Binary { op, left, right } => write!(
                f,
                "({} {} {})",
                self.builder.expressions[*left].writer(self.module, self.builder),
                match op {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Subtract => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::Divide => "/",
                    BinaryOperator::Modulo => "%",
                    BinaryOperator::Equal => "==",
                    BinaryOperator::NotEqual => "!=",
                    BinaryOperator::Less => "<",
                    BinaryOperator::LessEqual => "<=",
                    BinaryOperator::Greater => ">",
                    BinaryOperator::GreaterEqual => ">=",
                    BinaryOperator::And => "&",
                    BinaryOperator::ExclusiveOr => "^",
                    BinaryOperator::InclusiveOr => "|",
                    BinaryOperator::LogicalAnd => "&&",
                    BinaryOperator::LogicalOr => "||",
                    BinaryOperator::ShiftLeftLogical => "<<",
                    BinaryOperator::ShiftRightLogical => todo!(),
                    BinaryOperator::ShiftRightArithmetic => ">>",
                },
                self.builder.expressions[*right].writer(self.module, self.builder)
            ),
            Expression::Intrinsic { fun, argument } => todo!(),
            Expression::DotProduct(left, right) => write!(
                f,
                "dot({},{})",
                self.builder.expressions[*left].writer(self.module, self.builder),
                self.builder.expressions[*right].writer(self.module, self.builder)
            ),
            Expression::CrossProduct(left, right) => write!(
                f,
                "cross({},{})",
                self.builder.expressions[*left].writer(self.module, self.builder),
                self.builder.expressions[*right].writer(self.module, self.builder)
            ),
            Expression::Derivative { axis, expr } => write!(
                f,
                "{}({})",
                match axis {
                    DerivativeAxis::X => "dFdx",
                    DerivativeAxis::Y => "dFdy",
                    _ => todo!(),
                },
                self.builder.expressions[*expr].writer(self.module, self.builder)
            ),
            Expression::Call { origin, arguments } => write!(
                f,
                "{}({})",
                match origin {
                    FunctionOrigin::External(name) => name,
                    FunctionOrigin::Local(handle) => self.builder.functions.get(&handle).unwrap(),
                },
                arguments
                    .iter()
                    .map(|arg| self.builder.expressions[*arg]
                        .writer(self.module, self.builder)
                        .to_string())
                    .collect::<Vec<_>>()
                    .join(","),
            ),
        }
    }
}

struct ConstantWriter<'a> {
    inner: &'a Constant,
    module: &'a Module,
}

impl Constant {
    fn writer<'a>(&'a self, module: &'a Module) -> ConstantWriter<'a> {
        ConstantWriter {
            inner: self,
            module,
        }
    }
}

impl<'a> fmt::Display for ConstantWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner.inner {
            ConstantInner::Sint(int) => write!(f, "{}", int),
            ConstantInner::Uint(int) => write!(f, "{}", int),
            ConstantInner::Float(float) => write!(f, "{}", float),
            ConstantInner::Bool(boolean) => write!(f, "{}", boolean),
            ConstantInner::Composite(components) => match self.module.types[self.inner.ty].inner {
                _ => todo!(),
            },
        }
    }
}

struct StorageClassWriter<'a> {
    inner: &'a StorageClass,
}

impl StorageClass {
    fn writer<'a>(&'a self) -> StorageClassWriter<'a> {
        StorageClassWriter { inner: self }
    }
}

impl<'a> fmt::Display for StorageClassWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self.inner {
                StorageClass::Constant => "const ",
                StorageClass::Function => todo!(),
                StorageClass::Input => "in ",
                StorageClass::Output => "out ",
                StorageClass::Private => "",
                StorageClass::StorageBuffer => "buffer ",
                StorageClass::Uniform => "uniform ",
                StorageClass::WorkGroup => "shared ",
            }
        )
    }
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

struct TypeWriter<'a> {
    inner: Handle<Type>,
    types: &'a Arena<Type>,
    structs: &'a FastHashMap<Handle<Type>, String>,
}

impl Handle<Type> {
    fn writer<'a>(
        &self,
        types: &'a Arena<Type>,
        structs: &'a FastHashMap<Handle<Type>, String>,
    ) -> TypeWriter<'a> {
        TypeWriter {
            inner: *self,
            types,
            structs,
        }
    }
}

impl<'a> fmt::Display for TypeWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.types[self.inner].inner {
            TypeInner::Scalar { kind, width } => match kind {
                ScalarKind::Sint => write!(f, "int"),
                ScalarKind::Uint => write!(f, "uint"),
                ScalarKind::Float => match width {
                    4 => write!(f, "float"),
                    8 => write!(f, "double"),
                    _ => todo!(),
                },
                ScalarKind::Bool => write!(f, "bool"),
            },
            TypeInner::Vector { size, kind, width } => write!(
                f,
                "{}vec{}",
                match kind {
                    ScalarKind::Sint => "i",
                    ScalarKind::Uint => "u",
                    ScalarKind::Float => match width {
                        4 => "",
                        8 => "d",
                        _ => todo!(),
                    },
                    ScalarKind::Bool => "b",
                },
                *size as u8
            ),
            TypeInner::Matrix {
                columns,
                rows,
                kind,
                width,
            } => write!(
                f,
                "{}mat{}x{}",
                match kind {
                    ScalarKind::Sint => "i",
                    ScalarKind::Uint => "u",
                    ScalarKind::Float => match width {
                        4 => "",
                        8 => "d",
                        _ => todo!(),
                    },
                    ScalarKind::Bool => "b",
                },
                *columns as u8,
                *rows as u8
            ),
            TypeInner::Pointer { base, class } => todo!(),
            TypeInner::Array { base, size, stride } => write!(
                f,
                "{}[{}]",
                base.writer(self.types, self.structs),
                size.writer()
            ),
            TypeInner::Struct { .. } => write!(f, "{}", self.structs.get(&self.inner).unwrap()),
            TypeInner::Image { base, dim, flags } => write!(
                f,
                "{}image{}{}",
                match self.types[*base].inner {
                    TypeInner::Scalar { kind, .. } => match kind {
                        ScalarKind::Sint => "i",
                        ScalarKind::Uint => "u",
                        ScalarKind::Float => "",
                        _ => todo!(),
                    },
                    _ => todo!(),
                },
                dim.writer(),
                flags.writer()
            ),
            TypeInner::DepthImage { dim, arrayed } => todo!(),
            TypeInner::Sampler { comparison } => {
                if *comparison {
                    write!(f, "sampler")
                } else {
                    write!(f, "samplerShadow")
                }
            }
        }
    }
}

struct ArraySizeWriter<'a> {
    inner: &'a ArraySize,
}

impl ArraySize {
    fn writer<'a>(&'a self) -> ArraySizeWriter<'a> {
        ArraySizeWriter { inner: self }
    }
}

impl<'a> fmt::Display for ArraySizeWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            ArraySize::Static(size) => write!(f, "{}", size),
            ArraySize::Dynamic => Ok(()),
        }
    }
}

struct DimWriter<'a> {
    inner: &'a ImageDimension,
}

impl ImageDimension {
    fn writer<'a>(&'a self) -> DimWriter<'a> {
        DimWriter { inner: self }
    }
}

impl<'a> fmt::Display for DimWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self.inner {
                ImageDimension::D1 => "1D",
                ImageDimension::D2 => "2D",
                ImageDimension::D3 => "3D",
                ImageDimension::Cube => "Cube",
            }
        )
    }
}

struct ImageFlagsWriter<'a> {
    inner: &'a ImageFlags,
}

impl ImageFlags {
    fn writer<'a>(&'a self) -> ImageFlagsWriter<'a> {
        ImageFlagsWriter { inner: self }
    }
}

impl<'a> fmt::Display for ImageFlagsWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.inner.contains(ImageFlags::MULTISAMPLED) {
            write!(f, "MS")?;
        }

        if self.inner.contains(ImageFlags::ARRAYED) {
            write!(f, "Array")?;
        }

        Ok(())
    }
}
