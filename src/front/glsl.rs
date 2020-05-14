use crate::{
    Arena, ArraySize, BinaryOperator, Binding, Constant, ConstantInner, EntryPoint, Expression,
    FastHashMap, Function, GlobalVariable, Handle, Header, LocalVariable, Module, ScalarKind, Type,
    TypeInner, VectorSize,
};
use glsl::{
    parser::{Parse, ParseError},
    syntax::*,
};
use spirv::{BuiltIn, ExecutionModel, StorageClass};

mod helpers;

struct Parser<'a> {
    source: &'a str,
    types: Arena<Type>,
    globals: Arena<GlobalVariable>,
    globals_lookup: FastHashMap<String, Handle<GlobalVariable>>,
    constants: Arena<Constant>,
    functions: Arena<Function>,
    function_lookup: FastHashMap<String, Handle<Function>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Result<Self, ParseError> {
        Ok(Self {
            source,
            types: Arena::new(),
            globals: Arena::new(),
            globals_lookup: FastHashMap::default(),
            constants: Arena::new(),
            functions: Arena::new(),
            function_lookup: FastHashMap::default(),
        })
    }

    pub fn parse(
        mut self,
        entry: String,
        exec: ExecutionModel,
    ) -> Result<crate::Module, ParseError> {
        let ast = TranslationUnit::parse(self.source)?;

        //println!("{:#?}", ast);

        let mut entry_point = None;

        for declaration in ast {
            match declaration {
                ExternalDeclaration::Preprocessor(_) => { /* TODO */ }
                ExternalDeclaration::FunctionDefinition(function) => {
                    let function = self.parse_function_definition(function);

                    if *self.functions[function].name.as_ref().unwrap() == entry {
                        assert!(entry_point.is_none());

                        entry_point = Some(function);
                    }
                }
                ExternalDeclaration::Declaration(decl) => match decl {
                    Declaration::InitDeclaratorList(init) => {
                        let handle = self.parse_global(init);
                        let name = self.globals[handle].name.clone().unwrap();
                        self.globals_lookup.insert(name, handle);
                    }
                    Declaration::Block(block) => unimplemented!(),
                    _ => unimplemented!(),
                },
            }
        }

        Ok(Module {
            header: Header {
                version: (1, 0, 0),
                generator: 0,
            },
            types: self.types,
            constants: self.constants,
            global_variables: self.globals,
            functions: self.functions,
            entry_points: vec![EntryPoint {
                exec_model: exec,
                function: entry_point.unwrap(),
                name: entry,
            }],
        })
    }

    fn parse_function_definition(&mut self, function: FunctionDefinition) -> Handle<Function> {
        let name = function.prototype.name.0;

        // Parse return type
        let ty = self.parse_type(function.prototype.ty.ty);

        let mut parameter_types = Vec::with_capacity(function.prototype.parameters.len());
        let mut parameter_lookup = FastHashMap::default();

        let mut local_variables = Arena::<LocalVariable>::new();
        let mut locals_map = FastHashMap::default();
        let mut expressions = Arena::<Expression>::new();
        let mut body = Vec::new();

        // TODO: Parse Qualifiers
        for (index, parameter) in function.prototype.parameters.into_iter().enumerate() {
            match parameter {
                FunctionParameterDeclaration::Named(_ /* TODO */, decl) => {
                    let ty = self.parse_type(decl.ty).unwrap();

                    let ty = if let Some(specifier) = decl.ident.array_spec {
                        match specifier {
                            ArraySpecifier::Unsized => self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Array {
                                    base: ty,
                                    size: ArraySize::Dynamic,
                                },
                            }),
                            ArraySpecifier::ExplicitlySized(_) => unimplemented!(),
                        }
                    } else {
                        ty
                    };

                    parameter_types.push(ty);
                    parameter_lookup.insert(
                        decl.ident.ident.0,
                        Expression::FunctionParameter(index as u32),
                    );
                }
                FunctionParameterDeclaration::Unnamed(_, ty) => {
                    parameter_types.push(self.parse_type(ty).unwrap());
                }
            }
        }

        for statement in function.statement.statement_list {
            match statement {
                Statement::Compound(_) => unimplemented!(),
                Statement::Simple(statement) => match *statement {
                    SimpleStatement::Declaration(_) => unimplemented!(),
                    SimpleStatement::Expression(expr) => {
                        let expr = match expr {
                            Some(expr) => expr,
                            None => continue,
                        };

                        body.push(self.parse_statement(
                            expr,
                            &mut expressions,
                            &mut local_variables,
                            &mut locals_map,
                            &parameter_lookup,
                        ));
                    }
                    SimpleStatement::Selection(_) => unimplemented!(),
                    SimpleStatement::Switch(_) => unimplemented!(),
                    SimpleStatement::CaseLabel(_) => unimplemented!(),
                    SimpleStatement::Iteration(_) => unimplemented!(),
                    SimpleStatement::Jump(op) => body.push(match op {
                        JumpStatement::Continue => crate::Statement::Continue,
                        JumpStatement::Break => crate::Statement::Break,
                        JumpStatement::Return(expr) => crate::Statement::Return {
                            value: expr.map(|expr| {
                                let expr = self.parse_expression(
                                    *expr,
                                    &mut expressions,
                                    &mut local_variables,
                                    &mut locals_map,
                                    &parameter_lookup,
                                );
                                expressions.append(expr)
                            }),
                        },
                        JumpStatement::Discard => crate::Statement::Kill,
                    }),
                },
            }
        }

        let handle = self.functions.append(Function {
            name: Some(name.clone()),
            control: spirv::FunctionControl::NONE,
            parameter_types: parameter_types,
            return_type: ty,
            global_usage: vec![],
            local_variables,
            expressions,
            body,
        });

        self.function_lookup.insert(name, handle);

        handle
    }

    fn parse_statement(
        &mut self,
        expr: Expr,
        expressions: &mut Arena<Expression>,
        locals: &mut Arena<LocalVariable>,
        locals_map: &mut FastHashMap<String, Handle<LocalVariable>>,
        parameter_lookup: &FastHashMap<String, Expression>,
    ) -> crate::Statement {
        match expr {
            Expr::Assignment(reg, op, value) => {
                let pointer = {
                    let pointer = self.parse_expression(
                        *reg,
                        expressions,
                        locals,
                        locals_map,
                        parameter_lookup,
                    );
                    expressions.append(pointer)
                };
                let mut value = self.parse_expression(
                    *value,
                    expressions,
                    locals,
                    locals_map,
                    parameter_lookup,
                );

                match op {
                    AssignmentOp::Equal => {}
                    AssignmentOp::Mult => {
                        value = Expression::Binary {
                            op: BinaryOperator::Multiply,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::Div => {
                        value = Expression::Binary {
                            op: BinaryOperator::Divide,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::Mod => {
                        value = Expression::Binary {
                            op: BinaryOperator::Modulo,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::Add => {
                        value = Expression::Binary {
                            op: BinaryOperator::Add,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::Sub => {
                        value = Expression::Binary {
                            op: BinaryOperator::Subtract,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::LShift => {
                        value = Expression::Binary {
                            op: BinaryOperator::ShiftLeftLogical,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::RShift => {
                        value = Expression::Binary {
                            op: BinaryOperator::ShiftRightArithmetic, /* ??? */
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::And => {
                        value = Expression::Binary {
                            op: BinaryOperator::And,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::Xor => {
                        value = Expression::Binary {
                            op: BinaryOperator::ExclusiveOr,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                    AssignmentOp::Or => {
                        value = Expression::Binary {
                            op: BinaryOperator::InclusiveOr,
                            left: pointer,
                            right: expressions.append(value),
                        };
                    }
                }

                crate::Statement::Store {
                    pointer,
                    value: expressions.append(value),
                }
            }
            Expr::FunCall(_, _) => unimplemented!(),
            Expr::PostInc(_) => unimplemented!(),
            Expr::PostDec(_) => unimplemented!(),
            _ => panic!(),
        }
    }

    fn parse_expression(
        &mut self,
        expr: Expr,
        expressions: &mut Arena<Expression>,
        locals: &mut Arena<LocalVariable>,
        locals_map: &mut FastHashMap<String, Handle<LocalVariable>>,
        parameter_lookup: &FastHashMap<String, Expression>,
    ) -> Expression {
        match expr {
            Expr::Variable(ident) => {
                let name = ident.0;

                match name.as_str() {
                    "gl_VertexIndex" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::VertexIndex)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Sint,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_InstanceIndex" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::InstanceIndex)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Sint,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_DrawID" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::DrawIndex)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Sint,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_BaseVertex" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::BaseVertex)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Sint,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_BaseInstance" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::BaseInstance)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Sint,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_Position" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::Position)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Vector {
                                    size: VectorSize::Quad,
                                    kind: ScalarKind::Float,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_PointSize" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::PointSize)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Float,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_ClipDistance" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::ClipDistance)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Float,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    "gl_CullDistance" => {
                        Expression::GlobalVariable(self.globals.fetch_or_append(GlobalVariable {
                            name: Some(name),
                            class: StorageClass::Input,
                            binding: Some(Binding::BuiltIn(BuiltIn::CullDistance)),
                            ty: self.types.fetch_or_append(Type {
                                name: None,
                                inner: TypeInner::Scalar {
                                    kind: ScalarKind::Float,
                                    width: 32,
                                },
                            }),
                        }))
                    }
                    other => {
                        if let Some(global) = self.globals_lookup.get(other) {
                            Expression::GlobalVariable(*global)
                        } else if let Some(expr) = parameter_lookup.get(other) {
                            expr.clone()
                        } else if let Some(local) = locals_map.get(other) {
                            Expression::LocalVariable(*local)
                        } else {
                            println!("{}", other);
                            panic!()
                        }
                    }
                }
            }
            Expr::IntConst(value) => {
                Expression::Constant(self.constants.fetch_or_append(Constant {
                    name: None,
                    specialization: None,
                    inner: ConstantInner::Sint(value as i64),
                    ty: self.types.fetch_or_append(Type {
                        name: None,
                        inner: TypeInner::Scalar {
                            kind: ScalarKind::Sint,
                            width: 32,
                        },
                    }),
                }))
            }
            Expr::UIntConst(value) => {
                Expression::Constant(self.constants.fetch_or_append(Constant {
                    name: None,
                    specialization: None,
                    inner: ConstantInner::Uint(value as u64),
                    ty: self.types.fetch_or_append(Type {
                        name: None,
                        inner: TypeInner::Scalar {
                            kind: ScalarKind::Uint,
                            width: 32,
                        },
                    }),
                }))
            }
            Expr::BoolConst(value) => {
                Expression::Constant(self.constants.fetch_or_append(Constant {
                    name: None,
                    specialization: None,
                    inner: ConstantInner::Bool(value),
                    ty: self.types.fetch_or_append(Type {
                        name: None,
                        inner: TypeInner::Scalar {
                            kind: ScalarKind::Bool,
                            width: 1,
                        },
                    }),
                }))
            }
            Expr::FloatConst(value) => {
                Expression::Constant(self.constants.fetch_or_append(Constant {
                    name: None,
                    specialization: None,
                    inner: ConstantInner::Float(value as f64),
                    ty: self.types.fetch_or_append(Type {
                        name: None,
                        inner: TypeInner::Scalar {
                            kind: ScalarKind::Float,
                            width: 32,
                        },
                    }),
                }))
            }
            Expr::DoubleConst(value) => {
                Expression::Constant(self.constants.fetch_or_append(Constant {
                    name: None,
                    specialization: None,
                    inner: ConstantInner::Float(value),
                    ty: self.types.fetch_or_append(Type {
                        name: None,
                        inner: TypeInner::Scalar {
                            kind: ScalarKind::Float,
                            width: 64,
                        },
                    }),
                }))
            }
            Expr::Unary(op, reg) => unimplemented!(),
            Expr::Binary(op, left, right) => {
                let left =
                    self.parse_expression(*left, expressions, locals, locals_map, parameter_lookup);
                let right = self.parse_expression(
                    *right,
                    expressions,
                    locals,
                    locals_map,
                    parameter_lookup,
                );

                Expression::Binary {
                    op: helpers::glsl_to_spirv_binary_op(op),
                    left: expressions.append(left),
                    right: expressions.append(right),
                }
            }
            Expr::Ternary(condition, accept, reject) => unimplemented!(),
            Expr::Assignment(_, _, _) => panic!(),
            Expr::Bracket(reg, index) => unimplemented!(),
            Expr::FunCall(ident, args) => Expression::Call {
                name: match ident {
                    FunIdentifier::Identifier(ident) => ident.0,
                    FunIdentifier::Expr(expr) => todo!(),
                },
                arguments: args
                    .into_iter()
                    .map(|arg| {
                        let expr = self.parse_expression(
                            arg,
                            expressions,
                            locals,
                            locals_map,
                            parameter_lookup,
                        );
                        expressions.append(expr)
                    })
                    .collect(),
            },
            Expr::Dot(reg, ident) => {
                let handle = {
                    let expr = self.parse_expression(
                        *reg,
                        expressions,
                        locals,
                        locals_map,
                        parameter_lookup,
                    );
                    expressions.append(expr)
                };

                let mut typefier = crate::proc::Typifier::new();
                let name = ident.0.as_str();
                let type_handle = typefier
                    .resolve(
                        handle,
                        expressions,
                        &mut self.types,
                        &self.constants,
                        &self.globals,
                        locals,
                        &self.functions,
                        &self.function_lookup,
                    )
                    .unwrap();
                let base_type = &self.types[type_handle];
                match base_type.inner {
                    crate::TypeInner::Struct { ref members } => {
                        let index = members
                            .iter()
                            .position(|m| m.name.as_ref().map(|s| s.as_str()) == Some(name))
                            .unwrap() as u32;
                        crate::Expression::AccessIndex {
                            base: handle,
                            index,
                        }
                    }
                    crate::TypeInner::Vector { size, kind, width }
                    | crate::TypeInner::Matrix {
                        columns: size,
                        kind,
                        width,
                        ..
                    } => {
                        const MEMBERS: [char; 4] = ['x', 'y', 'z', 'w'];
                        if name.len() > 1 {
                            let mut components = Vec::with_capacity(name.len());
                            for ch in name.chars() {
                                let expr = crate::Expression::AccessIndex {
                                    base: handle,
                                    index: MEMBERS[..size as usize]
                                        .iter()
                                        .position(|&m| m == ch)
                                        .unwrap() as u32,
                                };
                                components.push(expressions.append(expr));
                            }
                            let size = match name.len() {
                                2 => crate::VectorSize::Bi,
                                3 => crate::VectorSize::Tri,
                                4 => crate::VectorSize::Quad,
                                _ => return panic!(),
                            };
                            let inner =
                                if let crate::TypeInner::Matrix { rows, .. } = base_type.inner {
                                    crate::TypeInner::Matrix {
                                        columns: size,
                                        rows,
                                        kind,
                                        width,
                                    }
                                } else {
                                    crate::TypeInner::Vector { size, kind, width }
                                };
                            crate::Expression::Compose {
                                ty: crate::proc::Typifier::deduce_type_handle(
                                    inner,
                                    &mut self.types,
                                ),
                                components,
                            }
                        } else {
                            let ch = name.chars().next().unwrap();
                            let index = MEMBERS[..size as usize]
                                .iter()
                                .position(|&m| m == ch)
                                .unwrap() as u32;
                            crate::Expression::AccessIndex {
                                base: handle,
                                index,
                            }
                        }
                    }
                    _ => panic!(),
                }
            }
            Expr::PostInc(reg) => unimplemented!(),
            Expr::PostDec(reg) => unimplemented!(),
            Expr::Comma(_, _) => unimplemented!(),
        }
    }

    // None = void
    fn parse_type(&mut self, ty: TypeSpecifier) -> Option<Handle<Type>> {
        let base_ty = match helpers::glsl_to_spirv_type(ty.ty) {
            Some(ty) => ty,
            None => return None,
        };

        let ty = if let Some(specifier) = ty.array_specifier {
            let handle = self.types.fetch_or_append(Type {
                name: None,
                inner: base_ty,
            });

            match specifier {
                ArraySpecifier::Unsized => TypeInner::Array {
                    base: handle,
                    size: ArraySize::Dynamic,
                },
                ArraySpecifier::ExplicitlySized(_) => unimplemented!(),
            }
        } else {
            base_ty
        };

        Some(self.types.fetch_or_append(Type {
            name: None,
            inner: ty,
        }))
    }

    fn parse_global(&mut self, init: InitDeclaratorList) -> Handle<GlobalVariable> {
        let name = init.head.name.map(|d| d.0);
        let ty = {
            let ty = self.parse_type(init.head.ty.ty).unwrap();

            if let Some(specifier) = init.head.array_specifier {
                match specifier {
                    ArraySpecifier::Unsized => self.types.fetch_or_append(Type {
                        name: None,
                        inner: TypeInner::Array {
                            base: ty,
                            size: ArraySize::Dynamic,
                        },
                    }),
                    ArraySpecifier::ExplicitlySized(_) => unimplemented!(),
                }
            } else {
                ty
            }
        };

        let (class, binding) = init
            .head
            .ty
            .qualifier
            .map(|qualifiers| Self::parse_type_qualifier(qualifiers))
            .unwrap_or((StorageClass::Private, None));

        self.globals.append(GlobalVariable {
            name,
            class,
            binding,
            ty,
        })
    }

    fn parse_type_qualifier(qualifier: TypeQualifier) -> (StorageClass, Option<Binding>) {
        let mut storage = None;
        let mut binding = None;

        for qualifier in qualifier.qualifiers {
            match qualifier {
                TypeQualifierSpec::Storage(storage_qualifier) => {
                    assert!(storage.is_none());

                    match storage_qualifier {
                        StorageQualifier::Const => storage = Some(StorageClass::UniformConstant),
                        StorageQualifier::In => storage = Some(StorageClass::Input),
                        StorageQualifier::Out => storage = Some(StorageClass::Output),
                        StorageQualifier::Uniform => storage = Some(StorageClass::Uniform),
                        StorageQualifier::Buffer => storage = Some(StorageClass::StorageBuffer),
                        StorageQualifier::Shared => storage = Some(StorageClass::Workgroup),
                        StorageQualifier::Coherent => storage = Some(StorageClass::Workgroup),
                        _ => panic!(),
                    }
                }
                TypeQualifierSpec::Layout(layout_qualifier) => {
                    use glsl::syntax::{Expr, LayoutQualifierSpec};

                    assert!(binding.is_none());

                    let mut set = None;
                    let mut bind = None;
                    let mut location = None;

                    for identifier in layout_qualifier.ids {
                        match identifier {
                            LayoutQualifierSpec::Identifier(identifier, Some(expr)) => {
                                if let Expr::IntConst(word) = *expr {
                                    match identifier.as_str() {
                                        "location" => {
                                            assert!(set.is_none(),);
                                            assert!(bind.is_none(),);
                                            assert!(location.is_none());

                                            location = Some(word);
                                        }
                                        "binding" => {
                                            assert!(bind.is_none(),);
                                            assert!(location.is_none());

                                            bind = Some(word);
                                        }
                                        "set " => {
                                            assert!(set.is_none(),);
                                            assert!(location.is_none());

                                            set = Some(word);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    if let (Some(set), Some(bind)) = (set, bind) {
                        binding = Some(Binding::Descriptor {
                            set: set as u32,
                            binding: bind as u32,
                        })
                    } else if let Some(location) = location {
                        binding = Some(Binding::Location(location as u32))
                    } else {
                        panic!()
                    }
                }
                _ => unimplemented!(),
            }
        }

        (storage.unwrap_or(StorageClass::Private), binding)
    }
}

pub fn parse_str(
    source: &str,
    entry: String,
    exec: ExecutionModel,
) -> Result<crate::Module, ParseError> {
    Parser::new(source)?.parse(entry, exec)
}

#[cfg(test)]
mod tests {
    use super::parse_str;

    #[test]
    fn test_vertex() {
        let data = r#"#version 450 core

layout(location=0) in vec3 a_position;
layout(location=1) in vec3 a_color;
layout(location=2) in vec3 a_normal;
        
layout(location=0) out vec3 v_position;
layout(location=1) out vec3 v_color;
layout(location=2) out vec3 v_normal;
        
layout(set=0, binding=0)
uniform Globals {
    mat4 u_view_proj;
    vec3 u_view_position;
};
        
layout(set=2, binding=0)
uniform Locals {
    mat4 u_transform;
    vec2 U_min_max;
};
        
void main() {
    v_color = a_color;
    v_normal = a_normal;
        
    v_position = (u_transform * vec4(a_position, 1.0)).xyz;
    gl_Position = u_view_proj * u_transform * vec4(a_position, 1.0);
}"#;

        println!(
            "{:#?}",
            parse_str(data, String::from("main"), spirv::ExecutionModel::Vertex)
        );
    }

    #[test]
    fn test() {
        let data = r#"#version 450

layout(location=0) in vec3 v_position;
layout(location=1) in vec3 v_color;
layout(location=2) in vec3 v_normal;
        
layout(location=0) out vec4 f_color;
        
layout(set=0, binding=0)
uniform Globals {
    mat4 u_view_proj;
    vec3 u_view_position;
};
        
layout(set = 1, binding = 0) uniform Light {
    vec3 u_position;
    vec3 u_color;
};
        
layout(set=2, binding=0)
uniform Locals {
    mat4 u_transform;
    vec2 u_min_max;
};
        
layout (set = 2, binding = 1) uniform texture2D t_color;
layout (set = 2, binding = 2) uniform sampler s_color;
        
float invLerp(float from, float to, float value){
    return (value - from) / (to - from);
}
        
void main() {
    vec3 object_color = 
        texture(sampler2D(t_color,s_color), vec2(invLerp(u_min_max.x,u_min_max.y,length(v_position)),0.0)).xyz;
        
    float ambient_strength = 0.1;
    vec3 ambient_color = u_color * ambient_strength;
        
    vec3 normal = normalize(v_normal);
    vec3 light_dir = normalize(u_position - v_position);
        
    float diffuse_strength = max(dot(normal, light_dir), 0.0);
    vec3 diffuse_color = u_color * diffuse_strength;
        
    vec3 view_dir = normalize(u_view_position - v_position);
    vec3 half_dir = normalize(view_dir + light_dir);
        
    float specular_strength = pow(max(dot(normal, half_dir), 0.0), 32);
        
    vec3 specular_color = specular_strength * u_color;
        
    vec3 result = (ambient_color + diffuse_color + specular_color) * object_color;
        
    f_color = vec4(result, 1.0);
}"#;

        println!(
            "{:#?}",
            parse_str(data, String::from("main"), spirv::ExecutionModel::Vertex)
        );
    }
}
