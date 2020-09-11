use crate::{
    Binding, BuiltIn, Expression, GlobalVariable, Handle, ScalarKind, ShaderStage, StorageAccess,
    StorageClass, Type, TypeInner, VectorSize,
};

use super::ast::*;

pub fn lookup_variable(program: &mut Program, name: &str) -> Option<Handle<Expression>> {
    let mut expression: Option<Handle<Expression>> = None;
    if name == "gl_Position" {
        if let ShaderStage::Vertex | ShaderStage::Fragment { .. } = program.shader_stage {
            let h = program.global_variables.fetch_or_append(GlobalVariable {
                name: Some(name.into()),
                class: match program.shader_stage {
                    ShaderStage::Vertex => StorageClass::Output,
                    ShaderStage::Fragment { .. } => StorageClass::Input,
                    _ => StorageClass::Input,
                },
                binding: Some(Binding::BuiltIn(BuiltIn::Position)),
                ty: program.types.fetch_or_append(Type {
                    name: None,
                    inner: TypeInner::Vector {
                        size: VectorSize::Quad,
                        kind: ScalarKind::Float,
                        width: 4,
                    },
                }),
                interpolation: None,
                storage_access: StorageAccess::empty(),
            });
            program.lookup_global_variables.insert(name.into(), h);
            let exp = program
                .context
                .expressions
                .append(Expression::GlobalVariable(h));
            program
                .context
                .lookup_global_var_exps
                .insert(name.into(), exp);

            expression = Some(exp);
        }
    }
    if program.shader_stage == ShaderStage::Vertex && name == "gl_VertexIndex" {
        let h = program.global_variables.fetch_or_append(GlobalVariable {
            name: Some(name.into()),
            class: StorageClass::Input,
            binding: Some(Binding::BuiltIn(BuiltIn::VertexIndex)),
            ty: program.types.fetch_or_append(Type {
                name: None,
                inner: TypeInner::Scalar {
                    kind: ScalarKind::Uint,
                    width: 4,
                },
            }),
            interpolation: None,
            storage_access: StorageAccess::empty(),
        });
        program.lookup_global_variables.insert(name.into(), h);
        let exp = program
            .context
            .expressions
            .append(Expression::GlobalVariable(h));
        program
            .context
            .lookup_global_var_exps
            .insert(name.into(), exp);

        expression = Some(exp);
    }

    if let Some(expression) = expression {
        Some(expression)
    } else if let Some(local_var) = program.context.lookup_local_var(name) {
        Some(local_var)
    } else if let Some(global_var) = program.context.lookup_global_var_exps.get(name) {
        Some(*global_var)
    } else {
        None
    }
}
