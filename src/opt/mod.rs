use crate::{Module, Expression, BinaryOperator, TypeInner};
use crate::proc::TypeResolution;

pub fn optimise(module: &mut Module, info: &crate::valid::ModuleInfo) {
    consecutive_mul_vec_scalar_pass(module, info);
}

pub fn consecutive_mul_vec_scalar_pass(module: &mut Module, info: &crate::valid::ModuleInfo) {
    for (function_handle, function) in module.functions.iter_mut() {
        let info = &info[function_handle];

        let mut map = std::collections::HashMap::new();
        let mut to_change = std::collections::HashMap::new();

        for (output, expression) in function.expressions.iter().rev() {
            if let Expression::Binary {
                op: BinaryOperator::Multiply,
                left: input_vec, right: input_scalar
            } = expression {
                let output_ty = match &info[output].ty {
                    TypeResolution::Handle(handle) => &module.types[*handle].inner,
                    TypeResolution::Value(value) => value
                };
                if !matches!(output_ty, TypeInner::Vector { .. }) {
                    continue;
                }

                let output_vec = output;

                map.insert(*input_vec, (*input_scalar, output_vec));

                let (earlier_input_scalar, earlier_output_vec) = match map.get(&output_vec) {
                    Some(earlier) => earlier,
                    None => continue
                };

                to_change.insert(*earlier_output_vec, Expression::Binary {
                    op: BinaryOperator::Multiply,
                    left: *input_vec,
                    right: output_vec
                });

                to_change.insert(output_vec, Expression::Binary {
                    op: BinaryOperator::Multiply,
                    left: *input_scalar,
                    right: *earlier_input_scalar,
                });
            }
        }

        for (handle, expression) in to_change.drain() {
            *function.expressions.get_mut(handle) = expression;
        }
    }
}