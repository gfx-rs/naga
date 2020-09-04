use crate::back::spv::{helpers, Instruction};
use spirv::{Op, Word};

pub(super) enum Signedness {
    Unsigned = 0,
    Signed = 1,
}

//
// Debug Instructions
//

pub(super) fn instruction_source(
    source_language: spirv::SourceLanguage,
    version: u32,
) -> Instruction {
    let mut instruction = Instruction::new(Op::Source);
    instruction.add_operand(source_language as u32);
    instruction.add_operands(helpers::bytes_to_words(&version.to_le_bytes()));
    instruction
}

pub(super) fn instruction_name(target_id: Word, name: &str) -> Instruction {
    let mut instruction = Instruction::new(Op::Name);
    instruction.add_operand(target_id);
    instruction.add_operands(helpers::string_to_words(name));
    instruction
}

//
// Annotation Instructions
//

pub(super) fn instruction_decorate(
    target_id: Word,
    decoration: spirv::Decoration,
    operands: &[Word],
) -> Instruction {
    let mut instruction = Instruction::new(Op::Decorate);
    instruction.add_operand(target_id);
    instruction.add_operand(decoration as u32);
    instruction.add_operands(Vec::from(operands));
    instruction
}

//
// Extension Instructions
//

pub(super) fn instruction_ext_inst_import(id: Word, name: &str) -> Instruction {
    let mut instruction = Instruction::new(Op::ExtInstImport);
    instruction.set_result(id);
    instruction.add_operands(helpers::string_to_words(name));
    instruction
}

//
// Mode-Setting Instructions
//

pub(super) fn instruction_memory_model(
    addressing_model: spirv::AddressingModel,
    memory_model: spirv::MemoryModel,
) -> Instruction {
    let mut instruction = Instruction::new(Op::MemoryModel);
    instruction.add_operand(addressing_model as u32);
    instruction.add_operand(memory_model as u32);
    instruction
}

pub(super) fn instruction_entry_point(
    execution_model: spirv::ExecutionModel,
    entry_point_id: Word,
    name: &str,
    interface_ids: Vec<Word>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::EntryPoint);
    instruction.add_operand(execution_model as u32);
    instruction.add_operand(entry_point_id);
    instruction.add_operands(helpers::string_to_words(name));
    instruction.add_operands(interface_ids);
    instruction
}

pub(super) fn instruction_execution_mode(
    function_id: Word,
    execution_mode: spirv::ExecutionMode,
) -> Instruction {
    let mut instruction = Instruction::new(Op::ExecutionMode);
    instruction.add_operand(function_id);
    instruction.add_operand(execution_mode as u32);
    instruction
}

pub(super) fn instruction_capability(capability: spirv::Capability) -> Instruction {
    let mut instruction = Instruction::new(Op::Capability);
    instruction.add_operand(capability as u32);
    instruction
}

//
// Type-Declaration Instructions
//

pub(super) fn instruction_type_void(id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeVoid);
    instruction.set_result(id);
    instruction
}

pub(super) fn instruction_type_bool(id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeBool);
    instruction.set_result(id);
    instruction
}

pub(super) fn instruction_type_int(id: Word, width: Word, signedness: Signedness) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeInt);
    instruction.set_result(id);
    instruction.add_operand(width);
    instruction.add_operand(signedness as u32);
    instruction
}

pub(super) fn instruction_type_float(id: Word, width: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeFloat);
    instruction.set_result(id);
    instruction.add_operand(width);
    instruction
}

pub(super) fn instruction_type_vector(
    id: Word,
    component_type_id: Word,
    component_count: crate::VectorSize,
) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeVector);
    instruction.set_result(id);
    instruction.add_operand(component_type_id);
    instruction.add_operand(component_count as u32);
    instruction
}

pub(super) fn instruction_type_matrix(
    id: Word,
    column_type_id: Word,
    column_count: crate::VectorSize,
) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeMatrix);
    instruction.set_result(id);
    instruction.add_operand(column_type_id);
    instruction.add_operand(column_count as u32);
    instruction
}

pub(super) fn instruction_type_image(
    id: Word,
    sampled_type_id: Word,
    dim: spirv::Dim,
    arrayed: bool,
    class: crate::ImageClass,
) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeImage);
    instruction.set_result(id);
    instruction.add_operand(sampled_type_id);
    instruction.add_operand(dim as u32);

    instruction.add_operand(match class {
        crate::ImageClass::Depth => 1,
        _ => 0,
    });
    instruction.add_operand(if arrayed { 1 } else { 0 });
    instruction.add_operand(match class {
        crate::ImageClass::Multisampled => 1,
        _ => 0,
    });
    instruction.add_operand(match class {
        crate::ImageClass::Sampled => 1,
        _ => 0,
    });

    let (format, access) = match class {
        crate::ImageClass::Storage(format, access) => {
            let spv_format = match format {
                crate::StorageFormat::R8Unorm => spirv::ImageFormat::R8,
                crate::StorageFormat::R8Snorm => spirv::ImageFormat::R8Snorm,
                crate::StorageFormat::R8Uint => spirv::ImageFormat::R8ui,
                crate::StorageFormat::R8Sint => spirv::ImageFormat::R8i,
                crate::StorageFormat::R16Uint => spirv::ImageFormat::R16ui,
                crate::StorageFormat::R16Sint => spirv::ImageFormat::R16i,
                crate::StorageFormat::R16Float => spirv::ImageFormat::R16f,
                crate::StorageFormat::Rg8Unorm => spirv::ImageFormat::Rg8,
                crate::StorageFormat::Rg8Snorm => spirv::ImageFormat::Rg8Snorm,
                crate::StorageFormat::Rg8Uint => spirv::ImageFormat::Rg8ui,
                crate::StorageFormat::Rg8Sint => spirv::ImageFormat::Rg8i,
                crate::StorageFormat::R32Uint => spirv::ImageFormat::R32ui,
                crate::StorageFormat::R32Sint => spirv::ImageFormat::R32i,
                crate::StorageFormat::R32Float => spirv::ImageFormat::R32f,
                crate::StorageFormat::Rg16Uint => spirv::ImageFormat::Rg16ui,
                crate::StorageFormat::Rg16Sint => spirv::ImageFormat::Rg16i,
                crate::StorageFormat::Rg16Float => spirv::ImageFormat::Rg16f,
                crate::StorageFormat::Rgba8Unorm => spirv::ImageFormat::Rgba8,
                crate::StorageFormat::Rgba8Snorm => spirv::ImageFormat::Rgba8Snorm,
                crate::StorageFormat::Rgba8Uint => spirv::ImageFormat::Rgba8ui,
                crate::StorageFormat::Rgba8Sint => spirv::ImageFormat::Rgba8i,
                crate::StorageFormat::Rgb10a2Unorm => spirv::ImageFormat::Rgb10a2ui,
                crate::StorageFormat::Rg11b10Float => spirv::ImageFormat::R11fG11fB10f,
                crate::StorageFormat::Rg32Uint => spirv::ImageFormat::Rg32ui,
                crate::StorageFormat::Rg32Sint => spirv::ImageFormat::Rg32i,
                crate::StorageFormat::Rg32Float => spirv::ImageFormat::Rg32f,
                crate::StorageFormat::Rgba16Uint => spirv::ImageFormat::Rgba16ui,
                crate::StorageFormat::Rgba16Sint => spirv::ImageFormat::Rgba16i,
                crate::StorageFormat::Rgba16Float => spirv::ImageFormat::Rgba16f,
                crate::StorageFormat::Rgba32Uint => spirv::ImageFormat::Rgba32ui,
                crate::StorageFormat::Rgba32Sint => spirv::ImageFormat::Rgba32i,
                crate::StorageFormat::Rgba32Float => spirv::ImageFormat::Rgba32f,
            };
            (spv_format, access)
        }
        _ => (spirv::ImageFormat::Unknown, crate::StorageAccess::empty()),
    };

    instruction.add_operand(format as u32);
    // Access Qualifier
    if !access.is_empty() {
        instruction.add_operand(if access == crate::StorageAccess::all() {
            2
        } else if access.contains(crate::StorageAccess::STORE) {
            1
        } else {
            0
        });
    }

    instruction
}

pub(super) fn instruction_type_sampler(id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeSampler);
    instruction.set_result(id);
    instruction
}

pub(super) fn instruction_type_array(
    id: Word,
    element_type_id: Word,
    length_id: Word,
) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeArray);
    instruction.set_result(id);
    instruction.add_operand(element_type_id);
    instruction.add_operand(length_id);
    instruction
}

pub(super) fn instruction_type_runtime_array(id: Word, element_type_id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeRuntimeArray);
    instruction.set_result(id);
    instruction.add_operand(element_type_id);
    instruction
}

pub(super) fn instruction_type_struct(id: Word, member_ids: Vec<Word>) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeStruct);
    instruction.set_result(id);
    instruction.add_operands(member_ids);
    instruction
}

pub(super) fn instruction_type_pointer(
    id: Word,
    storage_class: spirv::StorageClass,
    type_id: Word,
) -> Instruction {
    let mut instruction = Instruction::new(Op::TypePointer);
    instruction.set_result(id);
    instruction.add_operand(storage_class as u32);
    instruction.add_operand(type_id);
    instruction
}

pub(super) fn instruction_type_function(
    id: Word,
    return_type_id: Word,
    parameter_ids: Vec<Word>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::TypeFunction);
    instruction.set_result(id);
    instruction.add_operand(return_type_id);
    instruction.add_operands(parameter_ids);
    instruction
}

//
// Constant-Creation Instructions
//

pub(super) fn instruction_constant_true(scalar_constant_id: Word, id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::ConstantTrue);
    instruction.set_type(scalar_constant_id);
    instruction.set_result(id);
    instruction
}

pub(super) fn instruction_constant_false(scalar_constant_id: Word, id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::ConstantFalse);
    instruction.set_type(scalar_constant_id);
    instruction.set_result(id);
    instruction
}

pub(super) fn instruction_constant(
    scalar_constant_id: Word,
    id: Word,
    values: &[Word],
) -> Instruction {
    let mut instruction = Instruction::new(Op::Constant);
    instruction.set_type(scalar_constant_id);
    instruction.set_result(id);
    for value in values {
        instruction.add_operand(*value);
    }
    instruction
}

pub(super) fn instruction_constant_composite(
    composite_type_id: Word,
    id: Word,
    constituent_ids: Vec<Word>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::ConstantComposite);
    instruction.set_type(composite_type_id);
    instruction.set_result(id);
    instruction.add_operands(constituent_ids);
    instruction
}

//
// Memory Instructions
//

pub(super) fn instruction_variable(
    result_type_id: Word,
    result_id: Word,
    storage_class: spirv::StorageClass,
    initializer_id: Option<Word>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::Variable);
    instruction.set_type(result_type_id);
    instruction.set_result(result_id);
    instruction.add_operand(storage_class as u32);

    if let Some(initializer_id) = initializer_id {
        instruction.add_operand(initializer_id);
    }

    instruction
}

pub(super) fn instruction_load(
    type_id: Word,
    id: Word,
    pointer_type_id: Word,
    memory_access: Option<spirv::MemoryAccess>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::Load);
    instruction.set_type(type_id);
    instruction.set_result(id);
    instruction.add_operand(pointer_type_id);

    instruction.add_operand(if let Some(memory_access) = memory_access {
        memory_access.bits()
    } else {
        spirv::MemoryAccess::NONE.bits()
    });

    instruction
}

pub(super) fn instruction_store(
    pointer_type_id: Word,
    object_id: Word,
    memory_access: Option<spirv::MemoryAccess>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::Store);
    instruction.add_operand(pointer_type_id);
    instruction.add_operand(object_id);

    instruction.add_operand(if let Some(memory_access) = memory_access {
        memory_access.bits()
    } else {
        spirv::MemoryAccess::NONE.bits()
    });

    instruction
}

//
// Function Instructions
//

pub(super) fn instruction_function(
    return_type_id: Word,
    id: Word,
    function_control: spirv::FunctionControl,
    function_type_id: Word,
) -> Instruction {
    let mut instruction = Instruction::new(Op::Function);
    instruction.set_type(return_type_id);
    instruction.set_result(id);
    instruction.add_operand(function_control.bits());
    instruction.add_operand(function_type_id);
    instruction
}

pub(super) fn instruction_function_parameter(result_type_id: Word, id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::FunctionParameter);
    instruction.set_type(result_type_id);
    instruction.set_result(id);
    instruction
}

pub(super) fn instruction_function_end() -> Instruction {
    Instruction::new(Op::FunctionEnd)
}

pub(super) fn instruction_function_call(
    result_type_id: Word,
    id: Word,
    function_id: Word,
    argument_ids: Vec<Word>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::FunctionCall);
    instruction.set_type(result_type_id);
    instruction.set_result(id);
    instruction.add_operand(function_id);
    instruction.add_operands(argument_ids);
    instruction
}

//
// Image Instructions
//

//
// Conversion Instructions
//

//
// Composite Instructions
//

pub(super) fn instruction_composite_construct(
    composite_type_id: Word,
    id: Word,
    constituent_ids: Vec<Word>,
) -> Instruction {
    let mut instruction = Instruction::new(Op::CompositeConstruct);
    instruction.set_type(composite_type_id);
    instruction.set_result(id);
    instruction.add_operands(constituent_ids);
    instruction
}

//
// Arithmetic Instructions
//

pub(super) fn instruction_vector_times_scalar(
    float_type_id: Word,
    id: Word,
    vector_type_id: Word,
    scalar_type_id: Word,
) -> Instruction {
    let mut instruction = Instruction::new(Op::VectorTimesScalar);
    instruction.set_type(float_type_id);
    instruction.set_result(id);
    instruction.add_operand(vector_type_id);
    instruction.add_operand(scalar_type_id);
    instruction
}

//
// Bit Instructions
//

//
// Relational and Logical Instructions
//

//
// Derivative Instructions
//

//
// Control-Flow Instructions
//

pub(super) fn instruction_label(id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::Label);
    instruction.set_result(id);
    instruction
}

pub(super) fn instruction_return() -> Instruction {
    Instruction::new(Op::Return)
}

pub(super) fn instruction_return_value(value_id: Word) -> Instruction {
    let mut instruction = Instruction::new(Op::ReturnValue);
    instruction.add_operand(value_id);
    instruction
}

//
// Atomic Instructions
//

//
// Primitive Instructions
//

#[cfg(test)]
mod tests {
    use crate::back::spv::test_framework::*;
    use spirv::*;

    #[test]
    fn test_instruction_capability() {
        let instruction = super::instruction_capability(Capability::Shader);
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::Capability,
            wc: 2,
            type_id: false,
            result_id: false,
            operands: true,
        };

        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_ext_inst_import() {
        let import_name = "GLSL.std.450";
        let instruction = super::instruction_ext_inst_import(1, import_name);
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::ExtInstImport,
            wc: 2,
            type_id: false,
            result_id: true,
            operands: true,
        };

        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_memory_model() {
        let instruction =
            super::instruction_memory_model(AddressingModel::Logical, MemoryModel::GLSL450);
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::MemoryModel,
            wc: 3,
            type_id: false,
            result_id: false,
            operands: true,
        };
        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_name() {
        let instruction = super::instruction_name(1, "Test");
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::Name,
            wc: 3,
            type_id: false,
            result_id: true,
            operands: true,
        };
        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_execution_mode() {
        let instruction = super::instruction_execution_mode(1, ExecutionMode::OriginUpperLeft);
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::ExecutionMode,
            wc: 3,
            type_id: false,
            result_id: false,
            operands: true,
        };
        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_source() {
        let version = 450;
        let instruction = super::instruction_source(SourceLanguage::GLSL, version);
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::Source,
            wc: 3,
            type_id: false,
            result_id: false,
            operands: true,
        };
        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_label() {
        let instruction = super::instruction_label(1);
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::Label,
            wc: 2,
            type_id: false,
            result_id: true,
            operands: false,
        };
        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_function_end() {
        let instruction = super::instruction_function_end();
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::FunctionEnd,
            wc: 1,
            type_id: false,
            result_id: false,
            operands: false,
        };
        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }

    #[test]
    fn test_instruction_decorate() {
        let instruction = super::instruction_decorate(1, Decoration::Location, &[1]);
        let mut output = vec![];

        let requirements = SpecRequirements {
            op: Op::Decorate,
            wc: 3,
            type_id: false,
            result_id: false,
            operands: true,
        };
        validate_spec_requirements(requirements, &instruction);

        instruction.to_words(&mut output);
        validate_instruction(output.as_slice(), &instruction);
    }
}
