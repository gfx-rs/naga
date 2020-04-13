/*! Standard Portable Intermediate Representation (SPIR-V) backend !*/
use super::layout::{Instruction, Module};
use crate::FastHashMap;
use spirv::*;

trait LookupHelper<T> {
    type Target;
    fn lookup_id(&self, handle: &crate::Handle<T>) -> Option<Word>;
    fn lookup_handle(&self, word: &Word) -> Option<crate::Handle<T>>;
}

impl<T> LookupHelper<T> for FastHashMap<Word, crate::Handle<T>> {
    type Target = T;

    fn lookup_id(&self, handle: &crate::Handle<T>) -> Option<Word> {
        let mut word = None;
        for (k, v) in self.iter() {
            if v.eq(handle) {
                word = Some(*k);
                break;
            }
        }
        word
    }

    fn lookup_handle(&self, word: &u32) -> Option<crate::Handle<T>> {
        let mut handle = None;
        for (k, v) in self.iter() {
            if k.eq(word) {
                handle = Some(*v);
                break;
            }
        }
        handle
    }
}

#[derive(Debug, PartialEq)]
struct LookupFunctionType {
    parameter_type_ids: Vec<Word>,
    return_type_id: Word,
}

pub struct Parser {
    module: Module,
    id_count: u32,
    capabilities: Vec<Capability>,
    debugs: Vec<Instruction>,
    annotations: Vec<Instruction>,
    debug_enabled: bool,
    void_type: Option<u32>,
    lookup_type: FastHashMap<Word, crate::Handle<crate::Type>>,
    lookup_function: FastHashMap<Word, crate::Handle<crate::Function>>,
    lookup_function_type: FastHashMap<Word, LookupFunctionType>,
    lookup_constant: FastHashMap<Word, crate::Handle<crate::Constant>>,
    lookup_global_variable: FastHashMap<Word, crate::Handle<crate::GlobalVariable>>,
    lookup_import: FastHashMap<Word, String>,
    lookup_label: Vec<Word>,
    current_label: Word,
}

impl Parser {
    pub fn new(module: &crate::Module, debug_enabled: bool) -> Self {
        Parser {
            module: Module::new(module),
            id_count: 0,
            capabilities: vec![],
            debugs: vec![],
            annotations: vec![],
            debug_enabled,
            void_type: None,
            lookup_type: FastHashMap::default(),
            lookup_function: FastHashMap::default(),
            lookup_function_type: FastHashMap::default(),
            lookup_constant: FastHashMap::default(),
            lookup_global_variable: FastHashMap::default(),
            lookup_import: FastHashMap::default(),
            lookup_label: vec![],
            current_label: 0,
        }
    }

    fn generate_id(&mut self) -> Word {
        self.id_count += 1;
        self.id_count
    }

    fn bytes_to_words(&self, bytes: &[u8]) -> Vec<Word> {
        let words: Vec<Word> = bytes
            .chunks(4)
            .map(|chars| match chars.len() {
                4 => {
                    (chars[3] as u32) << 24
                        | (chars[2] as u32) << 16
                        | (chars[1] as u32) << 8
                        | chars[0] as u32
                }
                3 => {
                    0x0u32 << 24
                        | (chars[2] as u32) << 16
                        | (chars[1] as u32) << 8
                        | (chars[0] as u32) as u32
                }
                2 => 0x0u32 << 24 | 0x0u32 << 16 | (chars[1] as u32) << 8 | chars[0] as u32,
                1 => 0x0u32 << 24 | 0x0u32 << 16 | 0x0u32 << 8 | chars[0] as u32,
                _ => 0x0u32,
            })
            .collect();

        words
    }

    fn string_to_words(&self, input: &str) -> Vec<Word> {
        let mut words: Vec<Word> = self.bytes_to_words(input.as_bytes());

        let last_word = words.last().unwrap();

        let last_character = last_word.to_le_bytes()[3];

        if last_character != 0x0 {
            // nul-termination
            words.push(0x0u32);
        }

        words
    }

    fn instruction_capability(&self, capability: &Capability) -> Instruction {
        let mut instruction = Instruction::new(Op::Capability);
        instruction.add_operand(*capability as u32);
        instruction
    }

    fn try_add_capabilities(&mut self, capabilities: &[Capability]) {
        for capability in capabilities.iter() {
            if !self.capabilities.contains(capability) {
                self.instruction_capability(capability);
                self.capabilities.push(*capability);
            }
        }
    }

    fn instruction_ext_inst_import(&mut self) -> Instruction {
        let mut instruction = Instruction::new(Op::ExtInstImport);
        let id = self.generate_id();
        instruction.set_result(id);

        // TODO Support other imports
        instruction.add_operands(self.string_to_words("GLSL.std.450"));
        self.lookup_import.insert(id, String::from("GLSL.std.450"));

        instruction
    }

    fn instruction_memory_model(&mut self) -> Instruction {
        let mut instruction = Instruction::new(Op::MemoryModel);
        let addressing_model = AddressingModel::Logical;
        let memory_model = MemoryModel::GLSL450;
        self.try_add_capabilities(addressing_model.required_capabilities());
        self.try_add_capabilities(memory_model.required_capabilities());

        instruction.add_operand(addressing_model as u32);
        instruction.add_operand(memory_model as u32);
        instruction
    }

    fn instruction_entry_point(
        &mut self,
        entry_point: &crate::EntryPoint,
        ir_module: &crate::Module,
    ) -> Instruction {
        let mut instruction = Instruction::new(Op::EntryPoint);
        let function_id = self
            .lookup_function
            .lookup_id(&entry_point.function)
            .unwrap();

        instruction.add_operand(entry_point.exec_model as u32);
        instruction.add_operand(function_id);

        if self.debug_enabled {
            let mut debug_instruction = Instruction::new(Op::Name);
            debug_instruction.set_result(function_id);
            debug_instruction.add_operands(self.string_to_words(entry_point.name.as_str()));
            self.debugs.push(debug_instruction);
        }

        instruction.add_operands(self.string_to_words(entry_point.name.as_str()));

        let function = &ir_module.functions[entry_point.function];
        for ((handle, _), &usage) in ir_module
            .global_variables
            .iter()
            .zip(&function.global_usage)
        {
            if usage.contains(crate::GlobalUse::STORE) || usage.contains(crate::GlobalUse::LOAD) {
                let id = self.get_global_variable_id(
                    &ir_module.types,
                    &ir_module.global_variables,
                    &handle,
                );
                instruction.add_operand(id);
            }
        }

        self.try_add_capabilities(entry_point.exec_model.required_capabilities());
        match entry_point.exec_model {
            ExecutionModel::Vertex | ExecutionModel::GLCompute => {}
            ExecutionModel::Fragment => {
                let execution_mode = ExecutionMode::OriginUpperLeft;
                self.try_add_capabilities(execution_mode.required_capabilities());
                let mut execution_mode_instruction = Instruction::new(Op::ExecutionMode);
                execution_mode_instruction.add_operand(function_id);
                execution_mode_instruction.add_operand(execution_mode as u32);
                for word in execution_mode_instruction.to_words() {
                    self.module.logical_layout.execution_modes.push(word)
                }
            }

            _ => unimplemented!("{:?}", entry_point.exec_model),
        }

        instruction
    }

    fn get_type_id(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        handle: &crate::Handle<crate::Type>,
    ) -> Word {
        match self.lookup_type.lookup_id(&handle) {
            Some(word) => word,
            None => {
                let (instruction, id) = self.instruction_type_declaration(arena, *handle);

                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }
                id
            }
        }
    }

    fn get_constant_id(
        &mut self,
        handle: &crate::Handle<crate::Constant>,
        ir_module: &crate::Module,
    ) -> Word {
        match self.lookup_constant.lookup_id(&handle) {
            Some(word) => word,
            None => {
                let (instruction, id) = self.instruction_constant_type(*handle, ir_module);

                for words in instruction.to_words() {
                    self.module.logical_layout.constants.push(words);
                }
                id
            }
        }
    }

    fn get_global_variable_id(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        global_arena: &crate::Arena<crate::GlobalVariable>,
        handle: &crate::Handle<crate::GlobalVariable>,
    ) -> Word {
        match self.lookup_global_variable.lookup_id(&handle) {
            Some(word) => word,
            None => {
                let global_variable = &global_arena[*handle];
                let (instruction, id) =
                    self.instruction_global_variable(arena, global_variable, handle);

                for words in instruction.to_words() {
                    self.module.logical_layout.global_variables.push(words);
                }
                id
            }
        }
    }

    fn get_function_type(
        &mut self,
        ty: Option<crate::Handle<crate::Type>>,
        arena: &crate::Arena<crate::Type>,
    ) -> Word {
        match ty {
            Some(handle) => self.get_type_id(arena, &handle),
            None => match self.void_type {
                Some(id) => id,
                None => {
                    let id = self.generate_id();

                    let mut instruction = Instruction::new(Op::TypeVoid);
                    instruction.set_result(id);

                    self.void_type = Some(id);
                    for word in instruction.to_words().iter() {
                        self.module.logical_layout.type_declarations.push(*word);
                    }
                    id
                }
            },
        }
    }

    fn find_scalar_handle(
        &self,
        arena: &crate::Arena<crate::Type>,
        kind: &crate::ScalarKind,
        width: &u8,
    ) -> crate::Handle<crate::Type> {
        let mut scalar_handle = None;
        for (handle, ty) in arena.iter() {
            match ty.inner {
                crate::TypeInner::Scalar {
                    kind: _kind,
                    width: _width,
                } => {
                    if kind == &_kind && width == &_width {
                        scalar_handle = Some(handle);
                        break;
                    }
                }
                _ => continue,
            }
        }
        scalar_handle.unwrap()
    }

    fn instruction_type_declaration(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        handle: crate::Handle<crate::Type>,
    ) -> (Instruction, Word) {
        let ty = &arena[handle];
        let id = self.generate_id();
        let mut instruction;

        match ty.inner {
            crate::TypeInner::Scalar { kind, width } => {
                match kind {
                    crate::ScalarKind::Sint => {
                        instruction = Instruction::new(Op::TypeInt);
                        instruction.set_result(id);
                        instruction.add_operand(width as u32);
                        instruction.add_operand(0x1u32);
                    }
                    crate::ScalarKind::Uint => {
                        instruction = Instruction::new(Op::TypeInt);
                        instruction.set_result(id);
                        instruction.add_operand(width as u32);
                        instruction.add_operand(0x0u32);
                    }
                    crate::ScalarKind::Float => {
                        instruction = Instruction::new(Op::TypeFloat);
                        instruction.set_result(id);
                        instruction.add_operand(width as u32);
                    }
                    crate::ScalarKind::Bool => {
                        instruction = Instruction::new(Op::TypeBool);
                        instruction.set_result(id);
                    }
                }
                self.lookup_type.insert(id, handle);
            }
            crate::TypeInner::Vector { size, kind, width } => {
                let scalar_handle = self.find_scalar_handle(arena, &kind, &width);
                let scalar_id = self.get_type_id(arena, &scalar_handle);

                instruction = Instruction::new(Op::TypeVector);
                instruction.set_result(id);
                instruction.add_operand(scalar_id);
                instruction.add_operand(size as u32);

                self.lookup_type.insert(id, handle);
            }
            crate::TypeInner::Matrix {
                columns,
                rows: _,
                kind,
                width,
            } => {
                let scalar_handle = self.find_scalar_handle(arena, &kind, &width);
                let scalar_id = self.get_type_id(arena, &scalar_handle);

                instruction = Instruction::new(Op::TypeMatrix);
                instruction.set_result(id);
                instruction.add_operand(scalar_id);
                instruction.add_operand(columns as u32);
            }
            crate::TypeInner::Pointer { base, class } => {
                let type_id = self.get_type_id(arena, &base);
                instruction = Instruction::new(Op::TypePointer);
                instruction.set_result(id);
                instruction.add_operand(class as u32);
                instruction.add_operand(type_id);

                self.lookup_type.insert(id, handle);
            }
            crate::TypeInner::Array { base, size } => {
                let type_id = self.get_type_id(arena, &base);

                instruction = Instruction::new(Op::TypeArray);
                instruction.set_result(id);
                instruction.add_operand(type_id);

                match size {
                    crate::ArraySize::Static(word) => {
                        instruction.add_operand(word);
                    }
                    _ => panic!("Array size {:?} unsupported", size),
                }

                self.lookup_type.insert(id, base);
            }
            crate::TypeInner::Struct { ref members } => {
                instruction = Instruction::new(Op::TypeStruct);
                instruction.set_result(id);

                for member in members {
                    let type_id = self.get_type_id(arena, &member.ty);
                    instruction.add_operand(type_id);
                }

                self.lookup_type.insert(id, handle);
            }
            crate::TypeInner::Image { base, dim, flags } => {
                let type_id = self.get_type_id(arena, &base);
                self.try_add_capabilities(dim.required_capabilities());

                instruction = Instruction::new(Op::TypeImage);
                instruction.set_result(id);
                instruction.add_operand(type_id);
                instruction.add_operand(dim as u32);

                // TODO Add Depth, but how to determine? Not yet in the WGSL spec
                instruction.add_operand(1);

                if flags.contains(crate::ImageFlags::ARRAYED) {
                    instruction.add_operand(1);
                } else {
                    instruction.add_operand(0);
                }

                if flags.contains(crate::ImageFlags::MULTISAMPLED) {
                    instruction.add_operand(1);
                } else {
                    instruction.add_operand(0);
                }

                let is_subpass_data = match dim {
                    Dim::DimSubpassData => true,
                    _ => false,
                };

                if is_subpass_data {
                    instruction.add_operand(2);
                    instruction.add_operand(ImageFormat::Unknown as u32);
                } else {
                    if flags.contains(crate::ImageFlags::SAMPLED) {
                        instruction.add_operand(1);
                    } else {
                        instruction.add_operand(0);
                    }

                    // TODO Add Image Format, but how to determine? Not yet in the WGSL spec
                    instruction.add_operand(ImageFormat::Unknown as u32);
                }

                if flags.contains(crate::ImageFlags::CAN_STORE)
                    && flags.contains(crate::ImageFlags::CAN_LOAD)
                {
                    instruction.add_operand(2);
                } else {
                    if flags.contains(crate::ImageFlags::CAN_STORE) {
                        instruction.add_operand(1);
                    } else if flags.contains(crate::ImageFlags::CAN_LOAD) {
                        instruction.add_operand(0);
                    }
                }

                self.lookup_type.insert(id, base);
            }
            crate::TypeInner::Sampler => {
                instruction = Instruction::new(Op::TypeSampler);
                instruction.set_result(id);
                self.lookup_type.insert(id, handle);
            }
        }

        (instruction, id)
    }

    fn instruction_constant_type(
        &mut self,
        handle: crate::Handle<crate::Constant>,
        ir_module: &crate::Module,
    ) -> (Instruction, Word) {
        let id = self.generate_id();
        self.lookup_constant.insert(id, handle);
        let constant = &ir_module.constants[handle];
        let arena = &ir_module.types;

        match constant.inner {
            crate::ConstantInner::Sint(val) => {
                let type_id = self.get_type_id(arena, &constant.ty);

                let mut instruction = Instruction::new(Op::Constant);
                instruction.set_type(type_id);
                instruction.set_result(id);

                let ty = &ir_module.types[constant.ty];
                match ty.inner {
                    crate::TypeInner::Scalar { kind: _, width } => match width {
                        32 => {
                            instruction.add_operand(val as u32);
                        }
                        64 => {
                            let (low, high) = ((val >> 32) as u32, val as u32);
                            instruction.add_operand(low);
                            instruction.add_operand(high);
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }

                (instruction, id)
            }
            crate::ConstantInner::Uint(val) => {
                let type_id = self.get_type_id(arena, &constant.ty);

                let mut instruction = Instruction::new(Op::Constant);
                instruction.set_type(type_id);
                instruction.set_result(id);

                let ty = &ir_module.types[constant.ty];
                match ty.inner {
                    crate::TypeInner::Scalar { kind: _, width } => match width {
                        32 => {
                            instruction.add_operand(val as u32);
                        }
                        64 => {
                            let (low, high) = ((val >> 32) as u32, val as u32);
                            instruction.add_operand(low);
                            instruction.add_operand(high);
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }

                (instruction, id)
            }
            crate::ConstantInner::Float(val) => {
                let type_id = self.get_type_id(arena, &constant.ty);

                let mut instruction = Instruction::new(Op::Constant);
                instruction.set_type(type_id);
                instruction.set_result(id);

                let ty = &ir_module.types[constant.ty];
                match ty.inner {
                    crate::TypeInner::Scalar { kind: _, width } => match width {
                        32 => {
                            instruction.add_operand((val as f32).to_bits());
                        }
                        64 => {
                            let bits = f64::to_bits(val);
                            let (low, high) = ((bits >> 32) as u32, bits as u32);
                            instruction.add_operand(low);
                            instruction.add_operand(high);
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }

                (instruction, id)
            }
            crate::ConstantInner::Bool(val) => {
                let type_id = self.get_type_id(arena, &constant.ty);
                let mut instruction;

                if val {
                    instruction = Instruction::new(Op::ConstantTrue);
                    instruction.set_type(type_id);
                    instruction.set_result(id);
                } else {
                    instruction = Instruction::new(Op::ConstantFalse);
                    instruction.set_type(type_id);
                    instruction.set_result(id);
                }
                (instruction, id)
            }
            crate::ConstantInner::Composite(ref constituents) => {
                let type_id = self.get_type_id(arena, &constant.ty);

                let mut instruction = Instruction::new(Op::ConstantComposite);
                instruction.set_type(type_id);
                instruction.set_result(id);

                for constituent in constituents.iter() {
                    let id = self.get_constant_id(constituent, &ir_module);
                    instruction.add_operand(id);
                }

                (instruction, id)
            }
        }
    }

    fn get_pointer_id(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        handle: &crate::Handle<crate::Type>,
        class: &StorageClass,
    ) -> Word {
        let ty = &arena[*handle];
        let type_id = self.get_type_id(arena, handle);
        match ty.inner {
            crate::TypeInner::Pointer { .. } => type_id,
            _ => {
                let pointer_id = self.generate_id();
                let mut instruction = Instruction::new(Op::TypePointer);
                instruction.set_result(pointer_id);
                instruction.add_operand((*class) as u32);
                instruction.add_operand(type_id);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }

                /* TODO
                    Not able to lookup Pointer, because there is no Handle in the IR for it.
                    Idea would be to not have any handles at all in the lookups, so we aren't bound
                    to the IR. We can then insert, like here runtime values to the lookups
                */
                // self.lookup_type.insert(pointer_id, global_variable.ty);
                pointer_id
            }
        }
    }

    fn instruction_global_variable(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        global_variable: &crate::GlobalVariable,
        handle: &crate::Handle<crate::GlobalVariable>,
    ) -> (Instruction, Word) {
        let mut instruction = Instruction::new(Op::Variable);
        let id = self.generate_id();

        self.try_add_capabilities(global_variable.class.required_capabilities());

        let pointer_id = self.get_pointer_id(arena, &global_variable.ty, &global_variable.class);

        instruction.set_type(pointer_id);
        instruction.set_result(id);
        instruction.add_operand(global_variable.class as u32);

        if self.debug_enabled {
            let mut debug_instruction = Instruction::new(Op::Name);
            debug_instruction.set_result(id);
            debug_instruction.add_operands(
                self.string_to_words(global_variable.name.as_ref().unwrap().as_str()),
            );
            self.debugs.push(debug_instruction);
        }

        match global_variable.binding.as_ref().unwrap() {
            crate::Binding::Location(location) => {
                let mut instruction = Instruction::new(Op::Decorate);
                instruction.add_operand(id);
                instruction.add_operand(Decoration::Location as u32);
                instruction.add_operand(*location);
                self.annotations.push(instruction);
            }
            crate::Binding::Descriptor { set, binding } => {
                let mut set_instruction = Instruction::new(Op::Decorate);
                set_instruction.add_operand(id);
                set_instruction.add_operand(Decoration::DescriptorSet as u32);
                set_instruction.add_operand(*set);
                self.annotations.push(set_instruction);

                let mut binding_instruction = Instruction::new(Op::Decorate);
                binding_instruction.add_operand(id);
                binding_instruction.add_operand(Decoration::Binding as u32);
                binding_instruction.add_operand(*binding);
                self.annotations.push(binding_instruction);
            }
            crate::Binding::BuiltIn(built_in) => {
                let built_in_u32: u32 = unsafe { std::mem::transmute(*built_in) };

                let mut instruction = Instruction::new(Op::Decorate);
                instruction.add_operand(id);
                instruction.add_operand(Decoration::BuiltIn as u32);
                instruction.add_operand(built_in_u32);
                self.annotations.push(instruction);
            }
        }

        // TODO Initializer is optional and not (yet) included in the IR

        self.lookup_global_variable.insert(id, *handle);
        (instruction, id)
    }

    fn write_physical_layout(&mut self) {
        self.module.physical_layout.bound = self.id_count + 1;
    }

    fn instruction_source(&self) -> Instruction {
        let version = 450u32;

        let mut instruction = Instruction::new(Op::Source);
        instruction.add_operand(SourceLanguage::GLSL as u32);
        instruction.add_operands(self.bytes_to_words(&version.to_le_bytes()));
        instruction
    }

    fn instruction_function_type(&mut self, lookup_function_type: LookupFunctionType) -> Word {
        let mut id = None;

        for (k, v) in self.lookup_function_type.iter() {
            if v.eq(&lookup_function_type) {
                id = Some(*k);
                break;
            }
        }

        if id.is_none() {
            let _id = self.generate_id();
            id = Some(_id);

            let mut instruction = Instruction::new(Op::TypeFunction);
            instruction.set_result(_id);
            instruction.add_operand(lookup_function_type.return_type_id);

            for parameter_type_id in lookup_function_type.parameter_type_ids.iter() {
                instruction.add_operand(*parameter_type_id);
            }

            self.lookup_function_type.insert(_id, lookup_function_type);
            for word in instruction.to_words() {
                self.module.logical_layout.type_declarations.push(word);
            }
        }

        id.unwrap()
    }

    fn instruction_function(
        &mut self,
        handle: crate::Handle<crate::Function>,
        function: &crate::Function,
        arena: &crate::Arena<crate::Type>,
    ) -> Instruction {
        let id = self.generate_id();

        let return_type_id = self.get_function_type(function.return_type, arena);

        let mut instruction = Instruction::new(Op::Function);
        instruction.set_type(return_type_id);
        instruction.set_result(id);

        let control_u32: Word = unsafe { std::mem::transmute(function.control) };

        instruction.add_operand(control_u32);

        let mut parameter_type_ids = Vec::with_capacity(function.parameter_types.len());
        for parameter_type in function.parameter_types.iter() {
            parameter_type_ids.push(self.get_type_id(arena, &parameter_type))
        }

        let lookup_function_type = LookupFunctionType {
            return_type_id,
            parameter_type_ids,
        };

        let type_function_id = self.instruction_function_type(lookup_function_type);

        instruction.add_operand(type_function_id);

        self.lookup_function.insert(id, handle);

        instruction
    }

    fn get_type_by_inner(
        &self,
        arena: &crate::Arena<crate::Type>,
        inner: &crate::TypeInner,
    ) -> Word {
        let mut word = None;
        for (k, v) in self.lookup_type.iter() {
            let ty = &arena[*v];
            if ty.inner.eq(inner) {
                word = Some(*k);
                break;
            }
        }
        word.unwrap()
    }

    fn parse_expression<'a>(
        &mut self,
        ir_module: &'a crate::Module,
        function: &crate::Function,
        expression: &crate::Expression,
        output: &mut Vec<Instruction>,
    ) -> (Word, &'a crate::TypeInner) {
        match expression {
            crate::Expression::GlobalVariable(handle) => {
                let var = &ir_module.global_variables[*handle];
                let inner = &ir_module.types[var.ty].inner;
                let id = self.get_global_variable_id(
                    &ir_module.types,
                    &ir_module.global_variables,
                    handle,
                );
                (id, inner)
            }
            crate::Expression::Constant(handle) => {
                let var = &ir_module.constants[*handle];
                let inner = &ir_module.types[var.ty].inner;
                let id = self.get_constant_id(handle, ir_module);
                (id, inner)
            }
            crate::Expression::Compose { ty, components } => {
                let var = &ir_module.types[*ty];
                let inner = &var.inner;
                let id = self.generate_id();
                let type_id = self.get_type_id(&ir_module.types, &ty);

                let mut instruction = Instruction::new(Op::CompositeConstruct);
                instruction.set_type(type_id);
                instruction.set_result(id);

                for component in components {
                    let expression = &function.expressions[*component];
                    let (component_id, _) =
                        self.parse_expression(ir_module, &function, expression, output);
                    instruction.add_operand(component_id);
                }

                output.push(instruction);

                (id, inner)
            }
            crate::Expression::Binary { op, left, right } => {
                let left_expression = &function.expressions[*left];
                let right_expression = &function.expressions[*right];
                let (left_id, left_inner) =
                    self.parse_expression(ir_module, function, left_expression, output);
                let (right_id, right_inner) =
                    self.parse_expression(ir_module, function, right_expression, output);
                match op {
                    crate::BinaryOperator::Add => {
                        let mut instruction;
                        let id = self.generate_id();
                        // TODO Always assuming now that left and right are the same type
                        match left_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::IAdd);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Float => {
                                    instruction = Instruction::new(Op::FAdd);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Float,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            crate::TypeInner::Vector { size, kind, width } => {
                                let inner = &crate::TypeInner::Scalar {
                                    kind: *kind,
                                    width: *width,
                                };
                                let type_id = self.get_type_by_inner(&ir_module.types, inner);

                                match kind {
                                    crate::ScalarKind::Sint => {
                                        instruction = Instruction::new(Op::IAdd);
                                        instruction.set_type(type_id);
                                    }
                                    crate::ScalarKind::Float => {
                                        instruction = Instruction::new(Op::FAdd);
                                        instruction.set_type(type_id);
                                    }
                                    _ => unimplemented!("{:?}", kind),
                                }
                            }
                            _ => unimplemented!("{:?}", left_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        (id, left_inner)
                    }
                    crate::BinaryOperator::Subtract => {
                        let mut instruction;
                        let id = self.generate_id();
                        // TODO Always assuming now that left and right are the same type
                        match left_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::SNegate);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Float => {
                                    instruction = Instruction::new(Op::FNegate);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Float,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            crate::TypeInner::Vector { size, kind, width } => {
                                let inner = &crate::TypeInner::Scalar {
                                    kind: *kind,
                                    width: *width,
                                };
                                let type_id = self.get_type_by_inner(&ir_module.types, inner);

                                match kind {
                                    crate::ScalarKind::Sint => {
                                        instruction = Instruction::new(Op::SNegate);
                                        instruction.set_type(type_id);
                                    }
                                    crate::ScalarKind::Float => {
                                        instruction = Instruction::new(Op::FNegate);
                                        instruction.set_type(type_id);
                                    }
                                    _ => unimplemented!("{:?}", kind),
                                }
                            }
                            _ => unimplemented!("{:?}", left_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        (id, left_inner)
                    }
                    crate::BinaryOperator::Multiply => {
                        // TODO OpVectorTimesScalar is only supported
                        let id = self.generate_id();

                        let result_type_id = self.get_type_by_inner(&ir_module.types, left_inner);

                        let mut instruction = Instruction::new(Op::VectorTimesScalar);
                        instruction.set_type(result_type_id);
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        output.push(instruction);

                        // TODO Not sure how or what to return
                        (id, left_inner)
                    }
                    crate::BinaryOperator::Divide => {
                        let mut instruction;
                        let id = self.generate_id();
                        // TODO Always assuming now that left and right are the same type
                        match left_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Uint => {
                                    instruction = Instruction::new(Op::UDiv);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Uint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::SDiv);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Float => {
                                    instruction = Instruction::new(Op::FDiv);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Float,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            crate::TypeInner::Vector { size, kind, width } => {
                                let inner = &crate::TypeInner::Scalar {
                                    kind: *kind,
                                    width: *width,
                                };
                                let type_id = self.get_type_by_inner(&ir_module.types, inner);

                                match kind {
                                    crate::ScalarKind::Uint => {
                                        instruction = Instruction::new(Op::UDiv);
                                        instruction.set_type(type_id);
                                    }
                                    crate::ScalarKind::Sint => {
                                        instruction = Instruction::new(Op::SDiv);
                                        instruction.set_type(type_id);
                                    }
                                    crate::ScalarKind::Float => {
                                        instruction = Instruction::new(Op::FDiv);
                                        instruction.set_type(type_id);
                                    }
                                    _ => unimplemented!("{:?}", kind),
                                }
                            }
                            _ => unimplemented!("{:?}", left_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        (id, left_inner)
                    }
                    crate::BinaryOperator::Equal => {
                        let mut instruction;
                        let id = self.generate_id();
                        // TODO Always assuming now that left and right are the same type
                        match left_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Uint => {
                                    instruction = Instruction::new(Op::IEqual);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Uint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::IEqual);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            crate::TypeInner::Vector { size, kind, width } => {
                                let inner = &crate::TypeInner::Scalar {
                                    kind: *kind,
                                    width: *width,
                                };
                                let type_id = self.get_type_by_inner(&ir_module.types, inner);

                                match kind {
                                    crate::ScalarKind::Uint | crate::ScalarKind::Sint => {
                                        instruction = Instruction::new(Op::IEqual);
                                        instruction.set_type(type_id);
                                    }
                                    _ => unimplemented!("{:?}", kind),
                                }
                            }
                            _ => unimplemented!("{:?}", left_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        (id, left_inner)
                    }
                    crate::BinaryOperator::Less => {
                        let mut instruction;
                        let id = self.generate_id();
                        // TODO Always assuming now that left and right are the same type
                        match left_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Uint => {
                                    instruction = Instruction::new(Op::ULessThan);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Uint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::SLessThan);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            crate::TypeInner::Vector { size, kind, width } => {
                                let inner = &crate::TypeInner::Scalar {
                                    kind: *kind,
                                    width: *width,
                                };
                                let type_id = self.get_type_by_inner(&ir_module.types, inner);

                                match kind {
                                    crate::ScalarKind::Uint => {
                                        instruction = Instruction::new(Op::ULessThan);
                                        instruction.set_type(type_id);
                                    }
                                    crate::ScalarKind::Sint => {
                                        instruction = Instruction::new(Op::SLessThan);
                                        instruction.set_type(type_id);
                                    }
                                    _ => unimplemented!("{:?}", kind),
                                }
                            }
                            _ => unimplemented!("{:?}", left_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        (id, left_inner)
                    }
                    crate::BinaryOperator::Greater => {
                        let mut instruction;
                        let id = self.generate_id();
                        // TODO Always assuming now that left and right are the same type
                        match left_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Uint => {
                                    instruction = Instruction::new(Op::UGreaterThan);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Uint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::SGreaterThan);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            crate::TypeInner::Vector { size, kind, width } => {
                                let inner = &crate::TypeInner::Scalar {
                                    kind: *kind,
                                    width: *width,
                                };
                                let type_id = self.get_type_by_inner(&ir_module.types, inner);

                                match kind {
                                    crate::ScalarKind::Uint => {
                                        instruction = Instruction::new(Op::UGreaterThan);
                                        instruction.set_type(type_id);
                                    }
                                    crate::ScalarKind::Sint => {
                                        instruction = Instruction::new(Op::SGreaterThan);
                                        instruction.set_type(type_id);
                                    }
                                    _ => unimplemented!("{:?}", kind),
                                }
                            }
                            _ => unimplemented!("{:?}", left_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        (id, left_inner)
                    }
                    crate::BinaryOperator::GreaterEqual => {
                        let mut instruction;
                        let id = self.generate_id();
                        // TODO Always assuming now that left and right are the same type
                        match left_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Uint => {
                                    instruction = Instruction::new(Op::UGreaterThanEqual);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Uint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::SGreaterThanEqual);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            crate::TypeInner::Vector { size, kind, width } => {
                                let inner = &crate::TypeInner::Scalar {
                                    kind: *kind,
                                    width: *width,
                                };
                                let type_id = self.get_type_by_inner(&ir_module.types, inner);

                                match kind {
                                    crate::ScalarKind::Uint => {
                                        instruction = Instruction::new(Op::UGreaterThanEqual);
                                        instruction.set_type(type_id);
                                    }
                                    crate::ScalarKind::Sint => {
                                        instruction = Instruction::new(Op::SGreaterThanEqual);
                                        instruction.set_type(type_id);
                                    }
                                    _ => unimplemented!("{:?}", kind),
                                }
                            }
                            _ => unimplemented!("{:?}", left_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(left_id);
                        instruction.add_operand(right_id);
                        (id, left_inner)
                    }
                    _ => unimplemented!("{:?}", op),
                }
            }
            crate::Expression::LocalVariable(variable) => {
                let id = self.generate_id();
                let var = &function.local_variables[*variable];

                let ty = &ir_module.types[var.ty];

                let pointer_id =
                    self.get_pointer_id(&ir_module.types, &var.ty, &StorageClass::Function);

                let mut instruction = Instruction::new(Op::Variable);
                instruction.set_type(pointer_id);
                instruction.set_result(id);
                instruction.add_operand(StorageClass::Function as u32);
                (id, &ty.inner)
            }
            crate::Expression::AccessIndex { base, index } => {
                self.parse_expression(ir_module, function, &function.expressions[*base], output)
            }
            crate::Expression::Access { base, index } => {
                self.parse_expression(ir_module, function, &function.expressions[*base], output)
            }
            crate::Expression::Call { name, arguments } => {
                let id = self.generate_id();
                match name.as_str() {
                    "atan2" | "sin" | "cos" | "normalize" | "length" => {
                        let mut instruction = Instruction::new(Op::ExtInst);
                        let inner = &crate::TypeInner::Scalar {
                            kind: crate::ScalarKind::Float,
                            width: 32,
                        };
                        let type_id = self.get_type_by_inner(&ir_module.types, inner);
                        instruction.set_type(type_id);
                        instruction.set_result(id);

                        // TODO Support other imports
                        //  There is always one key for now
                        for (k, _) in self.lookup_import.iter() {
                            instruction.add_operand(*k);
                        }

                        instruction.add_operands(self.string_to_words(name));

                        for arg in arguments {
                            let (id, _) = self.parse_expression(
                                ir_module,
                                function,
                                &function.expressions[*arg],
                                output,
                            );
                            instruction.add_operand(id);
                        }
                        output.push(instruction);
                        (id, inner)
                    }
                    "fclamp" => {
                        let mut instruction = Instruction::new(Op::ExtInst);
                        let inner = &crate::TypeInner::Scalar {
                            kind: crate::ScalarKind::Float,
                            width: 32,
                        };
                        let type_id = self.get_type_by_inner(&ir_module.types, inner);
                        instruction.set_type(type_id);
                        instruction.set_result(id);

                        // TODO Support other imports
                        //  There is always one key for now
                        for (k, _) in self.lookup_import.iter() {
                            instruction.add_operand(*k);
                        }

                        instruction.add_operands(self.string_to_words("clamp"));

                        for arg in arguments {
                            let (id, _) = self.parse_expression(
                                ir_module,
                                function,
                                &function.expressions[*arg],
                                output,
                            );
                            instruction.add_operand(id);
                        }
                        output.push(instruction);
                        (id, inner)
                    }
                    _ => unimplemented!(
                        "Function: {:?} with arguments: {:?} is not supported",
                        name,
                        arguments
                    ),
                }
            }
            crate::Expression::Unary { op, expr } => {
                let expression = &function.expressions[*expr];
                let (operand_id, operand_inner) =
                    self.parse_expression(ir_module, function, expression, output);

                match op {
                    crate::UnaryOperator::Negate => {
                        let mut instruction;
                        let id = self.generate_id();
                        match operand_inner {
                            crate::TypeInner::Scalar { kind, .. } => match kind {
                                crate::ScalarKind::Sint => {
                                    instruction = Instruction::new(Op::SNegate);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Sint,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                crate::ScalarKind::Float => {
                                    instruction = Instruction::new(Op::FNegate);
                                    let inner = &crate::TypeInner::Scalar {
                                        kind: crate::ScalarKind::Float,
                                        width: 32,
                                    };
                                    let type_id = self.get_type_by_inner(&ir_module.types, inner);
                                    instruction.set_type(type_id);
                                }
                                _ => unimplemented!("{:?}", kind),
                            },
                            _ => unimplemented!("{:?}", operand_inner),
                        }
                        instruction.set_result(id);
                        instruction.add_operand(operand_id);
                        (id, operand_inner)
                    }
                    _ => unimplemented!("{:?}", op),
                }
            }
            _ => unimplemented!("{:?}", expression),
        }
    }

    fn instruction_function_block(
        &mut self,
        ir_module: &crate::Module,
        function: &crate::Function,
        statement: &crate::Statement,
        output: &mut Vec<Instruction>,
    ) -> Instruction {
        match statement {
            crate::Statement::Return { value: _ } => match function.return_type {
                Some(_) => unimplemented!(),
                None => Instruction::new(Op::Return),
            },
            crate::Statement::Store { pointer, value } => {
                let mut instruction = Instruction::new(Op::Store);

                let pointer_expression = &function.expressions[*pointer];
                let value_expression = &function.expressions[*value];
                let (pointer_id, _) =
                    self.parse_expression(ir_module, function, pointer_expression, output);
                let (value_id, _) =
                    self.parse_expression(ir_module, function, value_expression, output);

                instruction.add_operand(pointer_id);
                instruction.add_operand(value_id);

                instruction
            }
            crate::Statement::If {
                condition,
                accept,
                reject,
            } => {
                // TODO
                Instruction::new(Op::Undef)
            }
            crate::Statement::Loop { body, continuing } => {
                // TODO
                Instruction::new(Op::Undef)
            }
            crate::Statement::Continue => {
                // TODO
                Instruction::new(Op::Undef)
            }
            crate::Statement::Empty => {
                // TODO
                Instruction::new(Op::Undef)
            }
            _ => unimplemented!("{:?}", statement),
        }
    }

    fn instruction_label(&mut self) -> Instruction {
        let id = self.generate_id();
        self.lookup_label.push(id);
        self.current_label = id;
        let mut instruction = Instruction::new(Op::Label);
        instruction.set_result(id);
        instruction
    }

    fn instruction_function_end(&self) -> Instruction {
        Instruction::new(Op::FunctionEnd)
    }

    fn write_logical_layout(&mut self, ir_module: &crate::Module) {
        for word in &self.instruction_ext_inst_import().to_words() {
            self.module.logical_layout.ext_inst_imports.push(*word);
        }

        if self.debug_enabled {
            self.debugs.push(self.instruction_source());
        }

        for (handle, function) in ir_module.functions.iter() {
            let mut function_instructions: Vec<Instruction> = vec![];
            function_instructions.push(self.instruction_function(
                handle,
                function,
                &ir_module.types,
            ));

            function_instructions.push(self.instruction_label());

            for block in function.body.iter() {
                let mut output: Vec<Instruction> = vec![];
                let instruction =
                    self.instruction_function_block(ir_module, function, &block, &mut output);
                function_instructions.append(&mut output);
                function_instructions.push(instruction);
            }

            function_instructions.push(self.instruction_function_end());

            for word in function_instructions.iter().flat_map(|f| f.to_words()) {
                self.module.logical_layout.function_definitions.push(word);
            }
        }

        for entry_point in ir_module.entry_points.iter() {
            let entry_point_instruction = self.instruction_entry_point(entry_point, ir_module);

            for word in &entry_point_instruction.to_words() {
                self.module.logical_layout.entry_points.push(*word)
            }
        }

        // Looking through all global variable, types, constants.
        // Doing this because we also want to include not used parts of the module
        // to be included in the output
        for (handle, _) in ir_module.global_variables.iter() {
            let _ =
                self.get_global_variable_id(&ir_module.types, &ir_module.global_variables, &handle);
        }

        for (handle, _) in ir_module.types.iter() {
            let _ = self.get_type_id(&ir_module.types, &handle);
        }

        for (handle, _) in ir_module.constants.iter() {
            let _ = self.get_constant_id(&handle, &ir_module);
        }

        for annotation in self.annotations.iter() {
            for word in &annotation.to_words() {
                self.module.logical_layout.annotations.push(*word);
            }
        }

        for capability in self.capabilities.iter() {
            let instruction = self.instruction_capability(capability);
            for words in instruction.to_words() {
                self.module.logical_layout.capabilities.push(words);
            }
        }

        for word in &self.instruction_memory_model().to_words() {
            self.module.logical_layout.memory_model.push(*word);
        }

        if self.debug_enabled {
            for debug in self.debugs.iter() {
                for word in &debug.to_words() {
                    self.module.logical_layout.debugs.push(*word);
                }
            }
        }
    }

    pub fn parse(&mut self, ir_module: &crate::Module) -> Vec<Word> {
        let mut words: Vec<Word> = vec![];

        self.write_logical_layout(ir_module);
        self.write_physical_layout();

        words.append(&mut self.module.physical_layout.in_words());
        words.append(&mut self.module.logical_layout.in_words());
        words
    }
}
