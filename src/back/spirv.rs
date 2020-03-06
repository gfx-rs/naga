/*! Standard Portable Intermediate Representation (SPIR-V) backend !*/
use spirv::*;
use crate::{FastHashMap, BinaryOperator};

struct PhysicalLayout {
    magic_number: Word,
    version: Word,
    generator: Word,
    bound: Word,
    instruction_schema: Word,
}

impl PhysicalLayout {
    fn new(
        version: Word,
        generator: Word,
        instruction_schema: Word
    ) -> Self {
        PhysicalLayout {
            magic_number: MAGIC_NUMBER,
            version,
            generator,
            bound: 0,
            instruction_schema
        }
    }

    fn in_words(&self) -> Vec<Word> {
        let mut words: Vec<Word> = vec![];

        words.push(self.magic_number);
        words.push(self.version);
        words.push(self.generator);

        match self.bound {
            0 => panic!("Bound cannot be 0"),
            _ => words.push(self.bound),
        }

        words.push(self.instruction_schema);
        words
    }
}

struct LogicalLayout {
    capabilities: Vec<Word>,
    extensions: Vec<Word>,
    ext_inst_imports: Vec<Word>,
    memory_model: Vec<Word>,
    entry_points: Vec<Word>,
    execution_modes: Vec<Word>,
    debugs: Vec<Word>,
    annotations: Vec<Word>,
    type_declarations: Vec<Word>,
    constants: Vec<Word>,
    global_variables: Vec<Word>,
    function_declarations: Vec<Word>,
    function_definitions: Vec<Word>,
}

impl LogicalLayout {
    fn new() -> Self {
        LogicalLayout {
            capabilities: vec![],
            extensions: vec![],
            ext_inst_imports: vec![],
            memory_model: vec![],
            entry_points: vec![],
            execution_modes: vec![],
            debugs: vec![],
            annotations: vec![],
            type_declarations: vec![],
            constants: vec![],
            global_variables: vec![],
            function_declarations: vec![],
            function_definitions: vec![],
        }
    }

    fn in_words(
        &self,
    ) -> Vec<u32> {
        let mut words: Vec<Word> = vec![];

        for capability in self.capabilities.iter() {
            words.push(*capability);
        }

        for extension in self.extensions.iter() {
            words.push(*extension);
        }

        for ext_inst_import in self.ext_inst_imports.iter() {
            words.push(*ext_inst_import);
        }

        for memory_model in self.memory_model.iter() {
            words.push(*memory_model);
        }

        for entry_point in self.entry_points.iter() {
            words.push(*entry_point);
        }

        for execution_mode in self.execution_modes.iter() {
            words.push(*execution_mode);
        }

        for debug in self.debugs.iter() {
            words.push(*debug);
        }

        for annotation in self.annotations.iter() {
            words.push(*annotation);
        }

        for type_declaration in self.type_declarations.iter() {
            words.push(*type_declaration);
        }

        for constants in self.constants.iter() {
            words.push(*constants);
        }

        for global_variables in self.global_variables.iter() {
            words.push(*global_variables);
        }

        for function_declaration in self.function_declarations.iter() {
            words.push(*function_declaration);
        }

        for function_definition in self.function_definitions.iter() {
            words.push(*function_definition);
        }

        words
    }
}

struct Module {
    physical_layout: PhysicalLayout,
    logical_layout: LogicalLayout
}

impl Module {
    fn new(module: &crate::Module) -> Self {
        let version: Word = (0x0u32 << 24) |
            ((module.header.version.0 as u32) << 16) |
            ((module.header.version.1 as u32) << 8) |
            module.header.version.2 as u32;

        Module {
            physical_layout: PhysicalLayout::new(
                version,
                module.header.generator,
                0x0u32
            ),
            logical_layout: LogicalLayout::new(),
        }
    }
}

pub struct Instruction {
    op: u32,
    wc: u32,
    type_id: Option<Word>,
    result_id: Option<Word>,
    operands: Vec<Word>,
}

impl Instruction {
    fn new(
        op: Op,
    ) -> Self {
        Instruction {
            op: op as u32,
            wc: 1, // Always start at 1 for the first word (OP + WC),
            type_id: None,
            result_id: None,
            operands: vec![],
        }
    }

    fn set_type(&mut self, id: Word) {
        if self.type_id.is_none() {
            self.wc += 1;
        }
        self.type_id = Some(id);
    }

    fn set_result(&mut self, id: Word) {
        if self.result_id.is_none() {
            self.wc += 1;
        }
        self.result_id = Some(id);
    }

    fn add_operand(&mut self, operand: Word) {
        self.operands.push(operand);
        self.wc += 1;
    }

    fn add_operands(&mut self, operands: Vec<Word>) {
        for operand in operands {
            self.add_operand(operand);
        }
    }

    fn to_words(&self) -> Vec<Word> {
        let mut words = Vec::with_capacity(self.wc as usize);
        let wc_op = (self.wc << 16 | self.op ) as u32;

        words.push(wc_op);

        if self.type_id.is_some() {
            words.push(self.type_id.unwrap());
        }

        if self.result_id.is_some() {
            words.push(self.result_id.unwrap());
        }

        for operand in self.operands.iter() {
            words.push(*operand);
        }

        words
    }
}

pub struct Parser {
    module: Module,
    id_count: u32,
    capabilities: Vec<Capability>,
    debugs: Vec<Instruction>,
    annotations: Vec<Instruction>,

    scalar_types_lookup: FastHashMap<u8, u32>,
    void_type: Option<u32>,
    debug_enabled: bool,
    lookup_type: FastHashMap<Word, crate::Handle<crate::Type>>,
    lookup_function: FastHashMap<Word, crate::Handle<crate::Function>>,
    lookup_constant: FastHashMap<Word, crate::Handle<crate::Constant>>,
    lookup_global_variable: FastHashMap<Word, crate::Handle<crate::GlobalVariable>>,
    lookup_pointer: FastHashMap<Word, Word>,
}

impl Parser {
    pub fn new(
        module: &crate::Module,
        debug_enabled: bool
    ) -> Self {
        Parser {
            module: Module::new(module),
            id_count: 0,
            capabilities: vec![],
            debugs: vec![],
            annotations: vec![],

            scalar_types_lookup: FastHashMap::default(),
            void_type: None,
            debug_enabled,

            lookup_type: FastHashMap::default(),
            lookup_function: FastHashMap::default(),
            lookup_constant: FastHashMap::default(),
            lookup_global_variable: FastHashMap::default(),
            lookup_pointer: FastHashMap::default(),
        }
    }

    fn generate_id(
        &mut self,
    ) -> u32 {
         self.id_count += 1;
        self.id_count
    }

    fn bytes_to_words(
        &self,
        bytes: &[u8],
    ) -> Vec<Word> {
        let words: Vec<Word> = bytes
            .chunks(4)
            .map(|chars|
                match chars.len() {
                    4 => (chars[3] as u32) << 24 |
                        (chars[2] as u32) << 16 |
                        (chars[1] as u32) << 8 |
                        chars[0] as u32,
                    3 => 0x0u32 << 24 |
                        (chars[2] as u32) << 16 |
                        (chars[1] as u32) << 8 |
                        (chars[0] as u32) as u32,
                    2 => 0x0u32 << 24 |
                        0x0u32 << 16 |
                        (chars[1] as u32) << 8 |
                        chars[0] as u32,
                    1 => 0x0u32 << 24 |
                        0x0u32 << 16 |
                        0x0u32 << 8 |
                        chars[0] as u32,
                    _ => 0x0u32
                }
            )
            .collect();

        words
    }

    fn string_to_words(
        &self,
        input: &str
    ) -> Vec<Word> {
        let mut words: Vec<Word>
            = self.bytes_to_words(input.as_bytes());

        let last_word = words.last().unwrap();

        let last_character = last_word.to_le_bytes()[3];

        if last_character != 0x0 {
            // nul-termination
            words.push(0x0u32);
        }

        words
    }

    fn instruction_capability(
        &self,
        capability: &Capability
    ) -> Instruction {
        let mut instruction = Instruction::new(Op::Capability);
        instruction.add_operand(*capability as u32);
        instruction
    }

    fn instruction_ext_inst_import(&mut self) -> Instruction {
        let mut instruction= Instruction::new(Op::ExtInstImport);
        let id = self.generate_id();
        instruction.set_result(id);
        instruction.add_operands(self.string_to_words("GLSL.std.450"));
        instruction
    }

    fn instruction_memory_model(&self) -> Instruction {
        let mut instruction= Instruction::new(Op::MemoryModel);
        instruction.add_operand(AddressingModel::Logical as u32);
        instruction.add_operand(MemoryModel::GLSL450 as u32);
        instruction
    }

    fn instruction_entry_point(
        &mut self,
        entry_point: &crate::EntryPoint,
        ir_module: &crate::Module,
    ) -> Instruction {
        let mut instruction = Instruction::new(Op::EntryPoint);
        let function_id = self.lookup_function(entry_point.function);

        instruction.add_operand(entry_point.exec_model as u32);
        instruction.add_operand(function_id);

        if self.debug_enabled {
            let mut debug_instruction
                = Instruction::new(Op::Name);
            debug_instruction.set_result(function_id);
            debug_instruction.add_operands(
                self.string_to_words(entry_point.name.as_str()));
            self.debugs.push(debug_instruction);
        }

        instruction.add_operands(
            self.string_to_words(entry_point.name.as_str()));

        let function = &ir_module.functions[entry_point.function];
        for ((handle, _), &usage) in ir_module.global_variables.iter().zip(&function.global_usage) {
            if usage.contains(crate::GlobalUse::STORE) || usage.contains(crate::GlobalUse::LOAD) {
                let id = self.get_global_variable_id(
                    &ir_module.types,
                    &ir_module.global_variables,
                    &handle);
                instruction.add_operand(id);
            }
        }


        // TODO Always adding the same ExecutionMode, need to be researched how this works
        let mut execution_mode_instruction
            = Instruction::new(Op::ExecutionMode);
        execution_mode_instruction.add_operand(function_id);
        execution_mode_instruction.add_operand(ExecutionMode::OriginUpperLeft as u32);
        for word in execution_mode_instruction.to_words() {
            self.module.logical_layout.execution_modes.push(word)
        }

        instruction
    }

    fn lookup_function(
        &mut self,
        handle: crate::Handle<crate::Function>,
    ) -> Word {
        let mut word = 0;
        for (k, v) in self.lookup_function.iter() {
            if v.eq(&handle) {
                word = *k;
                break;
            }
        }

        // TODO error handling
        word
    }

    fn lookup_pointer(
        &mut self,
        type_id: Word,
    ) -> Option<Word> {
        let mut word = None;
        for (k, v) in self.lookup_pointer.iter() {
            if k.eq(&type_id) {
                word = Some(*v);
                break;
            }
        }
        word
    }

    fn get_pointer(
        &mut self,
        type_id: Word,
        class: u32,
    ) -> Word {
        match self.lookup_pointer(type_id) {
            Some(word) => word,
            None => {
                let mut instruction
                    = Instruction::new(Op::TypePointer);
                let id = self.generate_id();
                instruction.set_result(id);
                instruction.add_operand(class);
                instruction.add_operand(type_id);

                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }

                id
            }
        }
    }

    fn lookup_scalar_type(
        &mut self,
        kind: u8,
    ) -> Option<Word> {
        let mut word = None;

        for (k, v) in self.scalar_types_lookup.iter() {
            if k.eq(&kind) {
                word = Some(*v);
                break;
            }
        }

        word
    }

    fn get_scalar_type(
        &mut self,
        kind: &crate::ScalarKind,
        width: u8,
    ) -> Word {
        let kind_u8: u8 = unsafe {
            std::mem::transmute(*kind)
        };

        match self.lookup_scalar_type(kind_u8) {
            Some(word) => word,
            None => {
                let (instruction, id) = self.instruction_scalar_type_declaration(kind, width);

                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }

                self.scalar_types_lookup.insert(kind_u8, id);

                id
            }
        }
    }

    fn lookup_type(
        &mut self,
        handle: &crate::Handle<crate::Type>,
        ty: &crate::Type,
    ) -> Option<Word> {
        match ty.inner {
            crate::TypeInner::Scalar { kind, .. } => {
                let kind_u8: u8 = unsafe {
                    std::mem::transmute(kind)
                };

                self.lookup_scalar_type(kind_u8)
            }
            _ => {
                let mut word = None;
                for (k, v) in self.lookup_type.iter() {
                    if v.eq(&handle) {
                        word = Some(*k);
                        break;
                    }
                }

                word
            }
        }
    }

    fn get_type_id(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        handle: &crate::Handle<crate::Type>,
    ) -> Word {
        let ty = &arena[*handle];

        match self.lookup_type(handle, ty) {
            Some(word) => word,
            None => {
                let id = self.instruction_type_declaration(arena, *handle);
                id
            }
        }
    }

    fn lookup_constant(
        &mut self,
        handle: &crate::Handle<crate::Constant>,
    ) -> Option<Word> {
        let mut word = None;

        for (k, v) in self.lookup_constant.iter() {
            if v.eq(&handle) {
                word = Some(*k);
                break;
            }
        };

        word
    }

    fn get_constant_id(
        &mut self,
        handle: &crate::Handle<crate::Constant>,
        ir_module: &crate::Module,
    ) -> Word {
        match self.lookup_constant(handle) {
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

    fn lookup_global_variable(
        &mut self,
        handle: &crate::Handle<crate::GlobalVariable>,
    ) -> Option<Word> {
        let mut word = None;
        for (k, v) in self.lookup_global_variable.iter() {
            if v.eq(&handle) {
                word = Some(*k);
                break;
            }
        };

        word
    }

    fn get_global_variable_id(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        global_arena: &crate::Arena<crate::GlobalVariable>,
        handle: &crate::Handle<crate::GlobalVariable>,
    ) -> Word {
        match self.lookup_global_variable(handle) {
            Some(word) => word,
            None => {
                let global_variable = &global_arena[*handle];
                let (instruction, id) = self.instruction_global_variable(arena, global_variable, handle);

                for words in instruction.to_words() {
                    self.module.logical_layout.global_variables.push(words);
                }

                id
            }
        }
    }

    fn instruction_scalar_type_declaration(
        &mut self,
        kind: &crate::ScalarKind,
        width: u8,
    ) -> (Instruction, Word) {
        let id = self.generate_id();
        match kind {
            crate::ScalarKind::Sint => {
                let mut instruction
                    = Instruction::new(Op::TypeInt);
                instruction.set_result(id);
                instruction.add_operand(width as u32);
                instruction.add_operand(0x1u32);
                (instruction, id)
            }
            crate::ScalarKind::Uint => {
                let mut instruction
                    = Instruction::new(Op::TypeInt);
                instruction.set_result(id);
                instruction.add_operand(width as u32);
                instruction.add_operand(0x0u32);
                (instruction, id)
            }
            crate::ScalarKind::Float => {
                let mut instruction
                    = Instruction::new(Op::TypeFloat);
                instruction.set_result(id);
                instruction.add_operand(width as u32);
                (instruction, id)
            }
            crate::ScalarKind::Bool => {
                let mut instruction
                    = Instruction::new(Op::TypeBool);
                instruction.set_result(id);
                (instruction, id)
            }
        }
    }

    fn instruction_type_declaration(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        handle: crate::Handle<crate::Type>,
    ) -> Word {
        let ty = &arena[handle];


        match ty.inner {
            crate::TypeInner::Scalar { kind, width } => {
                let (instruction, id) = self.instruction_scalar_type_declaration(&kind, width);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }

                id
            }
            crate::TypeInner::Vector { size, kind, width } => {
                let id = self.generate_id();
                let kind_id = self.get_scalar_type(&kind, width);

                let mut instruction
                    = Instruction::new(Op::TypeVector);
                instruction.set_result(id);
                instruction.add_operand(kind_id);
                instruction.add_operand(size as u32);

                self.lookup_type.insert(id, handle);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }
                id
            }
            crate::TypeInner::Matrix { columns, rows: _, kind, width } => {
                let id = self.generate_id();
                let kind_id = self.get_scalar_type(&kind, width);

                let mut instruction
                    = Instruction::new(Op::TypeMatrix);
                instruction.set_result(id);
                instruction.add_operand(kind_id);
                instruction.add_operand(columns as u32);

                self.lookup_type.insert(id, handle);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }
                id
            }
            crate::TypeInner::Pointer { base, class } => {
                let type_id = self.get_type_id(arena, &base);
                let id = self.get_pointer(type_id, class as u32);
                id
            }
            crate::TypeInner::Array { base, size } => {
                let id = self.generate_id();
                let type_id = self.get_type_id(arena, &handle);

                let mut instruction
                    = Instruction::new(Op::TypeArray);
                instruction.set_result(id);
                instruction.add_operand(type_id);

                match size {
                    crate::ArraySize::Static(word) => {
                        instruction.add_operand(word);
                    }
                    _ => panic!("Array size {:?} unsupported", size)
                }

                self.lookup_type.insert(id, base);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }
                id
            }
            crate::TypeInner::Struct { ref members } => {
                let id = self.generate_id();
                let mut instruction
                    = Instruction::new(Op::TypeStruct);
                instruction.set_result(id);

                for member in members {
                    let type_id
                        = self.get_type_id(arena, &member.ty);
                    instruction.add_operand(type_id);
                }

                self.lookup_type.insert(id, handle);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }
                id
            }
            crate::TypeInner::Image { base, dim, flags } => {
                let id = self.generate_id();
                let type_id = self.get_type_id(arena, &base);

                let mut instruction
                    = Instruction::new(Op::TypeImage);
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
                    _ => false
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

                if flags.contains(crate::ImageFlags::CAN_STORE) &&
                    flags.contains(crate::ImageFlags::CAN_LOAD) {
                    instruction.add_operand(2);
                } else {
                    if flags.contains(crate::ImageFlags::CAN_STORE) {
                        instruction.add_operand(1);
                    } else if flags.contains(crate::ImageFlags::CAN_LOAD) {
                        instruction.add_operand(0);
                    }
                }

                self.lookup_type.insert(id, base);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }
                id
            }
            crate::TypeInner::Sampler => {
                let id = self.generate_id();
                let mut instruction
                    = Instruction::new(Op::TypeSampler);
                instruction.set_result(id);

                self.lookup_type.insert(id, handle);
                for words in instruction.to_words() {
                    self.module.logical_layout.type_declarations.push(words);
                }
                id
            }
        }
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

                let mut instruction
                    = Instruction::new(Op::Constant);
                instruction.set_type(type_id);
                instruction.set_result(id);

                let ty = &ir_module.types[constant.ty];
                match ty.inner {
                    crate::TypeInner::Scalar {kind: _, width} => {
                        match width {
                            32 =>  {
                                instruction.add_operand(val as u32);
                            }
                            64 => {
                                let (low, high) = ((val >> 32) as u32, val as u32);
                                instruction.add_operand(low);
                                instruction.add_operand(high);
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                }

                (instruction, id)
            }
            crate::ConstantInner::Uint(val) => {
                let type_id = self.get_type_id(arena, &constant.ty);

                let mut instruction
                    = Instruction::new(Op::Constant);
                instruction.set_type(type_id);
                instruction.set_result(id);

                let ty = &ir_module.types[constant.ty];
                match ty.inner {
                    crate::TypeInner::Scalar {kind: _, width} => {
                        match width {
                            32 =>  {
                                instruction.add_operand(val as u32);
                            }
                            64 => {
                                let (low, high) = ((val >> 32) as u32, val as u32);
                                instruction.add_operand(low);
                                instruction.add_operand(high);
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
                }

                (instruction, id)
            }
            crate::ConstantInner::Float(val) => {
                let type_id = self.get_type_id(arena, &constant.ty);

                let mut instruction
                    = Instruction::new(Op::Constant);
                instruction.set_type(type_id);
                instruction.set_result(id);

                let ty = &ir_module.types[constant.ty];
                match ty.inner {
                    crate::TypeInner::Scalar {kind: _, width} => {
                        match width {
                            32 =>  {
                                instruction.add_operand((val as f32).to_bits());
                            }
                            64 => {
                                let bits = f64::to_bits(val);
                                let (low, high) = ((bits >> 32) as u32, bits as u32);
                                instruction.add_operand(low);
                                instruction.add_operand(high);
                            }
                            _ => unreachable!()
                        }
                    }
                    _ => unreachable!()
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

                let mut instruction
                    = Instruction::new(Op::ConstantComposite);
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

    fn instruction_global_variable(
        &mut self,
        arena: &crate::Arena<crate::Type>,
        global_variable: &crate::GlobalVariable,
        handle: &crate::Handle<crate::GlobalVariable>,
    ) -> (Instruction, Word) {
        let mut instruction
            = Instruction::new(Op::Variable);
        let id = self.generate_id();
        let type_id = self.get_type_id(arena, &global_variable.ty);
        let pointer_id = self.get_pointer(type_id, global_variable.class as u32);

        instruction.set_type(pointer_id);
        instruction.set_result(id);
        instruction.add_operand(global_variable.class as u32);

        if self.debug_enabled {
            let mut debug_instruction
                = Instruction::new(Op::Name);
            debug_instruction.set_result(id);
            debug_instruction.add_operands(
                self.string_to_words(
                    global_variable.name.as_ref().unwrap().as_str()));
            self.debugs.push(debug_instruction);
        }

        match global_variable.binding.as_ref().unwrap() {
            crate::Binding::Location(location) => {
                let mut instruction
                    = Instruction::new(Op::Decorate);
                instruction.add_operand(id);
                instruction.add_operand(Decoration::Location as u32);
                instruction.add_operand(*location);
                self.annotations.push(instruction);
            }
            crate::Binding::Descriptor {set, binding} => {
                let mut set_instruction
                    = Instruction::new(Op::Decorate);
                set_instruction.add_operand(id);
                set_instruction.add_operand(Decoration::DescriptorSet as u32);
                set_instruction.add_operand(*set);
                self.annotations.push(set_instruction);

                let mut binding_instruction
                    = Instruction::new(Op::Decorate);
                binding_instruction.add_operand(id);
                binding_instruction.add_operand(Decoration::Binding as u32);
                binding_instruction.add_operand(*binding);
                self.annotations.push(binding_instruction);
            }
            crate::Binding::BuiltIn(_) => {
                // TODO
                panic!("unsupported")
            }
        }

        // TODO Initializer is optional and not (yet) included in the IR

        self.lookup_global_variable.insert(id, *handle);
        (instruction, id)
    }

    fn write_physical_layout(&mut self) {
        self.module.physical_layout.bound = self.id_count + 1;
    }

    fn instruction_source(
        &self,
    ) -> Instruction {
        let version = 450u32;

        let mut instruction
            = Instruction::new(Op::Source);
        instruction.add_operand(SourceLanguage::GLSL as u32);
        instruction.add_operands(
            self.bytes_to_words(&version.to_le_bytes()));
        instruction
    }

    fn get_or_insert_function_type_id(
        &mut self,
        ty: Option<crate::Handle<crate::Type>>,
        arena: &crate::Arena<crate::Type>,
    ) -> Word {
        match ty {
            Some(handle) => {
                self.get_type_id(arena, &handle)
            }
            None => {
                match self.void_type {
                    Some(id) => id,
                    None => {
                        let id = self.generate_id();

                        let mut instruction
                            = Instruction::new(Op::TypeVoid);
                        instruction.set_result(id);

                        self.void_type = Some(id);
                        for word in instruction.to_words().iter() {
                            self.module.logical_layout.type_declarations.push(*word);
                        }
                        id
                    }
                }
            }
        }
    }

    fn instruction_type_function(
        &mut self,
        return_type_id: Word,
    ) -> (Instruction, Word) {
        let id = self.generate_id();

        let mut instruction
            = Instruction::new(Op::TypeFunction);
        instruction.set_result(id);
        instruction.add_operand(return_type_id);

        // TODO Add support for parameters

        (instruction, id)
    }

    fn instruction_function(
        &mut self,
        handle: crate::Handle<crate::Function>,
        function: &crate::Function,
        arena: &crate::Arena<crate::Type>,
    ) -> Instruction {
        let id = self.generate_id();
        let return_type_id
            = self.get_or_insert_function_type_id(function.return_type, arena);

        let mut instruction
            = Instruction::new(Op::Function);
        instruction.set_type(return_type_id);
        instruction.set_result(id);

        let control_u32: Word = unsafe {
            std::mem::transmute(function.control)
        };

        instruction.add_operand(control_u32);

        let (type_function_instruction, type_function_id)
            = self.instruction_type_function(return_type_id);

        instruction.add_operand(type_function_id);

        self.lookup_function.insert(id, handle);
        for word in type_function_instruction.to_words() {
            self.module.logical_layout.type_declarations.push(word);
        }

        instruction
    }

    fn parse_expression(
        &mut self,
        ir_module: &crate::Module,
        function: &crate::Function,
        expression: &crate::Expression,
        output: &mut Vec<Instruction>,
    ) -> Word {
        match expression {
            crate::Expression::GlobalVariable(handle) => {
                self.get_global_variable_id(&ir_module.types, &ir_module.global_variables, handle)
            }
            crate::Expression::Constant(handle) => {
                self.get_constant_id(handle, ir_module)
            }
            crate::Expression::Compose { ty, components } => {
                let id = self.generate_id();
                let type_id = self.get_type_id( &ir_module.types, &ty);

                let mut instruction
                    = Instruction::new(Op::CompositeConstruct);
                instruction.set_type(type_id);
                instruction.set_result(id);

                for component in components {
                    let expression = &function.expressions[*component];
                    let component_id = self.parse_expression(ir_module,&function, expression, output);
                    instruction.add_operand(component_id);
                }

                output.push(instruction);

                id
            }
            crate::Expression::Binary {op, left, right} => {
                let expression_left = &function.expressions[*left];
                let expression_right = &function.expressions[*right];

                match op {
                    crate::BinaryOperator::Multiply => {
                        // retrieve type handles
                        // check what one is the composite
                        // match composite, only support vector now
                        // match other expression, only support scalar types for now
                        // create instruction
                        // return id
                    },
                    _ => panic!("Unsupported binary op {:?}", op),
                }
                1
            }
            _ => unimplemented!()
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
            crate::Statement::Return { value: _ } => {
                match function.return_type {
                    Some(_) => unimplemented!(),
                    None => Instruction::new(Op::Return)
                }
            }
            crate::Statement::Store { pointer, value } => {
                let mut instruction
                    = Instruction::new(Op::Store);

                let pointer_expression = &function.expressions[*pointer];
                let value_expression = &function.expressions[*value];
                let pointer_id = self.parse_expression(ir_module, function, pointer_expression, output);
                let value_id = self.parse_expression(ir_module, function, value_expression, output);

                instruction.add_operand(pointer_id);
                instruction.add_operand(value_id);

                instruction
            }
            _ => unimplemented!()
        }
    }

    fn instruction_label(
        &mut self,
    ) -> Instruction {
        let mut instruction = Instruction::new(Op::Label);
        instruction.set_result(self.generate_id());
        instruction
    }

    fn instruction_function_end(
        &self,
    ) -> Instruction {
        Instruction::new(Op::FunctionEnd)
    }

    fn write_logical_layout(
        &mut self,
        ir_module: &crate::Module,
    ) {
        println!("{:#?}", ir_module);

        for word in &self.instruction_ext_inst_import().to_words() {
            self.module.logical_layout.ext_inst_imports.push(*word);
        }

        if self.debug_enabled {
            self.debugs.push(self.instruction_source());
        }

        for (handle, function) in ir_module.functions.iter() {
            let mut function_instructions: Vec<Instruction> = vec![];
            function_instructions.push(
                self.instruction_function(handle, function, &ir_module.types));

            function_instructions.push(
                self.instruction_label()
            );

            for block in function.body.iter() {
                let mut output: Vec<Instruction> = vec![];
                let instruction = self.instruction_function_block(
                    ir_module,
                    function,
                    &block,
                    &mut output);
                function_instructions.append(&mut output);
                function_instructions.push(instruction);
            }

            function_instructions.push(self.instruction_function_end());

            for word in function_instructions
                .iter()
                .flat_map(|f| f.to_words()) {
                self.module.logical_layout.function_definitions.push(word);
            }
        }

        for entry_point in ir_module.entry_points.iter() {
            let entry_point_instruction
                = self.instruction_entry_point(entry_point, ir_module);

            for word in &entry_point_instruction.to_words() {
                self.module.logical_layout.entry_points.push(*word)
            }
        }

        // Looking through all global variable, types, constants.
        // Doing this because we also want to include not used parts of the module
        // to be included in the output
        for (handle, _) in ir_module.global_variables.iter() {
            let _ = self.get_global_variable_id(
                &ir_module.types,
                &ir_module.global_variables, &handle);
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

        // TODO Add support for other capabilities
        let mut capability_instruction
            = Instruction::new(Op::Capability);
        capability_instruction.add_operand(Capability::Shader as u32);

        for words in capability_instruction.to_words() {
            self.module.logical_layout.capabilities.push(words);
        }

        for word in &self.instruction_memory_model().to_words() {
            self.module.logical_layout.memory_model.push(*word);
        }

        if self.debug_enabled {
            for debug in self.debugs.iter() {
                for word in &debug.to_words() {
                    self.module.logical_layout.debugs.push(*word);
                };
            }
        }
    }

    pub fn parse(
        &mut self,
        ir_module: &crate::Module,
    ) -> Vec<Word> {
        let mut words: Vec<Word> = vec![];

        self.write_logical_layout(ir_module);
        self.write_physical_layout();

        words.append(&mut self.module.physical_layout.in_words());
        words.append(&mut self.module.logical_layout.in_words());
        words
    }
}
