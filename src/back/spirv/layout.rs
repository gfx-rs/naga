use spirv::*;

pub(crate) struct Module {
    pub(crate) physical_layout: PhysicalLayout,
    pub(crate) logical_layout: LogicalLayout
}

impl Module {
    pub(crate) fn new(module: &crate::Module) -> Self {
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

pub(crate) struct PhysicalLayout {
    magic_number: Word,
    version: Word,
    generator: Word,
    pub(crate) bound: Word,
    instruction_schema: Word,
}

impl PhysicalLayout {
    pub(crate) fn new(
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

    pub(crate) fn in_words(&self) -> Vec<Word> {
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

pub(crate) struct LogicalLayout {
    pub(crate) capabilities: Vec<Word>,
    extensions: Vec<Word>,
    pub(crate) ext_inst_imports: Vec<Word>,
    pub(crate) memory_model: Vec<Word>,
    pub(crate) entry_points: Vec<Word>,
    pub(crate) execution_modes: Vec<Word>,
    pub(crate) debugs: Vec<Word>,
    pub(crate) annotations: Vec<Word>,
    pub(crate) type_declarations: Vec<Word>,
    pub(crate) constants: Vec<Word>,
    pub(crate) global_variables: Vec<Word>,
    function_declarations: Vec<Word>,
    pub(crate) function_definitions: Vec<Word>,
}

impl LogicalLayout {
    pub(crate) fn new() -> Self {
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

    pub(crate) fn in_words(
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

pub(crate) struct Instruction {
    op: u32,
    wc: u32,
    type_id: Option<Word>,
    result_id: Option<Word>,
    operands: Vec<Word>,
}

impl Instruction {
    pub(crate) fn new(
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

    pub(crate) fn set_type(&mut self, id: Word) {
        if self.type_id.is_none() {
            self.wc += 1;
        }
        self.type_id = Some(id);
    }

    pub(crate) fn set_result(&mut self, id: Word) {
        if self.result_id.is_none() {
            self.wc += 1;
        }
        self.result_id = Some(id);
    }

    pub(crate) fn add_operand(&mut self, operand: Word) {
        self.operands.push(operand);
        self.wc += 1;
    }

    pub(crate) fn add_operands(&mut self, operands: Vec<Word>) {
        for operand in operands {
            self.add_operand(operand);
        }
    }

    pub(crate) fn to_words(&self) -> Vec<Word> {
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
