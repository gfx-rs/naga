use crate::{
    Arena, Constant, Expression, FastHashMap, Function, GlobalVariable, Handle, LocalVariable,
    ShaderStage, Type,
};

#[derive(Debug)]
pub struct Program {
    pub version: u16,
    pub profile: Profile,
    pub shader_stage: ShaderStage,
    pub lookup_function: FastHashMap<String, Handle<Function>>,
    pub functions: Arena<Function>,
    pub lookup_type: FastHashMap<String, Handle<Type>>,
    pub types: Arena<Type>,
    pub constants: Arena<Constant>,
    pub global_variables: Arena<GlobalVariable>,
    pub context: Option<Context>,
}

impl Program {
    pub fn new(shader_stage: ShaderStage) -> Program {
        Program {
            version: 0,
            profile: Profile::Core,
            shader_stage,
            lookup_function: FastHashMap::default(),
            functions: Arena::<Function>::new(),
            lookup_type: FastHashMap::default(),
            types: Arena::<Type>::new(),
            constants: Arena::<Constant>::new(),
            global_variables: Arena::<GlobalVariable>::new(),
            context: None,
        }
    }

    pub fn get_context(&mut self) -> &mut Context {
        match self.context {
            Some(ref mut c) => c,
            None => {
                self.context = Some(Context {
                    expressions: Arena::<Expression>::new(),
                    local_variables: Arena::<LocalVariable>::new(),
                });
                self.context.as_mut().unwrap()
            }
        }
    }

    pub fn take_context(&mut self) -> Option<Context> {
        self.context.take()
    }
}

#[derive(Debug)]
pub enum Profile {
    Core,
}

#[derive(Debug)]
pub struct Context {
    pub expressions: Arena<Expression>,
    pub local_variables: Arena<LocalVariable>,
}
