use koopa::ir::{BasicBlock, Function, Value};

pub struct Context {
    pub function: Option<Function>,
    pub block: Option<BasicBlock>,
    pub value: Option<Value>,
}

impl Context {
    pub fn new() -> Context {
        Context { function: None, block: None, value: None }
    }
}
