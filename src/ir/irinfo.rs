use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, Value};

pub struct Context {
    pub function: Option<Function>,
    pub block: Option<BasicBlock>,
    pub value: Option<Value>,
}

impl Context {
    pub fn new() -> Self {
        Context { function: None, block: None, value: None }
    }
}

pub enum Symbol {
    Const(String, i32),
    Var(String, Value),
}

pub struct IrInfo {
    pub context: Context,
    pub symble_table: HashMap<String, Symbol>,
}

impl IrInfo {
    pub fn new() -> Self {
        IrInfo { context: Context::new(), symble_table: HashMap::new() }
    }
}
