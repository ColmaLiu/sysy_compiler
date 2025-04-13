use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, Value};

#[derive(Default)]
pub struct Context {
    pub function: Option<Function>,
    pub returned: bool,
    pub block: Option<BasicBlock>,
    pub value: Option<Value>,
}

impl Context {
    pub fn new() -> Self {
        Default::default()
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
