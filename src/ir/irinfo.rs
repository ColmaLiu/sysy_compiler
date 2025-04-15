use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, Value};

#[derive(Default)]
pub struct Context {
    pub function: Option<Function>,
    pub returned: bool,
    pub block: Option<BasicBlock>,
    pub value: Option<Value>,
}

pub enum Symbol {
    Const(String, i32),
    Var(String, Value),
}

#[derive(Default)]
pub struct SymbolTableStack {
    symbol_table_stack: Vec<HashMap<String, Symbol>>,
}

#[derive(Default)]
pub struct IrInfo {
    pub context: Context,
    pub symbol_table: SymbolTableStack,
}

impl SymbolTableStack {
    pub fn depth(&self) -> usize {
        self.symbol_table_stack.len()
    }

    pub fn insert(&mut self, ident: String, symbol: Symbol) -> Option<Symbol> {
        let symbol_table = self.symbol_table_stack.last_mut().unwrap();
        symbol_table.insert(ident, symbol)
    }

    pub fn get(&self, ident: &String) -> Option<&Symbol> {
        for symbol_table in self.symbol_table_stack.iter().rev() {
            let symbol = symbol_table.get(ident);
            if symbol.is_some() {
                return symbol;
            }
        }
        None
    }

    pub fn push_table(&mut self) {
        self.symbol_table_stack.push(HashMap::new());
    }

    pub fn pop_table(&mut self) {
        self.symbol_table_stack.pop();
    }
}

impl IrInfo {
    pub fn new() -> Self {
        Default::default()
    }
}
