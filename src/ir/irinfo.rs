use std::collections::HashMap;

use koopa::ir::{BasicBlock, Function, Value};

#[derive(Default)]
pub struct Context {
    pub function: Option<Function>,
    pub block: Option<BasicBlock>,
    pub exited: bool,
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

pub struct WhileBlockInfo {
    entry_bb: BasicBlock,
    end_bb: BasicBlock,
}

#[derive(Default)]
pub struct IrInfo {
    pub context: Context,
    pub symbol_table: SymbolTableStack,
    pub if_cnt: usize,
    pub while_cnt: usize,
    pub while_info: Vec<WhileBlockInfo>,
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

impl WhileBlockInfo {
    pub fn new(entry_bb: BasicBlock, end_bb: BasicBlock) -> Self {
        WhileBlockInfo { entry_bb, end_bb }
    }

    pub fn entry_bb(&self) -> BasicBlock {
        self.entry_bb
    }

    pub fn end_bb(&self) -> BasicBlock {
        self.end_bb
    }
}

impl IrInfo {
    pub fn new() -> Self {
        Default::default()
    }
}
