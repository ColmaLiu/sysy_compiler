use std::{collections::HashMap, iter::zip};

use koopa::ir::{BasicBlock, Function, Value};

#[derive(Default)]
pub struct Context {
    pub function: Option<Function>,
    pub block: Option<BasicBlock>,
    pub value: Option<Value>,
    pub exited: bool,
}

pub enum Symbol {
    Const(String, i32),
    Var(String, Value),
    Func(String, Function),
}

#[derive(Default)]
pub struct SymbolTable {
    symbol_table_stack: Vec<HashMap<String, Symbol>>,
    is_entry: bool,
}

pub struct WhileBlockInfo {
    entry_bb: BasicBlock,
    end_bb: BasicBlock,
}

#[derive(Default)]
pub struct IrInfo {
    pub context: Context,
    pub symbol_table: SymbolTable,
    pub if_cnt: usize,
    pub while_cnt: usize,
    pub while_info: Vec<WhileBlockInfo>,
}

impl SymbolTable {
    pub fn depth(&self) -> usize {
        self.symbol_table_stack.len() - 1
    }

    pub fn insert(&mut self, ident: String, symbol: Symbol) -> Option<Symbol> {
        let symbol_table = self.symbol_table_stack.last_mut().unwrap();
        symbol_table.insert(ident, symbol)
    }

    pub fn get(&self, ident: &String) -> Option<(&Symbol, bool)> {
        for (symbol_table, depth) in zip(self.symbol_table_stack.iter(), 0..self.symbol_table_stack.len()).rev() {
            let symbol = symbol_table.get(ident);
            if symbol.is_some() {
                return symbol.map(|symbol| (symbol, depth == 0));
            }
        }
        None
    }

    pub fn set_entry(&mut self) {
        self.is_entry = true;
    }

    pub fn check_entry(&mut self) -> bool {
        if self.is_entry {
            self.is_entry = false;
            true
        } else {
            false
        }
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
