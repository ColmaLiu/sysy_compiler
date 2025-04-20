use std::collections::HashMap;

use crate::asm::TEMP_IDX;
use koopa::ir::{Function, Value};

#[derive(Default)]
pub struct AsmInfo {
    pub reg: [Option<Value>; 32],
    pub stack: HashMap<Value, usize>,
    pub function: HashMap<Function, String>,
    pub glob_var: HashMap<Value, String>,
    pub ra_used: bool,
}

impl AsmInfo {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get_vacant(&self) -> Result<usize, String> {
        for idx in TEMP_IDX {
            if self.reg[idx] == None {
                return Ok(idx);
            }
        }
        Err("No vacant register".to_string())
    }

    pub fn get_occupied(&self, value: Value) -> Result<usize, String> {
        for idx in TEMP_IDX {
            if let Some(reg_value) = self.reg[idx] {
                if reg_value == value {
                    return Ok(idx);
                }
            }
        }
        Err("No register is occupied by the value".to_string())
    }

    pub fn set_reg(&mut self, value: Value) -> Result<usize, String> {
        for idx in TEMP_IDX {
            if self.reg[idx] == None {
                self.reg[idx] = Some(value);
                return Ok(idx);
            }
        }
        Err("No vacant register".to_string())
    }

    pub fn free_reg(&mut self, reg_idx: usize) {
        self.reg[reg_idx] = None;
    }
}
