use koopa::ir::{FunctionData, Program, ValueKind};

pub trait GenerateAsm {
    fn generate_asm(&self) -> Result<Vec<String>, String>;
}

impl GenerateAsm for Program {
    fn generate_asm(&self) -> Result<Vec<String>, String> {
        let mut asm = Vec::<String>::new();
        asm.push(".text".to_string());
        for &func in self.func_layout() {
            let func_asm = self.func(func).generate_asm()?;
            asm.extend(func_asm);
        }
        Ok(asm)
    }
}

impl GenerateAsm for FunctionData {
    fn generate_asm(&self) -> Result<Vec<String>, String> {
        let mut func_asm = Vec::<String>::new();
        let name = &self.name()[1..];
        func_asm.push(format!("  .globl {}", name));
        func_asm.push(format!("{}:", name));
        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Return(ret) => {
                        if let Some(ret_value) = ret.value() {
                            let ret_value_data = self.dfg().value(ret_value);
                            match ret_value_data.kind() {
                                ValueKind::Integer(int) => {
                                    func_asm.push(format!("  li a0, {}", int.value()));
                                }
                                other_kind => {
                                    return Err(format!("Unknown value kind {:?}", other_kind));
                                },
                            }
                        }
                        func_asm.push("  ret".to_string());
                    }
                    other_kind => {
                        return Err(format!("Unknown value kind {:?}", other_kind));
                    }
                }
            }
        }
        Ok(func_asm)
    }
}
