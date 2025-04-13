use koopa::ir::{BinaryOp, FunctionData, Program, ValueKind};
use super::{asminfo::AsmInfo, REGISTER};

pub trait GenerateAsm {
    fn generate_asm(&self, asm: &mut Vec<String>, info: &mut AsmInfo) -> Result<(), String>;
}

impl GenerateAsm for Program {
    fn generate_asm(&self, asm: &mut Vec<String>, info: &mut AsmInfo) -> Result<(), String> {
        asm.push("  .text".to_string());
        for &func in self.func_layout() {
            self.func(func).generate_asm(asm, info)?;
        }
        Ok(())
    }
}

impl GenerateAsm for FunctionData {
    fn generate_asm(&self, asm: &mut Vec<String>, info: &mut AsmInfo) -> Result<(), String> {
        let name = &self.name()[1..];
        asm.push(format!("  .globl {}", name));
        asm.push(format!("{}:", name));
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Return(ret) => {
                        if let Some(ret_value) = ret.value() {
                            let ret_value_data = self.dfg().value(ret_value);
                            match ret_value_data.kind() {
                                ValueKind::Integer(int) => {
                                    asm.push(format!("  li a0, {}", int.value()));
                                }
                                ValueKind::Binary(_) => {
                                    let reg_idx = info.get_occupied(ret_value)?;
                                    info.free_reg(reg_idx);
                                    asm.push(format!("  mv a0, {}", REGISTER[reg_idx]));
                                }
                                other_kind => {
                                    return Err(format!("Unknown value kind {:?}", other_kind));
                                }
                            }
                        }
                        asm.push("  ret".to_string());
                    }
                    ValueKind::Binary(binary) => {
                        let mut get_reg = |value| {
                            let value_data = self.dfg().value(value);
                            match value_data.kind() {
                                ValueKind::Integer(int) => {
                                    if int.value() == 0 {
                                        Ok((0, false)) // x0 register
                                    } else {
                                        let reg_idx = info.set_reg(value)?;
                                        asm.push(format!("  li {}, {}", REGISTER[reg_idx], int.value()));
                                        Ok((reg_idx, true))
                                    }
                                }
                                ValueKind::Binary(_) => {
                                    info.get_occupied(value).map(|idx| (idx, true))
                                }
                                other_kind => {
                                    Err(format!("Unknown value kind {:?}", other_kind))
                                }
                            }
                        };
                        let (lhs_reg, lhs_need_free) = get_reg(binary.lhs())?;
                        let (rhs_reg, rhs_need_free) = get_reg(binary.rhs())?;
                        if lhs_need_free {
                            info.free_reg(lhs_reg);
                        }
                        if rhs_need_free {
                            info.free_reg(rhs_reg);
                        }
                        let res_reg = info.set_reg(inst)?;
                        match binary.op() {
                            BinaryOp::NotEq => {
                                asm.push(format!(
                                    "  xor {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                                asm.push(format!(
                                    "  snez {}, {}",
                                    REGISTER[res_reg], REGISTER[res_reg]
                                ));
                            }
                            BinaryOp::Eq => {
                                asm.push(format!(
                                    "  xor {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                                asm.push(format!(
                                    "  seqz {}, {}",
                                    REGISTER[res_reg], REGISTER[res_reg]
                                ));
                            }
                            BinaryOp::Gt => {
                                asm.push(format!(
                                    "  slt {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[rhs_reg], REGISTER[lhs_reg]
                                ));
                            }
                            BinaryOp::Lt => {
                                asm.push(format!(
                                    "  slt {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            BinaryOp::Ge => {
                                asm.push(format!(
                                    "  slt {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                                asm.push(format!(
                                    "  xori {}, {}, 1",
                                    REGISTER[res_reg], REGISTER[res_reg]
                                ));
                            }
                            BinaryOp::Le => {
                                asm.push(format!(
                                    "  sgt {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                                asm.push(format!(
                                    "  xori {}, {}, 1",
                                    REGISTER[res_reg], REGISTER[res_reg]
                                ));
                            }
                            BinaryOp::Add => {
                                asm.push(format!(
                                    "  add {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            BinaryOp::Sub => {
                                asm.push(format!(
                                    "  sub {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            BinaryOp::Mul => {
                                asm.push(format!(
                                    "  mul {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            BinaryOp::Div => {
                                asm.push(format!(
                                    "  div {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            BinaryOp::Mod => {
                                asm.push(format!(
                                    "  rem {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            BinaryOp::And => {
                                asm.push(format!(
                                    "  and {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            BinaryOp::Or => {
                                asm.push(format!(
                                    "  or {}, {}, {}",
                                    REGISTER[res_reg], REGISTER[lhs_reg], REGISTER[rhs_reg]
                                ));
                            }
                            _ => (),
                        }
                    }
                    other_kind => {
                        return Err(format!("Unknown value kind {:?}", other_kind));
                    }
                }
            }
        }
        Ok(())
    }
}
