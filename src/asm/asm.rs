use koopa::ir::{values::{Binary, Branch, Jump, Load, Return, Store}, BinaryOp, FunctionData, Program, Value, ValueKind};
use super::{asminfo::AsmInfo, REGISTER};

pub trait GenerateAsm {
    fn generate_asm(&self, asm: &mut Vec<String>, info: &mut AsmInfo) -> Result<(), String>;
}

impl GenerateAsm for Program {
    fn generate_asm(&self, asm: &mut Vec<String>, info: &mut AsmInfo) -> Result<(), String> {
        asm.push("  .text".to_string());
        for &func in self.func_layout() {
            info.stack.clear();
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

        let mut stack_frame = 0;
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                if let ValueKind::Alloc(_) = value_data.kind() {
                    stack_frame += value_data.ty().size();
                    info.stack.insert(inst, stack_frame);
                } else if !value_data.ty().is_unit() {
                    stack_frame += value_data.ty().size();
                    info.stack.insert(inst, stack_frame);
                }
            }
        }
        stack_frame = (stack_frame + 15) / 16 * 16;
        if stack_frame > 0 && stack_frame < 2048 {
            asm.push(format!("  addi sp, sp, -{}", stack_frame));
        } else if stack_frame >= 2048 {
            asm.push(format!("  li t0, -{}", stack_frame));
            asm.push(format!("  add sp, sp, t0"));
        }

        for (&bb, node) in self.layout().bbs() {
            let label = &self.dfg().bb(bb).name().as_ref().unwrap()[1..];
            asm.push(format!("{}:", label));
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Alloc(_) => (),
                    ValueKind::Load(load) => {
                        load_to_asm(self, asm, info, load, inst, stack_frame)?;
                    }
                    ValueKind::Store(store) => {
                        store_to_asm(self, asm, info, store, stack_frame)?;
                    }
                    ValueKind::Binary(binary) => {
                        binary_to_asm(self, asm, info, binary, inst, stack_frame)?;
                    }
                    ValueKind::Branch(branch) => {
                        branch_to_asm(self, asm, info, branch, stack_frame)?;
                    }
                    ValueKind::Jump(jump) => {
                        jump_to_asm(self, asm, jump)?;
                    }
                    ValueKind::Return(ret) => {
                        return_to_asm(self, asm, info, ret, stack_frame)?;
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

fn load_to_reg(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    value: Value,
    stack_frame: usize,
) -> Result<(usize, bool), String> {
    let value_data = function_data.dfg().value(value);
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
        _ => {
            let reg_idx = info.set_reg(value)?;
            let offset = stack_frame - info.stack.get(&value).unwrap();
            if offset < 2048 {
                asm.push(format!("  lw {}, {}(sp)", REGISTER[reg_idx], offset));
            } else {
                let tmp_reg = info.get_vacant()?;
                asm.push(format!("  li {}, {}", REGISTER[tmp_reg], offset));
                asm.push(format!("  add {}, {}, sp", REGISTER[tmp_reg], REGISTER[tmp_reg]));
                asm.push(format!("  lw {}, 0(sp)", REGISTER[reg_idx]));
            }
            Ok((reg_idx, true))
        }
    }
}

fn store_on_stack(
    info: &mut AsmInfo,
    asm: &mut Vec<String>,
    reg_idx: usize,
    dest: Value,
    stack_frame: usize,
) -> Result<(), String> {
    let offset = stack_frame - info.stack.get(&dest).unwrap();
    if offset < 2048 {
        asm.push(format!("  sw {}, {}(sp)", REGISTER[reg_idx], offset));
    } else {
        let tmp_reg = info.get_vacant()?;
        asm.push(format!("  li {}, {}", REGISTER[tmp_reg], offset));
        asm.push(format!("  add {}, {}, sp", REGISTER[tmp_reg], REGISTER[tmp_reg]));
        asm.push(format!("  sw {}, 0(sp)", REGISTER[reg_idx]));
    }
    Ok(())
}

fn load_to_asm(function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    load: &Load,
    inst: Value,
    stack_frame: usize,
) -> Result<(), String> {
    let (reg_idx, need_free) = load_to_reg(function_data, asm, info, load.src(), stack_frame)?;
    if need_free {
        info.free_reg(reg_idx);
    }
    store_on_stack(info, asm, reg_idx, inst, stack_frame)?;
    Ok(())
}

fn store_to_asm(function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    store: &Store,
    stack_frame: usize,
) -> Result<(), String> {
    let (reg_idx, need_free) = load_to_reg(function_data, asm, info, store.value(), stack_frame)?;
    if need_free {
        info.free_reg(reg_idx);
    }
    store_on_stack(info, asm, reg_idx, store.dest(), stack_frame)?;
    Ok(())
}

fn binary_to_asm(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    binary: &Binary,
    inst: Value,
    stack_frame: usize,
) -> Result<(), String> {
    let (lhs_reg, lhs_need_free) = load_to_reg(function_data, asm, info, binary.lhs(), stack_frame)?;
    let (rhs_reg, rhs_need_free) = load_to_reg(function_data, asm, info, binary.rhs(), stack_frame)?;
    if lhs_need_free {
        info.free_reg(lhs_reg);
    }
    if rhs_need_free {
        info.free_reg(rhs_reg);
    }
    let res_reg = info.get_vacant()?;
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
    store_on_stack(info, asm, res_reg, inst, stack_frame)?;
    Ok(())
}

fn branch_to_asm(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    branch: &Branch,
    stack_frame: usize,
) -> Result<(), String> {
    let cond = branch.cond();
    let true_bb = branch.true_bb();
    let false_bb = branch.false_bb();
    let true_label = &function_data.dfg().bb(true_bb).name().as_ref().unwrap()[1..];
    let false_label = &function_data.dfg().bb(false_bb).name().as_ref().unwrap()[1..];
    let (reg_idx, need_free) = load_to_reg(function_data, asm, info, cond, stack_frame)?;
    if need_free {
        info.free_reg(reg_idx);
    }
    asm.push(format!("  bnez {}, {}", REGISTER[reg_idx], true_label));
    asm.push(format!("  j {}", false_label));
    Ok(())
}

fn jump_to_asm(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    jump: &Jump,
) -> Result<(), String> {
    let target = jump.target();
    let label =  &function_data.dfg().bb(target).name().as_ref().unwrap()[1..];
    asm.push(format!("  j {}", label));
    Ok(())
}

fn return_to_asm(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    ret: &Return,
    stack_frame: usize,
) -> Result<(), String> {
    if let Some(ret_value) = ret.value() {
        let ret_value_data = function_data.dfg().value(ret_value);
        match ret_value_data.kind() {
            ValueKind::Integer(int) => {
                asm.push(format!("  li a0, {}", int.value()));
            }
            _ => {
                let (reg_idx, need_free) = load_to_reg(function_data, asm, info, ret_value, stack_frame)?;
                if need_free {
                    info.free_reg(reg_idx);
                }
                asm.push(format!("  mv a0, {}", REGISTER[reg_idx]));
            }
        }
    }
    if stack_frame > 0 && stack_frame < 2048 {
        asm.push(format!("  addi sp, sp, {}", stack_frame));
    } else if stack_frame >= 2048 {
        asm.push(format!("  li t0, {}", stack_frame));
        asm.push(format!("  add sp, sp, t0"));
    }
    asm.push("  ret".to_string());
    Ok(())
}
