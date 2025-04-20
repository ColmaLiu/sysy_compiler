use std::cmp;

use koopa::ir::{values::{Binary, Branch, Call, Jump, Load, Return, Store}, BinaryOp, FunctionData, Program, Value, ValueKind};
use super::{asminfo::AsmInfo, REGISTER};

pub trait GenerateAsm {
    fn generate_asm(&self, asm: &mut Vec<String>, info: &mut AsmInfo) -> Result<(), String>;
}

impl GenerateAsm for Program {
    fn generate_asm(&self, asm: &mut Vec<String>, info: &mut AsmInfo) -> Result<(), String> {
        if self.inst_layout().len() != 0 {
            asm.push("  .data".to_string());
        }
        for var in self.inst_layout() {
            let value_data = self.borrow_value(*var);
            let name = &value_data.name().as_ref().unwrap()[1..];
            info.glob_var.insert(*var, name.to_string());
            asm.push(format!("  .globl {}", name));
            asm.push(format!("{}:", name));
            match value_data.kind() {
                ValueKind::GlobalAlloc(alloc) => {
                    let init = alloc.init();
                    match self.borrow_value(init).kind() {
                        ValueKind::ZeroInit(_) => {
                            asm.push(format!("  .zero 4"));
                        }
                        ValueKind::Integer(int) => {
                            asm.push(format!("  .word {}", int.value()));
                        }
                        _ => {
                            return Err("Unexpected".to_string());
                        }
                    }
                }
                _ => {
                    return Err("Unexpected".to_string());
                }
            }
        }
        asm.push("  .text".to_string());
        for &func in self.func_layout() {
            let function_data = self.func(func);
            let name = &function_data.name()[1..];
            info.function.insert(func, name.to_string());
        }
        for &func in self.func_layout() {
            if self.func(func).layout().entry_bb().is_none() {
                continue;
            }
            info.stack.clear();
            info.ra_used = false;
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

        let mut local_space = 0;
        let mut ra_used = false;
        let mut param_len = 0;
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                if let ValueKind::Call(call) = value_data.kind() {
                    ra_used = true;
                    param_len = cmp::max(param_len, call.args().len());
                }
                if !value_data.ty().is_unit() {
                    // This includes alloc instruction
                    local_space += 4;
                    info.stack.insert(inst, local_space);
                }
            }
        }
        info.ra_used = ra_used;
        let mut stack_frame = 
            local_space +
            if ra_used { 4 } else { 0 } +
            cmp::max(param_len as isize - 8, 0) as usize * 4
        ;
        stack_frame = (stack_frame + 15) / 16 * 16;
        if stack_frame > 0 && stack_frame < 2048 {
            asm.push(format!("  addi sp, sp, -{}", stack_frame));
        } else if stack_frame >= 2048 {
            asm.push(format!("  li t0, -{}", stack_frame));
            asm.push(format!("  add sp, sp, t0"));
        }
        if ra_used {
            // stack_frame is greater than 0
            let ra_offset = stack_frame - 4;
            if ra_offset < 2048 {
                asm.push(format!("  sw ra, {}(sp)", ra_offset));
            } else {
                asm.push(format!("  li t0, {}", ra_offset));
                asm.push(format!("  add t0, t0, sp"));
                asm.push(format!("  sw ra (t0)"));
            }
        }

        for (&bb, node) in self.layout().bbs() {
            let label = &self.dfg().bb(bb).name().as_ref().unwrap()[1..];
            asm.push(format!("{}_{}:", name, label));
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
                    ValueKind::Call(call) => {
                        call_to_asm(self, asm, info, call, inst, stack_frame)?;
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
    if let Some(name) = info.glob_var.get(&value) {
        let name = name.clone();
        let reg_idx = info.set_reg(value)?;
        asm.push(format!("  la {}, {}", REGISTER[reg_idx], name));
        asm.push(format!("  lw {}, ({})", REGISTER[reg_idx], REGISTER[reg_idx]));
        Ok((reg_idx, true))
    } else {
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
            ValueKind::FuncArgRef(arg) => {
                let arg_idx = arg.index();
                let reg_idx = info.set_reg(value)?;
                if arg_idx < 8 {
                    asm.push(format!("  mv {}, a{}", REGISTER[reg_idx], arg_idx));
                } else {
                    let len = function_data.params().len();
                    let offset = stack_frame + (len - 1 - arg_idx) * 4;
                    if offset < 2048 {
                        asm.push(format!("  lw {}, {}(sp)", REGISTER[reg_idx], offset));
                    } else {
                        let tmp_reg = info.get_vacant()?;
                        asm.push(format!("  li {}, {}", REGISTER[tmp_reg], offset));
                        asm.push(format!("  add {}, {}, sp", REGISTER[tmp_reg], REGISTER[tmp_reg]));
                        asm.push(format!("  lw {}, ({})", REGISTER[reg_idx], REGISTER[tmp_reg]));
                    }
                }
                Ok((reg_idx, true))
            }
            _ => {
                let reg_idx = info.set_reg(value)?;
                let ra_offset = if info.ra_used { 4 } else { 0 };
                let offset = stack_frame - info.stack.get(&value).unwrap() - ra_offset;
                if offset < 2048 {
                    asm.push(format!("  lw {}, {}(sp)", REGISTER[reg_idx], offset));
                } else {
                    let tmp_reg = info.get_vacant()?;
                    asm.push(format!("  li {}, {}", REGISTER[tmp_reg], offset));
                    asm.push(format!("  add {}, {}, sp", REGISTER[tmp_reg], REGISTER[tmp_reg]));
                    asm.push(format!("  lw {}, ({})", REGISTER[reg_idx], REGISTER[tmp_reg]));
                }
                Ok((reg_idx, true))
            }
        }
    }
}

fn store_in_memory(
    info: &mut AsmInfo,
    asm: &mut Vec<String>,
    reg_idx: usize,
    dest: Value,
    stack_frame: usize,
) -> Result<(), String> {
    if let Some(name) = info.glob_var.get(&dest) {
        let name = name.clone();
        let tmp_reg = info.get_vacant()?;
        asm.push(format!("  la {}, {}", REGISTER[tmp_reg], name));
        asm.push(format!("  sw {}, ({})", REGISTER[reg_idx], REGISTER[tmp_reg]));
    } else {
        let ra_offset = if info.ra_used { 4 } else { 0 };
        let offset = stack_frame - info.stack.get(&dest).unwrap() - ra_offset;
        if offset < 2048 {
            asm.push(format!("  sw {}, {}(sp)", REGISTER[reg_idx], offset));
        } else {
            let tmp_reg = info.get_vacant()?;
            asm.push(format!("  li {}, {}", REGISTER[tmp_reg], offset));
            asm.push(format!("  add {}, {}, sp", REGISTER[tmp_reg], REGISTER[tmp_reg]));
            asm.push(format!("  sw {}, ({})", REGISTER[reg_idx], REGISTER[tmp_reg]));
        }
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
    store_in_memory(info, asm, reg_idx, inst, stack_frame)?;
    if need_free {
        info.free_reg(reg_idx);
    }
    Ok(())
}

fn store_to_asm(function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    store: &Store,
    stack_frame: usize,
) -> Result<(), String> {
    let (reg_idx, need_free) = load_to_reg(function_data, asm, info, store.value(), stack_frame)?;
    store_in_memory(info, asm, reg_idx, store.dest(), stack_frame)?;
    if need_free {
        info.free_reg(reg_idx);
    }
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
    store_in_memory(info, asm, res_reg, inst, stack_frame)?;
    info.free_reg(res_reg);
    Ok(())
}

fn branch_to_asm(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    branch: &Branch,
    stack_frame: usize,
) -> Result<(), String> {
    let name = &function_data.name()[1..];
    let cond = branch.cond();
    let true_bb = branch.true_bb();
    let false_bb = branch.false_bb();
    let true_label = &function_data.dfg().bb(true_bb).name().as_ref().unwrap()[1..];
    let false_label = &function_data.dfg().bb(false_bb).name().as_ref().unwrap()[1..];
    let (reg_idx, need_free) = load_to_reg(function_data, asm, info, cond, stack_frame)?;
    if need_free {
        info.free_reg(reg_idx);
    }
    asm.push(format!("  bnez {}, {}_{}", REGISTER[reg_idx], name, true_label));
    asm.push(format!("  j {}_{}", name, false_label));
    Ok(())
}

fn jump_to_asm(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    jump: &Jump,
) -> Result<(), String> {
    let name = &function_data.name()[1..];
    let target = jump.target();
    let label =  &function_data.dfg().bb(target).name().as_ref().unwrap()[1..];
    asm.push(format!("  j {}_{}", name, label));
    Ok(())
}

fn call_to_asm(
    function_data: &FunctionData,
    asm: &mut Vec<String>,
    info: &mut AsmInfo,
    call: &Call,
    inst: Value,
    stack_frame: usize,
) -> Result<(), String> {
    let callee = call.callee();
    let args = call.args();
    let len = args.len();
    for idx in 0..cmp::min(len, 8) {
        let (reg_idx, need_free) = load_to_reg(function_data, asm, info, args[idx], stack_frame)?;
        asm.push(format!("  mv a{}, {}", idx, REGISTER[reg_idx]));
        if need_free {
            info.free_reg(reg_idx);
        }
    }
    for idx in 8..len {
        let (reg_idx, need_free) = load_to_reg(function_data, asm, info, args[idx], stack_frame)?;
        let offset = (len - 1 - idx) * 4;
        if offset < 2048 {
            asm.push(format!("  sw {}, {}(sp)", REGISTER[reg_idx], offset));
        } else {
            let tmp_reg = info.get_vacant()?;
            asm.push(format!("  li {}, {}", REGISTER[tmp_reg], offset));
            asm.push(format!("  add {}, {}, sp", REGISTER[tmp_reg], REGISTER[tmp_reg]));
            asm.push(format!("  sw {}, ({})", REGISTER[reg_idx], REGISTER[tmp_reg]));
        }
        if need_free {
            info.free_reg(reg_idx);
        }
    }
    let function = info.function.get(&callee).unwrap();
    asm.push(format!("  call {}", function));
    if !function_data.dfg().value(inst).ty().is_unit() {
        store_in_memory(info, asm, 10, inst, stack_frame)?;
    }
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
    if info.ra_used {
        // stack_frame is greater than 0
        let ra_offset = stack_frame - 4;
        if ra_offset < 2048 {
            asm.push(format!("  lw ra, {}(sp)", ra_offset));
        } else {
            asm.push(format!("  li t0, {}", ra_offset));
            asm.push(format!("  add t0, t0, sp"));
            asm.push(format!("  lw ra (t0)"));
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
