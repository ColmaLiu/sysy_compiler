use std::iter::zip;

use crate::ast_type::*;
use koopa::ir::{builder::{BasicBlockBuilder, GlobalInstBuilder, LocalInstBuilder, ValueBuilder}, BinaryOp, FunctionData, Program, Type, TypeKind, Value, ValueKind};
use super::{irinfo::{Context, IrInfo, Symbol, WhileBlockInfo}, solve::Solve};

pub trait GenerateIR {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String>;
}

trait Expression: GenerateIR {}

fn exp2ir(
    op: BinaryOp,
    lexp: &dyn Expression,
    rexp: &dyn Expression,
    program: &mut Program,
    info: &mut IrInfo,
) -> Result<(), String> {
    lexp.generate_ir(program, info)?;
    let lhs = info.context.value.unwrap();
    rexp.generate_ir(program, info)?;
    let rhs = info.context.value.unwrap();
    val2ir(op, lhs, rhs, program, &mut info.context);
    Ok(())
}

fn val2ir(
    op: BinaryOp,
    lhs: Value,
    rhs: Value,
    program: &mut Program,
    context: &mut Context,
) {
    let func_data = program.func_mut(context.function.unwrap());
    let inst = func_data.dfg_mut().new_value().binary(op, lhs, rhs);
    func_data.layout_mut().bb_mut(context.block.unwrap()).insts_mut().extend([inst]);
    context.value = Some(inst);
}

#[derive(Debug)]
enum VarType {
    Int32,
    Array(usize),
    Pointer(usize),
}

fn get_ty_recursive(kind: &TypeKind) -> VarType {
    match kind {
        TypeKind::Int32 => {
            VarType::Int32
        }
        TypeKind::Array(base_ty, _) => {
            let var_ty = get_ty_recursive(base_ty.kind());
            match var_ty {
                VarType::Int32 => VarType::Array(1),
                VarType::Array(n) => VarType::Array(n + 1),
                VarType::Pointer(_) => unreachable!(),
            }
        }
        TypeKind::Pointer(base_ty) => {
            let var_ty = get_ty_recursive(base_ty.kind());
            match var_ty {
                VarType::Int32 => VarType::Pointer(1),
                VarType::Array(n) => VarType::Pointer(n + 1),
                VarType::Pointer(_) => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn get_var_type(var: Value, is_glob: bool, program: &mut Program, info: &mut IrInfo) -> VarType {
    let func = info.context.function.unwrap();
    let func_data = program.func_mut(func);
    let get = |kind: &TypeKind| {
        match kind {
            TypeKind::Pointer(ty) => {
                let kind = ty.kind();
                get_ty_recursive(kind)
            }
            _ => unreachable!(),
        }
    };
    if is_glob {
        let value = program.borrow_value(var);
        let kind = value.ty().kind();
        get(kind)
    } else {
        let kind = func_data.dfg().value(var).ty().kind();
        get(kind)
    }
}

impl GenerateIR for CompUnit {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        info.symbol_table.push_table();
        let names = [
            "getint", "getch", "getarray", "putint", "putch", "putarray", "starttime", "stoptime"
        ];
        let params_tys = [
            vec![], vec![], vec![Type::get_pointer(Type::get_i32())], vec![Type::get_i32()],
            vec![Type::get_i32()], vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
            vec![], vec![]
        ];
        let ret_tys = [
            Type::get_i32(), Type::get_i32(), Type::get_i32(), Type::get_unit(),
            Type::get_unit(), Type::get_unit(), Type::get_unit(), Type::get_unit()
        ];
        for (name, (params_ty, ret_ty)) in zip(names, zip(params_tys, ret_tys)) {
            let function = program.new_func(FunctionData::new_decl(format!("@{}", name), params_ty, ret_ty));
            info.symbol_table.insert(name.to_string(), Symbol::Func(name.to_string(), function));
        }
        for comp_unit_item in &self.comp_unit_items {
            match comp_unit_item {
                CompUnitItem::Decl(decl) => {
                    decl.generate_ir(program, info)?;
                }
                CompUnitItem::FuncDef(func_def) => {
                    func_def.generate_ir(program, info)?;
                }
            }
        }
        info.symbol_table.pop_table();
        Ok(())
    }
}

impl GenerateIR for Decl {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Const(const_decl) => const_decl.generate_ir(program, info),
            Self::Var(var_decl) => var_decl.generate_ir(program, info),
        }
    }
}

impl GenerateIR for ConstDecl {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        for const_def in &self.const_defs {
            const_def.generate_ir(program, info)?;
        }
        Ok(())
    }
}

impl GenerateIR for ConstDef {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match (self.shape.len(), &self.init_val) {
            (0, InitVal::Int(exp)) => {
                let int = exp.solve(info).map_err(|_err| "Unknown value during compile time")?;
                if info.symbol_table.insert(
                    self.ident.clone(), super::irinfo::Symbol::Const(self.ident.clone(), int)
                ).is_none() {
                    Ok(())
                } else {
                    Err("Redefined symbol".to_string())
                }
            }
            (_, InitVal::Array(_)) => {
                // see it as a variable array
                let mut shape = Vec::new();
                for exp in &self.shape {
                    let len = exp.solve(info)
                        .map_err(|_err| "Unknown value during compile time")? as usize;
                    shape.push(len);
                }
                self.init_val.init(program, info, &shape, &self.ident, false)?;
                Ok(())
            }
            _ => unreachable!(),
        }
    }
}

impl GenerateIR for VarDecl {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        for var_def in &self.var_defs {
            var_def.generate_ir(program, info)?;
        }
        Ok(())
    }
}

impl GenerateIR for VarDef {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        if self.shape.len() == 0 {
            if info.symbol_table.depth() == 0 {
                let init = match &self.init_val {
                    Some(init_val) => {
                        let exp = match init_val {
                            InitVal::Int(exp) => exp,
                            InitVal::Array(_) => unreachable!(),
                        };
                        let int = exp.solve(info).map_err(|_err| "Unknown value during compile time")?;
                        program.new_value().integer(int)
                    }
                    None => {
                        program.new_value().zero_init(Type::get_i32())
                    }
                };
                let var = program.new_value().global_alloc(init);
                program.set_value_name(var, Some(format!("@{}", self.ident)));
                if info.symbol_table.insert(
                    self.ident.clone(), super::irinfo::Symbol::Var(self.ident.clone(), var)
                ).is_some() {
                    return Err("Redefined symbol".to_string());
                }
            } else {
                let func = info.context.function.unwrap();
                let func_data = program.func_mut(func);
                let var = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                func_data.dfg_mut().set_value_name(var, Some(format!("@{}", self.ident)));
                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([var]);
                if info.symbol_table.insert(
                    self.ident.clone(), super::irinfo::Symbol::Var(self.ident.clone(), var)
                ).is_some() {
                    return Err("Redefined symbol".to_string());
                }
                if let Some(init_val) = &self.init_val {
                    init_val.generate_ir(program, info)?;
                    let func_data = program.func_mut(func);
                    let store = func_data.dfg_mut().new_value().store(info.context.value.unwrap(), var);
                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store]);
                }
            }
        } else {
            let mut shape = Vec::new();
            for exp in &self.shape {
                let len = exp.solve(info)
                    .map_err(|_err| "Unknown value during compile time")? as usize;
                shape.push(len);
            }
            if let Some(init_val) = &self.init_val {
                init_val.init(program, info, &shape, &self.ident, false)?;
            } else if info.symbol_table.depth() == 0 {
                InitVal::Array(Vec::new()).init(program, info, &shape, &self.ident, false)?;
            } else {
                InitVal::Array(Vec::new()).init(program, info, &shape, &self.ident, true)?;
            }
        }
        Ok(())
    }
}

impl GenerateIR for InitVal {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Int(exp) => {
                exp.generate_ir(program, info)
            }
            Self::Array(_) => unreachable!(),
        }
    }
}

impl GenerateIR for FuncDef {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        let ret_ty = match self.func_type {
            FuncType::Void => Type::get_unit(),
            FuncType::Int => Type::get_i32(),
        };
        let mut params_ty = Vec::new();
        if let Some(params) = &self.params {
            for param in &params.params {
                match param {
                    FuncFParam::Int(ident) => {
                        params_ty.push((Some(format!("@{}", ident)), Type::get_i32()));
                    }
                    FuncFParam::Array(ident, shape) => {
                        let mut ty = Type::get_i32();
                        for exp in shape {
                            let len = exp.solve(info)
                                .map_err(|_err| "Unknown value during compile time")? as usize;
                            ty = Type::get_array(ty, len);
                        }
                        ty = Type::get_pointer(ty);
                        params_ty.push((Some(format!("@{}", ident)), ty));
                    }
                }
            }
        }
        let func = program.new_func(
            FunctionData::with_param_names(
                format!("@{}", self.ident), params_ty, ret_ty
            )
        );
        if info.symbol_table.insert(self.ident.clone(), Symbol::Func(self.ident.clone(), func)).is_some() {
            return Err("Redefined symbol".to_string());
        }

        let func_data = program.func_mut(func);
        let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        info.context.function = Some(func);
        info.context.block = Some(entry);
        info.context.exited = false;

        info.symbol_table.push_table();
        info.symbol_table.set_entry();
        if let Some(params_f) = &self.params {
            let params_v = Vec::from(program.func_mut(func).params());
            for (param_f, param_v) in zip(&params_f.params, params_v) {
                let func_data = program.func_mut(func);
                let (ident, ty) = match param_f {
                    FuncFParam::Int(ident) => {
                        (ident.clone(), Type::get_i32())
                    }
                    FuncFParam::Array(ident, shape) => {
                        let mut ty = Type::get_i32();
                        for exp in shape {
                            let len = exp.solve(info)
                                .map_err(|_err| "Unknown value during compile time")? as usize;
                            ty = Type::get_array(ty, len);
                        }
                        ty = Type::get_pointer(ty);
                        (ident.clone(), ty)
                    }
                };
                let rp = func_data.dfg_mut().new_value().alloc(ty);
                func_data.dfg_mut().set_value_name(rp, Some(format!("%{}", ident)));
                let store = func_data.dfg_mut().new_value().store(param_v, rp);
                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([rp, store]);
                info.symbol_table.insert(ident.clone(), Symbol::Var(ident.clone(), rp));
            }
        }

        self.block.generate_ir(program, info)?;

        if !info.context.exited {
            let func_data = program.func_mut(info.context.function.unwrap());
            let value = match self.func_type {
                FuncType::Void => None,
                FuncType::Int => Some(func_data.dfg_mut().new_value().integer(0)),
            };
            let ret = func_data.dfg_mut().new_value().ret(value);
            func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([ret]);
        }

        Ok(())
    }
}

impl GenerateIR for Block {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        if !info.symbol_table.check_entry() {
            info.symbol_table.push_table();
        }
        for block_item in &self.block_items {
            if info.context.exited {
                break;
            }
            block_item.generate_ir(program, info)?;
        }
        info.symbol_table.pop_table();
        Ok(())
    }
}

impl GenerateIR for BlockItem {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Decl(decl) => decl.generate_ir(program, info),
            Self::Stmt(stmt) => stmt.generate_ir(program, info)
        }
    }
}

impl GenerateIR for Stmt {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Assign(lval, exp ) => {
                exp.generate_ir(program, info)?;
                let exp_val = info.context.value.unwrap();

                if let Some((symbol, is_glob)) = info.symbol_table.get(&lval.ident) {
                    match *symbol {
                        Symbol::Var(_, var) => {
                            let var_type = get_var_type(var, is_glob, program, info);
                            let func = info.context.function.unwrap();
                            let func_data = program.func_mut(func);
                            match var_type {
                                VarType::Int32 => {
                                    let store = func_data.dfg_mut().new_value().store(exp_val, var);
                                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store]);
                                }
                                VarType::Array(_) => {
                                    let mut get_vec = vec![var];
                                    for exp in &lval.index {
                                        exp.generate_ir(program, info)?;
                                        let index = info.context.value.unwrap();
                                        let func_data = program.func_mut(func);
                                        let get = func_data.dfg_mut().new_value().get_elem_ptr(*get_vec.last().unwrap(), index);
                                        get_vec.push(get);
                                    }
                                    let func_data = program.func_mut(func);
                                    let dest = get_vec.last().unwrap().clone();
                                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend(get_vec.drain(1..));
                                    let store = func_data.dfg_mut().new_value().store(exp_val, dest);
                                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store]);
                                }
                                VarType::Pointer(_) => {
                                    let load = func_data.dfg_mut().new_value().load(var);
                                    let mut get_vec = vec![load];
                                    for (exp, dim) in zip(&lval.index, 0..) {
                                        exp.generate_ir(program, info)?;
                                        let index = info.context.value.unwrap();
                                        let func_data = program.func_mut(func);
                                        let get = if dim == 0 {
                                            func_data.dfg_mut().new_value().get_ptr(*get_vec.last().unwrap(), index)
                                        } else {
                                            func_data.dfg_mut().new_value().get_elem_ptr(*get_vec.last().unwrap(), index)
                                        };
                                        get_vec.push(get);
                                    }
                                    let func_data = program.func_mut(func);
                                    let dest = get_vec.last().unwrap().clone();
                                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend(get_vec);
                                    let store = func_data.dfg_mut().new_value().store(exp_val, dest);
                                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store]);
                                    info.context.value = Some(load);
                                }
                            }
                        }
                        Symbol::Const(_, _) => {
                            return Err("Constant can not be a left value".to_string());
                        }
                        Symbol::Func(_, _) => {
                            return Err("Undefined variable".to_string());
                        }
                    }
                } else {
                    return Err("Undefined variable".to_string());
                }
            }
            Self::Exp(exp) => {
                if let Some(exp) = exp {
                    exp.generate_ir(program, info)?;
                }
            }
            Self::Block(block) => {
                block.generate_ir(program, info)?;
            }
            Self::If(exp, then_stmt, else_stmt) => {
                info.if_cnt += 1;
                let if_cnt = info.if_cnt;

                exp.generate_ir(program, info)?;

                let func = info.context.function.unwrap();

                let func_data = program.func_mut(func);
                let end_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%end_{}", if_cnt)));

                let start_bb = info.context.block.unwrap();
                let cond = info.context.value.unwrap();

                let func_data = program.func_mut(func);
                let true_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%then_{}", if_cnt)));
                program.func_mut(func).layout_mut().bbs_mut().extend([true_bb]);
                info.context.block = Some(true_bb);
                then_stmt.as_ref().generate_ir(program, info)?;
                if !info.context.exited {
                    let func_data = program.func_mut(func);
                    let jump = func_data.dfg_mut().new_value().jump(end_bb);
                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([jump]);
                } else {
                    info.context.exited = false;
                }

                let false_bb = match else_stmt {
                    Some(else_stmt) => {
                        let func_data = program.func_mut(func);
                        let else_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%else_{}", if_cnt)));
                        program.func_mut(func).layout_mut().bbs_mut().extend([else_bb]);
                        info.context.block = Some(else_bb);
                        else_stmt.as_ref().generate_ir(program, info)?;
                        if !info.context.exited {
                            let func_data = program.func_mut(func);
                            let jump = func_data.dfg_mut().new_value().jump(end_bb);
                            func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([jump]);
                        } else {
                            info.context.exited = false;
                        }
                        else_bb
                    }
                    None => end_bb,
                };

                let func_data = program.func_mut(func);
                let branch = func_data.dfg_mut().new_value().branch(cond, true_bb, false_bb);
                func_data.layout_mut().bb_mut(start_bb).insts_mut().extend([branch]);

                program.func_mut(func).layout_mut().bbs_mut().extend([end_bb]);
                info.context.block = Some(end_bb);
            }
            Self::While(exp, body_stmt) => {
                info.while_cnt += 1;
                let while_cnt = info.while_cnt;

                let func = info.context.function.unwrap();

                let func_data = program.func_mut(func);
                let while_entry_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%while_entry_{}", while_cnt)));
                let while_body_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%while_body_{}", while_cnt)));
                let while_end_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%while_end_{}", while_cnt)));

                let func_data = program.func_mut(func);
                let jump = func_data.dfg_mut().new_value().jump(while_entry_bb);
                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([jump]);

                program.func_mut(func).layout_mut().bbs_mut().extend([while_entry_bb]);
                info.context.block = Some(while_entry_bb);
                exp.generate_ir(program, info)?;
                let cond = info.context.value.unwrap();
                let func_data = program.func_mut(func);
                let branch = func_data.dfg_mut().new_value().branch(cond, while_body_bb, while_end_bb);
                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([branch]);

                program.func_mut(func).layout_mut().bbs_mut().extend([while_body_bb]);
                info.context.block = Some(while_body_bb);
                info.while_info.push(WhileBlockInfo::new(while_entry_bb, while_end_bb));
                body_stmt.as_ref().generate_ir(program, info)?;
                if !info.context.exited {
                    let func_data = program.func_mut(func);
                    let jump = func_data.dfg_mut().new_value().jump(while_entry_bb);
                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([jump]);
                } else {
                    info.context.exited = false;
                }

                program.func_mut(func).layout_mut().bbs_mut().extend([while_end_bb]);
                info.context.block = Some(while_end_bb);
                info.while_info.pop();
            }
            Self::Break => {
                let end_bb = info.while_info.last().unwrap().end_bb();
                let func_data = program.func_mut(info.context.function.unwrap());
                let jump = func_data.dfg_mut().new_value().jump(end_bb);
                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([jump]);
                info.context.exited = true;
            }
            Self::Continue => {
                let entry_bb = info.while_info.last().unwrap().entry_bb();
                let func_data = program.func_mut(info.context.function.unwrap());
                let jump = func_data.dfg_mut().new_value().jump(entry_bb);
                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([jump]);
                info.context.exited = true;
            }
            Self::Return(exp) => {
                if info.context.exited {
                    return Ok(());
                }
                let ret_value = match exp {
                    Some(exp) => {
                        exp.generate_ir(program, info)?;
                        info.context.value
                    }
                    None => None,
                };
                let func_data = program.func_mut(info.context.function.unwrap());
                let ret = func_data.dfg_mut().new_value().ret(ret_value);
                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([ret]);
                info.context.exited = true;
            }
        }
        Ok(())
    }
}

impl GenerateIR for Exp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        self.lor_exp.generate_ir(program, info)
    }
}

impl GenerateIR for LVal {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        let func = info.context.function.unwrap();
        let func_data = program.func_mut(func);
        if let Some((symbol, is_glob)) = info.symbol_table.get(&self.ident) {
            match *symbol {
                Symbol::Const(_, int) => {
                    let value = func_data.dfg_mut().new_value().integer(int);
                    info.context.value = Some(value);
                }
                Symbol::Var(_, var) => {
                    let var_type = get_var_type(var, is_glob, program, info);
                    let func_data = program.func_mut(func);
                    match var_type {
                        VarType::Int32 => {
                            let load = func_data.dfg_mut().new_value().load(var);
                            func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([load]);
                            info.context.value = Some(load);
                        }
                        VarType::Array(dim) => {
                            let mut get_vec = vec![var];
                            for exp in &self.index {
                                exp.generate_ir(program, info)?;
                                let index = info.context.value.unwrap();
                                let func_data = program.func_mut(func);
                                let get = func_data.dfg_mut().new_value().get_elem_ptr(*get_vec.last().unwrap(), index);
                                get_vec.push(get);
                            }
                            let src = get_vec.last().unwrap().clone();
                            let func_data = program.func_mut(func);
                            func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend(get_vec.drain(1..));
                            if dim == self.index.len() {
                                let load = func_data.dfg_mut().new_value().load(src);
                                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([load]);
                                info.context.value = Some(load);
                            } else {
                                let zero = func_data.dfg_mut().new_value().integer(0);
                                let get = func_data.dfg_mut().new_value().get_elem_ptr(src, zero);
                                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([get]);
                                info.context.value = Some(get);
                            }
                        }
                        VarType::Pointer(dim) => {
                            let load = func_data.dfg_mut().new_value().load(var);
                            let mut get_vec = vec![load];
                            for (exp, dim) in zip(&self.index, 0..) {
                                exp.generate_ir(program, info)?;
                                let index = info.context.value.unwrap();
                                let func_data = program.func_mut(func);
                                let get = if dim == 0 {
                                    func_data.dfg_mut().new_value().get_ptr(*get_vec.last().unwrap(), index)
                                } else {
                                    func_data.dfg_mut().new_value().get_elem_ptr(*get_vec.last().unwrap(), index)
                                };
                                get_vec.push(get);
                            }
                            let src = get_vec.last().unwrap().clone();
                            let func_data = program.func_mut(func);
                            func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend(get_vec);
                            if dim == self.index.len() {
                                let load = func_data.dfg_mut().new_value().load(src);
                                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([load]);
                                info.context.value = Some(load);
                            }
                            else if self.index.len() == 0 {
                                info.context.value = Some(src);
                            } else {
                                let zero = func_data.dfg_mut().new_value().integer(0);
                                let get = func_data.dfg_mut().new_value().get_elem_ptr(src, zero);
                                func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([get]);
                                info.context.value = Some(get);
                            }
                        }
                    }
                }
                _ => {
                    return Err("Undefined variable".to_string());
                }
            }
            Ok(())
        } else {
            Err("Undefined variable".to_string())
        }
    }
}

impl GenerateIR for PrimaryExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Exp(exp) => exp.as_ref().generate_ir(program, info),
            Self::LVal(lval) => lval.generate_ir(program, info),
            Self::Num(num) => num.generate_ir(program, info),
        }
    }
}

impl GenerateIR for Number {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        let func_data = program.func_mut(info.context.function.unwrap());
        match self {
            Self::IntConst(int) => {
                let value = func_data.dfg_mut().new_value().integer(*int);
                info.context.value = Some(value);
            }
        }
        Ok(())
    }
}

impl GenerateIR for UnaryExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Primary(primary_exp) => {
                primary_exp.generate_ir(program, info)
            }
            Self::Func(ident, params) => {
                if let Some((Symbol::Func(_, function), _)) = info.symbol_table.get(ident) {
                    let function = function.clone();
                    let mut args = Vec::new();
                    if let Some(params) = params {
                        for param in &params.params {
                            param.generate_ir(program, info)?;
                            args.push(info.context.value.unwrap());
                        }
                    }
                    let func_data = program.func_mut(info.context.function.unwrap());
                    let call = func_data.dfg_mut().new_value().call(function, args);
                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([call]);
                    info.context.value = Some(call);
                    Ok(())
                } else {
                    Err("Undefined function".to_string())
                }
            }
            Self::Unary(unary_op, unary_exp) => {
                let unary_exp = unary_exp.as_ref();
                match unary_op {
                    UnaryOp::Pos => unary_exp.generate_ir(program, info),
                    UnaryOp::Neg => exp2ir(BinaryOp::Sub, &Number::IntConst(0), unary_exp, program, info),
                    UnaryOp::Not => exp2ir(BinaryOp::Eq, &Number::IntConst(0), unary_exp, program, info),
                }
            }
        }
    }
}

impl GenerateIR for MulExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Unary(exp) => {
                exp.generate_ir(program, info)
            }
            Self::Mul(mul_exp, mul_op, unary_exp) => {
                let mul_exp = mul_exp.as_ref();
                let op = match mul_op {
                    MulOp::Mul => BinaryOp::Mul,
                    MulOp::Div => BinaryOp::Div,
                    MulOp::Mod => BinaryOp::Mod,
                };
                exp2ir(op, mul_exp, unary_exp, program, info)
            }
        }
    }
}

impl GenerateIR for AddExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Mul(exp) => {
                exp.generate_ir(program, info)
            }
            Self::Add(add_exp, add_op, mul_exp) => {
                let add_exp = add_exp.as_ref();
                let op = match add_op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                };
                exp2ir(op, add_exp, mul_exp, program, info)
            }
        }
    }
}

impl GenerateIR for RelExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Add(exp) => {
                exp.generate_ir(program, info)
            }
            Self::Rel(rel_exp, rel_op, add_exp) => {
                let rel_exp = rel_exp.as_ref();
                let op = match rel_op {
                    RelOp::Lt => BinaryOp::Lt,
                    RelOp::Gt => BinaryOp::Gt,
                    RelOp::Le => BinaryOp::Le,
                    RelOp::Ge => BinaryOp::Ge,
                };
                exp2ir(op, rel_exp, add_exp, program, info)
            }
        }
    }
}

impl GenerateIR for EqExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Rel(exp) => {
                exp.generate_ir(program, info)
            }
            Self::Eq(eq_exp, eq_op, rel_exp) => {
                let eq_exp = eq_exp.as_ref();
                let op = match eq_op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::NotEq => BinaryOp::NotEq,
                };
                exp2ir(op, eq_exp, rel_exp, program, info)
            }
        }
    }
}

impl GenerateIR for LAndExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::Eq(exp) => {
                exp.generate_ir(program, info)
            }
            Self::LAnd(land_exp, eq_exp) => {
                land_exp.as_ref().generate_ir(program, info)?;
                let exp1 = info.context.value.unwrap();

                let func = info.context.function.unwrap();
                match program.func(func).dfg().value(exp1).kind() {
                    ValueKind::Integer(int) => {
                        let int_val = int.value();
                        if int_val == 0 {
                            return Ok(());
                        } else {
                            exp2ir(BinaryOp::NotEq, &Number::IntConst(0), eq_exp, program, info)?;
                        }
                    }
                    _ => {
                        info.if_cnt += 1;

                        let then_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some(format!("%then_{}", info.if_cnt)));
                        let end_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some(format!("%end_{}", info.if_cnt)));
                        program.func_mut(func).layout_mut().bbs_mut().extend([then_bb]);

                        let func_data = program.func_mut(func);
                        let result = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let store = func_data.dfg_mut().new_value().store(zero, result);

                        let func_data = program.func_mut(func);
                        let branch = func_data.dfg_mut().new_value().branch(exp1, then_bb, end_bb);
                        func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([result, store, branch]);

                        info.context.block = Some(then_bb);
                        exp2ir(BinaryOp::NotEq, &Number::IntConst(0), eq_exp, program, info)?;
                        let func_data = program.func_mut(func);
                        let store = func_data.dfg_mut().new_value().store(info.context.value.unwrap(), result);
                        let jump = func_data.dfg_mut().new_value().jump(end_bb);
                        func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store, jump]);

                        program.func_mut(func).layout_mut().bbs_mut().extend([end_bb]);
                        info.context.block = Some(end_bb);
                        let func_data = program.func_mut(func);
                        let load = func_data.dfg_mut().new_value().load(result);
                        func_data.layout_mut().bb_mut(end_bb).insts_mut().extend([load]);
                        info.context.value = Some(load);
                    }
                }
                Ok(())
            }
        }
    }
}

impl GenerateIR for LOrExp {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self {
            Self::LAnd(exp) => {
                exp.generate_ir(program, info)
            }
            Self::LOr(lor_exp, land_exp) => {
                lor_exp.as_ref().generate_ir(program, info)?;
                let exp1 = info.context.value.unwrap();

                let func = info.context.function.unwrap();
                match program.func(func).dfg().value(exp1).kind() {
                    ValueKind::Integer(int) => {
                        let int_val = int.value();
                        if int_val != 0 {
                            let func_data = program.func_mut(func);
                            let one = func_data.dfg_mut().new_value().integer(1);
                            info.context.value = Some(one);
                            return Ok(());
                        } else {
                            exp2ir(BinaryOp::NotEq, &Number::IntConst(0), land_exp, program, info)?;
                        }
                    }
                    _ => {
                        info.if_cnt += 1;

                        let then_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some(format!("%then_{}", info.if_cnt)));
                        let end_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some(format!("%end_{}", info.if_cnt)));
                        program.func_mut(func).layout_mut().bbs_mut().extend([then_bb]);

                        let func_data = program.func_mut(func);
                        let result = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                        let one = func_data.dfg_mut().new_value().integer(1);
                        let store = func_data.dfg_mut().new_value().store(one, result);

                        let func_data = program.func_mut(func);
                        let branch = func_data.dfg_mut().new_value().branch(exp1, end_bb, then_bb);
                        func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([result, store, branch]);

                        info.context.block = Some(then_bb);
                        exp2ir(BinaryOp::NotEq, &Number::IntConst(0), land_exp, program, info)?;
                        let func_data = program.func_mut(func);
                        let store = func_data.dfg_mut().new_value().store(info.context.value.unwrap(), result);
                        let jump = func_data.dfg_mut().new_value().jump(end_bb);
                        func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store, jump]);

                        program.func_mut(func).layout_mut().bbs_mut().extend([end_bb]);
                        info.context.block = Some(end_bb);
                        let func_data = program.func_mut(func);
                        let load = func_data.dfg_mut().new_value().load(result);
                        func_data.layout_mut().bb_mut(end_bb).insts_mut().extend([load]);
                        info.context.value = Some(load);
                    }
                }
                Ok(())
            }
        }
    }
}

impl Expression for Exp {}
impl Expression for PrimaryExp {}
impl Expression for Number {}
impl Expression for UnaryExp {}
impl Expression for MulExp {}
impl Expression for AddExp {}
impl Expression for RelExp {}
impl Expression for EqExp {}
impl Expression for LAndExp {}
impl Expression for LOrExp {}
