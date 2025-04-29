use crate::ast_type::*;
use koopa::ir::{builder::{GlobalInstBuilder, LocalInstBuilder, ValueBuilder}, FunctionData, Program, Type, Value};
use super::{ir::GenerateIR, irinfo::IrInfo, solve::Solve};

#[derive(Clone, Debug)]
pub enum Elem {
    Const(i32),
    Var(Value),
}

fn aggregate(program: &mut Program, flatten_list: &[Elem], shape: &[usize]) -> Value {
    let mut elems = Vec::new();
    if shape.len() == 1 {
        for elem in flatten_list {
            match elem {
                Elem::Const(int) => elems.push(program.new_value().integer(*int)),
                Elem::Var(var) => elems.push(*var),
            }
        }
    } else {
        let numel = flatten_list.len();
        let size = shape[0];
        let numel_ = numel / size;
        for index in 0..size {
            elems.push(aggregate(program, &flatten_list[(index * numel_)..((index + 1) * numel_)], &shape[1..]));
        }
    }
    program.new_value().aggregate(elems)
}

fn store(func_data: &mut FunctionData, info: &mut IrInfo, array: Value, flatten_list: &[Elem], shape: &[usize]) {
    if shape.len() == 0 {
        let value = match flatten_list[0] {
            Elem::Const(int) => func_data.dfg_mut().new_value().integer(int),
            Elem::Var(var) => var,
        };
        let store = func_data.dfg_mut().new_value().store(value, array);
        func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store]);
    } else {
        let numel = flatten_list.len();
        let size = shape[0];
        let numel_ = numel / size;
        for index in 0..shape[0] {
            let index_value = func_data.dfg_mut().new_value().integer(index as i32);
            let get = func_data.dfg_mut().new_value().get_elem_ptr(array, index_value);
            func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([get]);
            store(func_data, info, get, &flatten_list[(index * numel_)..((index + 1) * numel_)], &shape[1..]);
        }
    }
}

impl InitVal {
    pub fn init(&self, program: &mut Program, info: &mut IrInfo, shape: &[usize], ident: &String, uninit: bool) -> Result<(), String> {
        match self {
            InitVal::Array(init_val_vec) => {
                if init_val_vec.is_empty() && info.symbol_table.depth() == 0 {
                    let mut ty = Type::get_i32();
                    for len in shape.iter().rev() {
                        ty = Type::get_array(ty, *len);
                    }
                    let init = program.new_value().zero_init(ty);
                    let array = program.new_value().global_alloc(init);
                    program.set_value_name(array, Some(format!("@{}", ident)));
                    if info.symbol_table.insert(
                        ident.to_string(), super::irinfo::Symbol::Var(ident.to_string(), array)
                    ).is_some() {
                        return Err("Redefined symbol".to_string());
                    }
                    return Ok(());
                }
                let mut numel = 1;
                for len in shape {
                    numel *= len;
                }
                let mut flatten_list = vec![Elem::Const(0); numel];
                self.init_(program, info, shape, &mut flatten_list[0..])?;
                if info.symbol_table.depth() == 0 {
                    let init = aggregate(program, &flatten_list, shape);
                    let array = program.new_value().global_alloc(init);
                    program.set_value_name(array, Some(format!("@{}", ident)));
                    if info.symbol_table.insert(
                        ident.to_string(), super::irinfo::Symbol::Var(ident.to_string(), array)
                    ).is_some() {
                        return Err("Redefined symbol".to_string());
                    }
                } else {
                    let func = info.context.function.unwrap();
                    let func_data = program.func_mut(func);
                    let mut ty = Type::get_i32();
                    for len in shape.iter().rev() {
                        ty = Type::get_array(ty, *len);
                    }
                    let array = func_data.dfg_mut().new_value().alloc(ty);
                    func_data.dfg_mut().set_value_name(array, Some(format!("@{}", ident)));
                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([array]);
                    if info.symbol_table.insert(
                        ident.to_string(), super::irinfo::Symbol::Var(ident.to_string(), array)
                    ).is_some() {
                        return Err("Redefined symbol".to_string());
                    }
                    if !uninit {
                        store(func_data, info, array, &flatten_list, shape);
                    }
                }
            }
            InitVal::Int(_) => unreachable!(),
        }
        Ok(())
    }

    fn init_(&self, program: &mut Program, info: &mut IrInfo, shape: &[usize], flatten_list: &mut [Elem]) -> Result<(), String> {
        match self {
            InitVal::Int(exp) => {
                if shape.len() == 0 && flatten_list.len() == 1 {
                    let elem = match exp.solve(info) {
                        Ok(int) => {
                            Elem::Const(int)
                        }
                        Err(()) => {
                            exp.generate_ir(program, info)?;
                            let value = info.context.value.unwrap();
                            Elem::Var(value)
                        }
                    };
                    flatten_list[0] = elem;
                } else {
                    return Err("Illegal initializer".to_string());
                }
            }
            InitVal::Array(init_val_vec) => {
                let mut curr_index = 0;
                for init_val in init_val_vec {
                    match init_val {
                        InitVal::Int(_) => {
                            init_val.init_(program, info, &[], &mut flatten_list[curr_index..curr_index + 1])?;
                            curr_index += 1;
                        }
                        InitVal::Array(_) => {
                            let mut dim = shape.len();
                            let mut curr_index_ = curr_index;
                            while dim > 1 && curr_index_ % shape[dim - 1] == 0 {
                                curr_index_ /= shape[dim - 1];
                                dim -= 1;
                            }
                            if dim == shape.len() {
                                return Err("Illegal initializer".to_string());
                            }
                            let mut numel = 1;
                            for len in &shape[dim..] {
                                numel *= len;
                            }
                            init_val.init_(program, info, &shape[dim..], &mut flatten_list[curr_index..(curr_index + numel)])?;
                            curr_index += numel;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
