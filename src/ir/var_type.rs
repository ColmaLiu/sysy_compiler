use koopa::ir::{Program, TypeKind, Value};

use super::irinfo::IrInfo;

#[derive(Debug)]
pub enum VarType {
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

pub fn get_var_type(var: Value, is_glob: bool, program: &mut Program, info: &mut IrInfo) -> VarType {
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
