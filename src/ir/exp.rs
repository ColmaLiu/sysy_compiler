use crate::ast_type::*;
use koopa::ir::{builder::LocalInstBuilder, BinaryOp, Program, Value};
use super::{ir::GenerateIR, irinfo::{Context, IrInfo}};

pub trait Expression: GenerateIR {}

pub fn exp2ir(
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
