use crate::ast_type::*;
use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, BinaryOp, FunctionData, Program, Type, Value, ValueKind};
use super::{irinfo::{Context, IrInfo, Symbol}, solve::Solve};

pub trait GenerateIR {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String>;
}

trait Expression: GenerateIR {}

impl GenerateIR for CompUnit {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        self.func_def.generate_ir(program, info)
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
        match self.b_type {
            BType::Int => {
                for const_def in &self.const_defs {
                    const_def.generate_ir(program, info)?;
                }
            }
        }
        Ok(())
    }
}

impl GenerateIR for ConstDef {
    fn generate_ir(&self, _program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        let int = self.const_init_val.const_exp.solve(info).map_err(|_err| "Unknown value during compile time")?;
        if info.symbol_table.insert(
            self.ident.clone(), super::irinfo::Symbol::Const(self.ident.clone(), int)
        ).is_none() {
            Ok(())
        } else {
            Err("Redefined symbol".to_string())
        }
    }
}

impl GenerateIR for VarDecl {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        match self.b_type {
            BType::Int => {
                for var_def in &self.var_defs {
                    var_def.generate_ir(program, info)?;
                }
            }
        }
        Ok(())
    }
}

impl GenerateIR for VarDef {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
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
        Ok(())
    }
}

impl GenerateIR for InitVal {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        self.exp.generate_ir(program, info)
    }
}

impl GenerateIR for FuncDef {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        let ret_ty = match self.func_type {
            FuncType::Int => Type::get_i32(),
        };
        let func = program.new_func(
            FunctionData::new(
                format!("@{}", self.ident.as_str()), vec![], ret_ty
            )
        );

        let func_data = program.func_mut(func);
        let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        info.context.function = Some(func);
        info.context.block = Some(entry);

        self.block.generate_ir(program, info)?;

        Ok(())
    }
}

impl GenerateIR for Block {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String> {
        info.symbol_table.push_table();
        for block_item in &self.block_items {
            if info.context.exited == true {
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

                match info.symbol_table.get(&lval.ident) {
                    Some(Symbol::Var(_, var)) => {
                        let func_data = program.func_mut(info.context.function.unwrap());
                        let store = func_data.dfg_mut().new_value().store(exp_val, *var);
                        func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([store]);
                    }
                    Some(Symbol::Const(_, _)) => {
                        return Err("Constant can not be a left value".to_string());
                    }
                    None => {
                        return Err("Undefined symbol".to_string());
                    }
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
                exp.generate_ir(program, info)?;

                let func = info.context.function.unwrap();

                let func_data = program.func_mut(func);
                let end_bb = func_data.dfg_mut().new_bb().basic_block(Some("%end".to_string()));

                let start_bb = info.context.block.unwrap();
                let cond = info.context.value.unwrap();

                let func_data = program.func_mut(func);
                let true_bb = func_data.dfg_mut().new_bb().basic_block(Some("%then".to_string()));
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
                        let else_bb = func_data.dfg_mut().new_bb().basic_block(Some("%else".to_string()));
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
            Self::Return(exp) => {
                if info.context.exited == true {
                    return Ok(());
                }
                if let Some(exp) = exp {
                    exp.generate_ir(program, info)?;

                    let func_data = program.func_mut(info.context.function.unwrap());
                    let ret = func_data.dfg_mut().new_value().ret(info.context.value);
                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([ret]);
                }

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
        let func_data = program.func_mut(info.context.function.unwrap());
        if let Some(symbol) = info.symbol_table.get(&self.ident) {
            match symbol {
                Symbol::Const(_, int) => {
                    let value = func_data.dfg_mut().new_value().integer(*int);
                    info.context.value = Some(value);
                }
                Symbol::Var(_, var) => {
                    let load = func_data.dfg_mut().new_value().load(*var);
                    func_data.layout_mut().bb_mut(info.context.block.unwrap()).insts_mut().extend([load]);
                    info.context.value = Some(load);
                }
            }
            Ok(())
        } else {
            Err("Undefined symbol".to_string())
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
                        let then_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some("%then".to_string()));
                        let end_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some("%end".to_string()));
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
                        let then_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some("%then".to_string()));
                        let end_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some("%end".to_string()));
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
