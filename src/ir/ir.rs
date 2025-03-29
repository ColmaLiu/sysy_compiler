use crate::ast_type::*;
use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, BinaryOp, FunctionData, Program, Type, Value};
use super::context::Context;

pub trait GenerateIR {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String>;
}

trait Expression: GenerateIR {}

impl GenerateIR for CompUnit {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        self.func_def.generate_ir(program, context)?;
        Ok(())
    }
}

impl GenerateIR for FuncDef {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
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

        context.function = Some(func);
        context.block = Some(entry);

        self.block.generate_ir(program, context)?;

        Ok(())
    }
}

impl GenerateIR for Block {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        self.stmt.generate_ir(program, context)
    }
}

impl GenerateIR for Stmt {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        self.exp.generate_ir(program, context)?;

        let func_data = program.func_mut(context.function.unwrap());
        let ret = func_data.dfg_mut().new_value().ret(context.value);
        func_data.layout_mut().bb_mut(context.block.unwrap()).insts_mut().extend([ret]);

        Ok(())
    }
}

impl GenerateIR for Exp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        self.lor_exp.generate_ir(program, context)
    }
}

impl GenerateIR for PrimaryExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            PrimaryExp::Exp(exp) => exp.as_ref().generate_ir(program, context),
            PrimaryExp::Num(num) => num.generate_ir(program, context),
        }
    }
}

impl GenerateIR for Number {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        let func_data = program.func_mut(context.function.unwrap());
        match self {
            Number::IntConst(int) => {
                let value = func_data.dfg_mut().new_value().integer(*int);
                context.value = Some(value);
            }
        }
        Ok(())
    }
}

impl GenerateIR for UnaryExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            UnaryExp::Primary(primary_exp) => {
                primary_exp.generate_ir(program, context)
            }
            UnaryExp::Unary(unary_op, unary_exp) => {
                let unary_exp = unary_exp.as_ref();
                match unary_op {
                    UnaryOp::Pos => unary_exp.generate_ir(program, context),
                    UnaryOp::Neg => exp2ir(BinaryOp::Sub, &Number::IntConst(0), unary_exp, program, context),
                    UnaryOp::Not => exp2ir(BinaryOp::Eq, &Number::IntConst(0), unary_exp, program, context),
                }
            }
        }
    }
}

impl GenerateIR for MulExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            MulExp::Unary(exp) => {
                exp.generate_ir(program, context)
            }
            MulExp::Mul(mul_exp, mul_op, unary_exp) => {
                let mul_exp = mul_exp.as_ref();
                let op = match mul_op {
                    MulOp::Mul => BinaryOp::Mul,
                    MulOp::Div => BinaryOp::Div,
                    MulOp::Mod => BinaryOp::Mod,
                };
                exp2ir(op, mul_exp, unary_exp, program, context)
            }
        }
    }
}

impl GenerateIR for AddExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            AddExp::Mul(exp) => {
                exp.generate_ir(program, context)
            }
            AddExp::Add(add_exp, add_op, mul_exp) => {
                let add_exp = add_exp.as_ref();
                let op = match add_op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                };
                exp2ir(op, add_exp, mul_exp, program, context)
            }
        }
    }
}

impl GenerateIR for RelExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            RelExp::Add(exp) => {
                exp.generate_ir(program, context)
            }
            RelExp::Rel(rel_exp, rel_op, add_exp) => {
                let rel_exp = rel_exp.as_ref();
                let op = match rel_op {
                    RelOp::Lt => BinaryOp::Lt,
                    RelOp::Gt => BinaryOp::Gt,
                    RelOp::Le => BinaryOp::Le,
                    RelOp::Ge => BinaryOp::Ge,
                };
                exp2ir(op, rel_exp, add_exp, program, context)
            }
        }
    }
}

impl GenerateIR for EqExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            EqExp::Rel(exp) => {
                exp.generate_ir(program, context)
            }
            EqExp::Eq(eq_exp, eq_op, rel_exp) => {
                let eq_exp = eq_exp.as_ref();
                let op = match eq_op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::NotEq => BinaryOp::NotEq,
                };
                exp2ir(op, eq_exp, rel_exp, program, context)
            }
        }
    }
}

impl GenerateIR for LAndExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            LAndExp::Eq(exp) => {
                exp.generate_ir(program, context)
            }
            LAndExp::LAnd(land_exp, eq_exp) => {
                let land_exp = land_exp.as_ref();
                exp2ir(BinaryOp::NotEq, &Number::IntConst(0), land_exp, program, context)?;
                let lhs = context.value.unwrap();
                exp2ir(BinaryOp::NotEq, &Number::IntConst(0), eq_exp, program, context)?;
                let rhs = context.value.unwrap();
                val2ir(BinaryOp::And, lhs, rhs, program, context);
                Ok(())
            }
        }
    }
}

impl GenerateIR for LOrExp {
    fn generate_ir(&self, program: &mut Program, context: &mut Context) -> Result<(), String> {
        match self {
            LOrExp::LAnd(exp) => {
                exp.generate_ir(program, context)
            }
            LOrExp::LOr(lor_exp, land_exp) => {
                let lor_exp = lor_exp.as_ref();
                exp2ir(BinaryOp::Or, lor_exp, land_exp, program, context)?;
                let bitor_value = context.value.unwrap();
                Number::IntConst(0).generate_ir(program, context)?;
                let zero = context.value.unwrap();
                val2ir(BinaryOp::NotEq, bitor_value, zero, program, context);
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
    context: &mut Context,
) -> Result<(), String> {
    lexp.generate_ir(program, context)?;
    let lhs = context.value.unwrap();
    rexp.generate_ir(program, context)?;
    let rhs = context.value.unwrap();
    val2ir(op, lhs, rhs, program, context);
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
