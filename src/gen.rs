use crate::ast::*;
use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, FunctionData, Program, Type};

pub trait GenerateIR {
    type R;

    fn generate_ir(&self, program: &mut Program) -> Result<Self::R, ()>;
}

impl GenerateIR for CompUnit {
    type R = ();

    fn generate_ir(&self, program: &mut Program) -> Result<Self::R, ()> {
        self.func_def.generate_ir(program)?;
        Ok(())
    }
}

impl GenerateIR for FuncDef {
    type R = ();

    fn generate_ir(&self, program: &mut Program) -> Result<Self::R, ()> {
        let ret_ty = self.func_type.generate_ir(program)?;
        let func = program.new_func(
            FunctionData::new(
                format!("@{}", self.ident.as_str()), vec![], ret_ty
            )
        );
        let num = self.block.generate_ir(program)?;

        let func_data = program.func_mut(func);
        let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);

        let value = func_data.dfg_mut().new_value().integer(num);
        let ret = func_data.dfg_mut().new_value().ret(Some(value));
        func_data.layout_mut().bb_mut(entry).insts_mut().extend([ret]);
        Ok(())
    }
}

impl GenerateIR for FuncType {
    type R = Type;

    fn generate_ir(&self, program: &mut Program) -> Result<Self::R, ()> {
        match self {
            Self::Int => Ok(Type::get_i32()),
        }
    }
}

impl GenerateIR for Block {
    type R = i32;

    fn generate_ir(&self, program: &mut Program) -> Result<Self::R, ()> {
        self.stmt.generate_ir(program)
    }
}

impl GenerateIR for Stmt {
    type R = i32;

    fn generate_ir(&self, program: &mut Program) -> Result<Self::R, ()> {
        Ok(self.num)
    }
}
