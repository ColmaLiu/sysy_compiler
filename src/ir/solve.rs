use crate::ast_type::{AddExp, AddOp, ConstExp, EqExp, EqOp, Exp, LAndExp, LOrExp, LVal, MulExp, MulOp, Number, PrimaryExp, RelExp, RelOp, UnaryExp, UnaryOp};
use super::irinfo::{IrInfo, Symbol};

pub trait Solve {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()>;
}

impl Solve for Exp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        self.lor_exp.solve(info)
    }
}

impl Solve for LVal {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        if let Some((Symbol::Const(_, int), _)) = info.symbol_table.get(&self.ident) {
            Ok(*int)
        } else {
            Err(())
        }
    }
}

impl Solve for PrimaryExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            PrimaryExp::Exp(exp) => exp.as_ref().solve(info),
            PrimaryExp::LVal(lval) => lval.solve(info),
            PrimaryExp::Num(num) => num.solve(info),
        }
    }
}

impl Solve for Number {
    fn solve(&self, _info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            Number::IntConst(int) => Ok(*int),
        }
    }
}

impl Solve for UnaryExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            UnaryExp::Primary(primary_exp) => {
                primary_exp.solve(info)
            }
            UnaryExp::Func(_, _) => {
                Err(())
            }
            UnaryExp::Unary(unary_op, unary_exp) => {
                let unary_exp = unary_exp.as_ref();
                match unary_op {
                    UnaryOp::Pos => unary_exp.solve(info),
                    UnaryOp::Neg => unary_exp.solve(info).map(|int| -int),
                    UnaryOp::Not => unary_exp.solve(info).map(|int| if int == 0 { 1 } else { 0 }),
                }
            }
        }
    }
}

impl Solve for MulExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            MulExp::Unary(exp) => {
                exp.solve(info)
            }
            MulExp::Mul(mul_exp, mul_op, unary_exp) => {
                let mul_exp = mul_exp.as_ref();
                let lhs = mul_exp.solve(info)?;
                let rhs = unary_exp.solve(info)?;
                match mul_op {
                    MulOp::Mul => Ok(lhs * rhs),
                    MulOp::Div => Ok(lhs / rhs),
                    MulOp::Mod => Ok(lhs % rhs),
                }
            }
        }
    }
}

impl Solve for AddExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            AddExp::Mul(exp) =>  {
                exp.solve(info)
            }
            AddExp::Add(add_exp, add_op, mul_exp) => {
                let add_exp = add_exp.as_ref();
                let lhs = add_exp.solve(info)?;
                let rhs = mul_exp.solve(info)?;
                match add_op {
                    AddOp::Add => Ok(lhs + rhs),
                    AddOp::Sub => Ok(lhs - rhs),
                }
            }
        }
    }
}

impl Solve for RelExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            RelExp::Add(exp) => {
                exp.solve(info)
            }
            RelExp::Rel(rel_exp, rel_op, add_exp) => {
                let rel_exp = rel_exp.as_ref();
                let lhs = rel_exp.solve(info)?;
                let rhs = add_exp.solve(info)?;
                match rel_op {
                    RelOp::Lt => Ok((lhs < rhs).into()),
                    RelOp::Gt => Ok((lhs > rhs).into()),
                    RelOp::Le => Ok((lhs <= rhs).into()),
                    RelOp::Ge => Ok((lhs >= rhs).into()),
                }
            }
        }
    }
}

impl Solve for EqExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            EqExp::Rel(exp) => {
                exp.solve(info)
            }
            EqExp::Eq(eq_exp, eq_op, rel_exp) => {
                let eq_exp = eq_exp.as_ref();
                let lhs = eq_exp.solve(info)?;
                let rhs = rel_exp.solve(info)?;
                match eq_op {
                    EqOp::Eq => Ok((lhs == rhs).into()),
                    EqOp::NotEq => Ok((lhs != rhs).into()),
                }
            }
        }
    }
}

impl Solve for LAndExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            LAndExp::Eq(exp) => {
                exp.solve(info)
            }
            LAndExp::LAnd(land_exp, eq_exp) => {
                let land_exp = land_exp.as_ref();
                let lhs = land_exp.solve(info)?;
                let rhs = eq_exp.solve(info)?;
                Ok((lhs != 0 && rhs != 0).into())
            }
        }
    }
}

impl Solve for LOrExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        match self {
            LOrExp::LAnd(exp) => {
                exp.solve(info)
            }
            LOrExp::LOr(lor_exp, land_exp) => {
                let lor_exp = lor_exp.as_ref();
                let lhs = lor_exp.solve(info)?;
                let rhs = land_exp.solve(info)?;
                Ok(((lhs | rhs) != 0).into())
            }
        }
    }
}

impl Solve for ConstExp {
    fn solve(&self, info: &mut IrInfo) -> Result<i32, ()> {
        self.exp.solve(info)
    }
}
