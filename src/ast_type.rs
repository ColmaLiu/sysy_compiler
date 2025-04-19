#[derive(Debug)]
pub struct CompUnit {
    pub comp_unit_items: Vec<CompUnitItem>,
}

#[derive(Debug)]
pub enum CompUnitItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

#[derive(Debug)]
pub enum Decl {
    Const(ConstDecl),
    Var(VarDecl),
}

#[derive(Debug)]
pub struct ConstDecl {
    pub const_defs: Vec<ConstDef>,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}

#[derive(Debug)]
pub struct VarDecl {
    pub var_defs: Vec<VarDef>,
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub init_val: Option<InitVal>,
}

#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub params: Option<FuncFParams>,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Void,
    Int,
}

#[derive(Debug)]
pub struct FuncFParams {
    pub params: Vec<FuncFParam>,
}

#[derive(Debug)]
pub struct FuncFParam {
    pub ident: String,
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Exp),
    Exp(Option<Exp>),
    Block(Block),
    If(Exp, Box<Stmt>, Option<Box<Stmt>>),
    While(Exp, Box<Stmt>),
    Break,
    Continue,
    Return(Option<Exp>),
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Num(Number),
}

#[derive(Debug)]
pub enum Number {
    IntConst(i32),
}

#[derive(Debug)]
pub enum UnaryExp {
    Primary(PrimaryExp),
    Func(String, Option<FuncRParams>),
    Unary(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug)]
pub struct FuncRParams {
    pub params: Vec<Exp>,
}

#[derive(Debug)]
pub enum MulExp {
    Unary(UnaryExp),
    Mul(Box<MulExp>, MulOp, UnaryExp),
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum AddExp {
    Mul(MulExp),
    Add(Box<AddExp>, AddOp, MulExp),
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub enum RelExp {
    Add(AddExp),
    Rel(Box<RelExp>, RelOp, AddExp),
}

#[derive(Debug)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug)]
pub enum EqExp {
    Rel(RelExp),
    Eq(Box<EqExp>, EqOp, RelExp),
}

#[derive(Debug)]
pub enum EqOp {
    Eq,
    NotEq,
}

#[derive(Debug)]
pub enum LAndExp {
    Eq(EqExp),
    LAnd(Box<LAndExp>, EqExp),
}

#[derive(Debug)]
pub enum LOrExp {
    LAnd(LAndExp),
    LOr(Box<LOrExp>, LAndExp),
}

#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
}
