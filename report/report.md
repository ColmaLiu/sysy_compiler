# 编译原理课程实践报告：sysy_compiler

信息科学技术学院 2300012993 刘雨诺

## 一、编译器概述

### 1.1 基本功能

本编译器基本具备如下功能：

1. 将 SysY 语言编译到 Koopa IR
2. 将 Koopa IR 编译到 RISC-V 汇编

### 1.2 主要特点

本编译器的主要特点是：基于 Rust 语言实现，使用了 Koopa IR 作为中间表示，支持 SysY 语言的基本语法和语义，能够处理变量作用域、函数调用、数组等特性。

## 二、编译器设计

### 2.1 主要模块组成

编译器由 3 个主要模块组成：

- 词法分析和语法分析：基于 `lalrpop` 实现，负责将 SysY 源代码转换为抽象语法树（AST）。
- 中间代码生成：将 AST 转换为 Koopa IR，使用 `koopa` 库进行 IR 的生成和优化。
- 目标代码生成：将 Koopa IR 转换为 RISC-V 汇编代码。

### 2.2 主要数据结构

### 2.2.1 词法分析和语法分析

AST 的定义参考了在线文档中的介绍，各种 AST 节点类型的定义在 `ast_type.rs` 中。

### 2.2.2 中间代码生成

我定义了一个 `GenerateIR` trait 来为 AST 中的每个节点生成中间代码。每个 AST 节点都实现了这个 trait，具体的生成逻辑在各自的实现中完成。

```rust
pub trait GenerateIR {
    fn generate_ir(&self, program: &mut Program, info: &mut IrInfo) -> Result<(), String>;
}
```

其中 `IrInfo` 用于存储中间代码生成过程中的上下文信息，包括当前函数、基本块、符号表等，后面对其中的各部分有更详细的介绍。

```rust
#[derive(Default)]
pub struct IrInfo {
    pub context: Context,
    pub symbol_table: SymbolTable,
    pub if_cnt: usize,
    pub while_cnt: usize,
    pub while_info: Vec<WhileBlockInfo>,
}
```

### 2.2.3 目标代码生成

我为 `Program` 和 `FunctionData` 实现了 `GenerateAsm` trait，用于将 Koopa IR 转换为 RISC-V 汇编代码。

```rust
pub trait GenerateAsm {
    fn generate_asm(&self, program: &Program, info: &mut AsmInfo) -> Result<(), String>;
}
```

其中 `AsmInfo` 用于存储目标代码生成过程中的上下文信息，包括当前函数、基本块、寄存器使用情况等。

```rust
#[derive(Default)]
pub struct AsmInfo {
    pub reg: [Option<Value>; 32],
    pub stack: HashMap<Value, usize>,
    pub function: HashMap<Function, String>,
    pub glob_var: HashMap<Value, (String, usize)>,// (ident, elem_size)
    pub ra_used: bool,
}
```

### 2.3 主要设计考虑及算法选择

#### 2.3.1 符号表的设计考虑

在中间代码生成阶段，符号表用于存储变量和函数的信息。符号表的设计考虑了作用域嵌套的问题，定义如下：

```rust
pub enum Symbol {
    Const(String, i32),
    Var(String, Value),
    Func(String, Function),
}

#[derive(Default)]
pub struct SymbolTable {
    symbol_table_stack: Vec<HashMap<String, Symbol>>,
    is_entry: bool,
}
```

其中`Symbol`枚举类型表示符号的不同类型，包括常量、变量和函数。`SymbolTable`结构体使用 `Vec` 模拟栈来存储符号表，以支持作用域的嵌套。每当进入新的作用域时，使用 `push_table` 创建一个新的符号表。当离开作用域时，使用 `pop_table` 销毁当前符号表。

#### 2.3.2 寄存器分配策略

在栈上为所有局部变量分配空间，在 `AsmInfo` 中，`reg` 用于记录寄存器的使用情况，当寄存器中的变量不再使用时，及时释放寄存器。

## 三、编译器实现

### 3.1 各阶段编码细节

#### Lv1. main函数和Lv2. 初试目标代码生成

这一阶段主要建立项目框架，搭建基本的模块结构。

#### Lv3. 表达式

为 AST 中所有代表表达式的结构体实现 `Expression` trait ，实现了 `Expression` trait 的结构体可以通过 `exp2ir` 和 `val2ir` 递归求值。

```rust
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
```

#### Lv4. 常量和变量

这部分的难点主要在于目标代码生成中的寄存器分配，在线文档中的讲解十分详细，这里不再赘述。

在这里的测试点中遇到了函数中有多个 return 语句的情况。为了解决这个问题，在 `Context` 中加入 `exited` 来跟踪当前的基本块是否已退出。

另外，由于我对 Rust 不是很熟悉，将按位取反操作符 `!` 误认为是同 C 中一样的逻辑非操作符，导致在处理逻辑非操作时出现了错误，花费了一些时间查找错误。

#### Lv5. 语句块和作用域

作用域的实现相对简单，只需将先前的单一符号表改为符号表栈即可。前面已经讲过符号表的设计考虑，这里不再赘述。

#### Lv6. if语句

这部分主要需要处理 if 语句经典的 dangling else 问题。我将 `Stmt` 分为 `OpenStmt` 和 `ClosedStmt` 两种类型，分别处理开放和封闭的 if 语句。注意到这样的处理方法只需修改 lalrpop 的部分即可，不需要修改 AST 。

```rust
Stmt: Stmt = {
    <OpenStmt> => <>,
    <ClosedStmt> => <>,
}

OpenStmt: Stmt = {
    "if" "(" <exp: Exp> ")" <stmt: Stmt> => Stmt::If(exp, Box::new(stmt), None),
    "if" "(" <exp: Exp> ")" <closed_stmt: ClosedStmt> "else" <open_stmt: OpenStmt> => {
        Stmt::If(exp, Box::new(closed_stmt), Some(Box::new(open_stmt)))
    },
    "while" "(" <exp: Exp> ")" <open_stmt: OpenStmt> => {
        Stmt::While(exp, Box::new(open_stmt))
    },
}

ClosedStmt: Stmt = {
    <SimpleStmt> => <>,
    "if" "(" <exp: Exp> ")" <closed_stmt1: ClosedStmt> "else" <closed_stmt2: ClosedStmt> => {
        Stmt::If(exp, Box::new(closed_stmt1), Some(Box::new(closed_stmt2)))
    },
    "while" "(" <exp: Exp> ")" <closed_stmt: ClosedStmt> => {
        Stmt::While(exp, Box::new(closed_stmt))
    },
}

SimpleStmt: Stmt = {
    <LVal> "=" <Exp> ";" => Stmt::Assign(<>),
    <Exp?> ";" => Stmt::Exp(<>),
    <Block> => Stmt::Block(<>),
    "break" ";" => Stmt::Break,
    "continue" ";" => Stmt::Continue,
    "return" <Exp?> ";" => Stmt::Return(<>),
}
```

这里遇到了一个函数末尾的基本块可能没有 `return` 语句的问题。对于这个问题，只需检查 `Context` 中的 `exited` 字段，若显示当前基本块未退出，则手动插入一个 `ret` 即可。

我在 `IrInfo` 中添加了一个 `if_cnt` 字段来跟踪当前 if 语句的数量，并在生成中间代码时使用该计数器来生成唯一的基本块名称。在后面的部分中又添加了一个 `while_cnt` 字段来跟踪 while 语句的数量，作用相同。

#### Lv7. while语句

这部分的处理与 if 语句类似，主要是处理 while 语句的循环结构，因此难度不高。

对于 break 和 continue 语句，我定义了一个 `WhileBlockInfo` 结构体用来存储当前 while 循环的基本块信息，包括循环的入口和出口基本块，然后在 `IrInfo` 中添加新字段 `while_info`。这样在处理 break 和 continue 时，可以获取应该跳转到的基本块信息。

```rust
pub struct WhileBlockInfo {
    entry_bb: BasicBlock,
    end_bb: BasicBlock,
}

#[derive(Default)]
pub struct IrInfo {
    /* … */
    pub while_info: Vec<WhileBlockInfo>,
}
```

#### Lv8. 函数和全局变量

这里如果按照完全在线文档中的语法规范实现，会出现归约-归约冲突，问题主要出现在 "int" 可以同时归约为 `BType` 和 `FuncType` ：

```txt
BType         ::= "int";
VarDecl       ::= BType VarDef {"," VarDef} ";";

FuncDef       ::= FuncType IDENT "(" [FuncFParams] ")" Block;
FuncType      ::= "void" | "int";
```

因此我在实现中将 `BType` 和 `FuncType` 展开：

```txt
VarDecl       ::= "int" VarDef {"," VarDef} ";";

FuncDef       ::= "void" IDENT "(" [FuncFParams] ")" Block
                | "int" IDENT "(" [FuncFParams] ")" Block;
```

#### Lv9. 数组

中间代码生成阶段主要的难点在于数组的初始化。我定义了一个 `Elem` 枚举类型代表一个数组元素，创建一个与数组变量相同大小的展平的 `Elem` 数组，然后根据初始化列表计算相应的元素下标并修改对应的值，最终根据这个 `Elem` 数组生成相应的中间代码，如果是全局数组则使用 `aggregate` 函数生成代码，如果是局部数组则使用 `store` 函数生成代码。

```rust
#[derive(Clone, Debug)]
pub enum Elem {
    Const(i32),
    Var(Value),
}

fn aggregate(program: &mut Program, flatten_list: &[Elem], shape: &[usize]) -> Value {
    /* … */
}

fn store(func_data: &mut FunctionData, info: &mut IrInfo, array: Value, flatten_list: &[Elem], shape: &[usize]) {
    /* … */
}

impl InitVal {
    pub fn init(&self, program: &mut Program, info: &mut IrInfo, shape: &[usize], ident: &String, uninit: bool) -> Result<(), String> {
        match self {
            InitVal::Array(init_val_vec) => {
                let mut numel = 1;
                for len in shape {
                    numel *= len;
                }
                let mut flatten_list = vec![Elem::Const(0); numel];
                self.init_(program, info, shape, &mut flatten_list[0..])?;
                if info.symbol_table.depth() == 0 {
                    let init = aggregate(program, &flatten_list, shape);
                    /* … */
                } else {
                    /* … */
                    let func = info.context.function.unwrap();
                    let func_data = program.func_mut(func);
                    let mut ty = Type::get_i32();
                    for len in shape.iter().rev() {
                        ty = Type::get_array(ty, *len);
                    }
                    let array = func_data.dfg_mut().new_value().alloc(ty);
                    /* … */
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
        /* 递归处理flatten_list */
    }
}
```

### 3.2 工具软件介绍

1. `lalrpop`：基于 Rust 的 LR(1) 语法分析器生成器（尽管以 LALR 命名），用于词法和语法分析。
2. `koopa`：一个中间表示（IR）库，用于生成，解析和优化 Koopa IR。

## 四、实习总结

### 4.1 收获和体会

这个项目加深了我对编译原理的理解，特别是在词法分析、语法分析和中间代码生成等方面的实践经验。我学会了如何使用 `lalrpop` 和 `koopa` 等工具来实现编译器的各个阶段，并在实际编码中遇到并解决了许多问题。

这个项目是我第一个使用 Rust 语言编写的大型项目。通过这个项目，我对 Rust 的所有权系统、生命周期和错误处理有了更深入的理解。

此外，在项目中我还学习了如何使用 Docker 进行环境管理和部署。

### 4.2 学习过程中的难点，以及对实习过程和内容的建议

在线文档中的介绍非常详细，对项目的完成帮助很大。

优化同样是编译器中一个重要的环节，而在线文档中对优化的提及相对较少且靠后，导致实现时可能需要较大程度的重构，存在一定难度。希望在线文档能在优化方面提供更多的指导和示例。
