mod asm;
mod ast_type;
mod ir;

use asm::asm::GenerateAsm;
use asm::asminfo::AsmInfo;
use ir::irinfo::IrInfo;
use ir::ir::GenerateIR;
use koopa::back::KoopaGenerator;
use koopa::ir::{Program, Type};
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{self, read_to_string, File};
use std::io::Write;

lalrpop_mod!(sysy);

fn main() -> Result<(), String> {
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input).map_err(
        |e| {e.to_string()}
    )?;

    match mode.as_str() {
        "-koopa" | "-riscv" => Ok(()),
        _ => Err("Unexpected mode"),
    }?;

    let ast = sysy::CompUnitParser::new().parse(&input).map_err(
        |e| {e.to_string()}
    )?;

    let mut program = Program::new();
    ast.generate_ir(&mut program, &mut IrInfo::new())?;

    if mode == "-koopa" {
        let mut generator = KoopaGenerator::new(Vec::new());
        generator.generate_on(&program).unwrap();
        fs::write(output, generator.writer()).map_err(
            |_e| {"Writing Koopa IR failed"}
        )?;
    } else if mode == "-riscv" {
        Type::set_ptr_size(4);
        let mut asm = Vec::new();
        program.generate_asm(&mut asm, &mut AsmInfo::new())?;
        let mut file = File::create(output).map_err(
            |_e| {"Opening output file failed"}
        )?;
        for line in asm {
            writeln!(file, "{}", line).map_err(
                |_e| {"Writing RISC-V assemble failed"}
            )?;
        }
    }

    Ok(())
}
