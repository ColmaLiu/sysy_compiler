use asm::GenerateAsm;
use gen::GenerateIR;
use koopa::back::KoopaGenerator;
use koopa::ir::Program;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{self, read_to_string, File};
use std::io::Write;

lalrpop_mod!(sysy);

mod asm;
mod ast;
mod gen;

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

    // let ast = sysy::CompUnitParser::new().parse(&input).expect("Parsing failed");
    let ast = sysy::CompUnitParser::new().parse(&input).map_err(
        |e| {e.to_string()}
    )?;

    let mut program = Program::new();
    ast.generate_ir(&mut program).map_err(
        |_e| {"Generating Koopa IR failed"}
    )?;

    if mode == "-koopa" {
        let mut generator = KoopaGenerator::new(Vec::new());
        generator.generate_on(&program).unwrap();
        fs::write(output, generator.writer()).map_err(
            |_e| {"Writing Koopa IR failed"}
        )?;
    } else if mode == "-riscv" {
        let asm = program.generate_asm()?;
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
