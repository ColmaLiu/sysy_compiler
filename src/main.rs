use gen::IRGenerator;
use koopa::back::KoopaGenerator;
use koopa::ir::Program;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{self, read_to_string};
use std::io::Result;

lalrpop_mod!(sysy);

mod ast;
mod gen;

fn main() -> Result<()> {
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input)?;

    let ast = sysy::CompUnitParser::new().parse(&input).expect("Parsing failed");

    let mut program = Program::new();
    ast.generate_ir(&mut program).expect("Generating Koopa IR failed");

    let mut generator = KoopaGenerator::new(Vec::new());
    generator.generate_on(&program)?;

    fs::write(output, generator.writer()).expect("Writing Koopa IR failed");

    Ok(())
}
