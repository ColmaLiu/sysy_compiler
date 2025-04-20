pub mod asm;
pub mod asminfo;

const REGISTER: [&str; 32] = [
    "x0", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "fp", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6",
];

const TEMP_IDX: [usize; 7] = [5, 6, 7, 28, 29, 30, 31];
