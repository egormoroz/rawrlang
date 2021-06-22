extern crate rawrlang;
use rawrlang::{vm::VM, compiler::compile};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.len() != 1 {
        println!("usage: <single> <path-to-file>");
    } else {
        let src = std::fs::read_to_string(&args[0]).expect("failed to load file");
        let prog = compile(&src).unwrap();
        let mut vm = VM::new(&prog);
        vm.run().unwrap();
    }
}
