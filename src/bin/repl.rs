extern crate rawrlang;
use rawrlang::{vm::VM, compiler::compile};

fn main() {
    let mut input = String::new();
    use std::io::{self, Write};

    loop {
        print!(">>> ");
        io::stdout().flush().unwrap();
        input.clear();
        io::stdin().read_line(&mut input).unwrap();

        match input.trim() {
            i if i.is_empty() => continue,
            ".quit" => break,
            src => {
                let prog = compile(src).unwrap();
                // println!("{:#?}", prog);
                print!("{}", prog);
                let mut vm = VM::new(&prog);
                vm.run().unwrap();
                // vm.print_stack();
            },
        }

    }
}