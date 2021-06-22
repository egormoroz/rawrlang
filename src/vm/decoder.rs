use super::value::Value;
use super::mem::Object;
use super::program::Program;

use super::opcode::*;
use std::fmt;

#[derive(Debug)]
pub enum DecodeError {
    NotEnoughBytes {
        got: usize,
        needed: usize,
    },
    InvalidDiscriminant(u8),
    InvalidConstStrId(u8),
    InvalidFuncId(u32),
}

type DecodeResult = Result<Value, DecodeError>;

pub fn decode_value(prog: &Program, code: &[u8], ip: &mut usize) -> DecodeResult {
        type DE = DecodeError;
        use std::convert::TryInto;
        if *ip >= code.len() {
            return Err(DE::NotEnoughBytes{
                got: 0,
                needed: 1,
            });
        }

        let got = code.len() - *ip;

        match code[*ip] {
            0 => { *ip += 1; Ok(Value::Nil) },
            1 => match code.get(*ip + 1) {
                Some(x) => { *ip += 2; Ok(Value::Bool(*x != 0)) },
                None => Err(DE::NotEnoughBytes{ got, needed: 2 }),
            },
            2 => match code.len() - *ip >= 9 {
                true => {
                    let result = f64::from_be_bytes(
                        code[*ip+1..*ip+9].try_into().unwrap()
                    );
                    *ip += 9;
                    Ok(Value::Num(result))
                },
                false => Err(DE::NotEnoughBytes { got, needed: 9 }),
            },
            3 => match code.get(*ip + 1) {
                Some(id) => match prog.get_const_str(*id) {
                    Some(_) => { *ip += 2; Ok(Value::ConstStr(*id)) },
                    None => Err(DE::InvalidConstStrId(*id)),
                },
                None => Err(DE::NotEnoughBytes { got, needed: 2 }),
            },
            4 => match code.len() - *ip >= 5 {
                true => {
                    *ip += 1;
                    let idx = u32::from_be_bytes(
                        code[*ip..*ip+4].try_into().unwrap()
                    ) as usize;
                    *ip += 4;
                    match prog.get_fn_info(idx) {
                        Some(_) => Ok(Value::Object(Object::Func(idx))),
                        None => Err(DE::InvalidFuncId(idx as u32))
                    }
                },
                false => Err(DE::NotEnoughBytes { got, needed: 6 }),
            },
            d => Err(DE::InvalidDiscriminant(d)),
        }
}

pub mod disasm {
    use super::*;
    impl Program {
        fn fmt_bytecode(&self, f: &mut fmt::Formatter<'_>, code: &[u8]) -> fmt::Result {
            use std::convert::TryInto;
            let mut ip = 0;
            while ip < code.len() {
                ip += 1;
                write!(f, "{}\t", ip - 1)?;
                match code[ip - 1] {
                    HLT => writeln!(f, "hlt"),
                    PUSH => writeln!(f, "push {}", decode_value(self, code, &mut ip).unwrap()),
                    POP => writeln!(f, "pop"),
                    POPN => { ip += 1; writeln!(f, "popn {}", code[ip - 1]) },
                    NEGATE => writeln!(f, "negate"),
                    ADD => writeln!(f, "add"),
                    SUB => writeln!(f, "sub"),
                    MUL => writeln!(f, "mul"),
                    DIV => writeln!(f, "div"),
                    NOT => writeln!(f, "not"),
                    EQUAL => writeln!(f, "equal"),
                    NOT_EQUAL => writeln!(f, "not_equal"),
                    LESS => writeln!(f, "less"),
                    LESS_EQUAL => writeln!(f, "less_equal"),
                    GREATER => writeln!(f, "greater"),
                    GREATER_EQUAL => writeln!(f, "greater_equal"),
                    PRINT => writeln!(f, "print"),
                    DEFINE_GLOBAL => { ip += 1; writeln!(f, "define_global <conststr id={}>", code[ip - 1]) },
                    SET_GLOBAL => { ip += 1; writeln!(f, "set_global <conststr id={}>", code[ip - 1]) },
                    GET_GLOBAL => { ip += 1; writeln!(f, "get_global <conststr id={}>", code[ip - 1]) },
                    DUPL => { ip += 1; writeln!(f, "dupl {}", code[ip - 1]) },
                    REPL => { ip += 1; writeln!(f, "repl {}", code[ip - 1]) },
                    x if x ==  JMP || x == JF => {
                        let addr = u32::from_be_bytes(code[ip..ip+4].try_into().unwrap());
                        ip += 4;
                        writeln!(f, "{} {}", if x == JMP { "jmp" } else { "jf" }, addr)
                    },
                    CALL => { 
                        let fdx = u32::from_be_bytes(code[ip..ip+4].try_into().unwrap());
                        ip += 4; 
                        writeln!(f, "call <function id={}>", fdx) 
                    },
                    RET => { ip += 1; writeln!(f, "ret {}", code[ip - 1])}
                    x => panic!("unknown opcode {}", x)
                }?;
            }
            Ok(())
        }
    }

    impl fmt::Display for Program {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            writeln!(f, "------------Constants table------------")?;
            for i in 0..self.num_of_consts() {
                writeln!(f, "{}. \"{}\"", i, self.get_const_str(i as u8).unwrap())?;
            }
            writeln!(f, "---------------Functions---------------")?;
            for i in 0..self.num_of_fns() {
                let info = self.get_fn_info(i).unwrap();
                let name = self.get_const_str(info.name).unwrap();
                let code = self.get_fn_code(i).unwrap();
                writeln!(f, "{}(accepts {} params): ", name, info.anary)?;
                self.fmt_bytecode(f, code)?;
                writeln!(f, "")?;
            }

            Ok(())
        }
    }
}
