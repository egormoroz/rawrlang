pub mod program;
pub mod opcode;
pub mod value;
mod handlers;
mod mem;
mod decoder;

use mem::{Slots, Object};
use program::Program;
use value::Value;
use handlers::{RtError, RtResult};
use std::collections::HashMap;

pub use decoder::disasm;
use std::fmt;

struct CallFrame<'a> {
    name: u8,
    caller_code: &'a [u8],
    caller_stack_start: usize,
    ret_addr: usize,
}

pub struct VM<'a> {
    program: &'a Program,
    code: &'a [u8],
    stack: Vec<Value>,
    call_stack: Vec<CallFrame<'a>>,
    globals: HashMap<u8, Value>,
    //dynamically allocated objects
    //since we don't have a garbage collector yet THEY ARE NOT DEALLOCATED!!!
    objects: Slots,

    running: bool,
    ip: usize,
    fn_name: u8,
    stack_start: usize,

    handlers: [handlers::HandlerFn<'a>; 0xFF],
}

pub const STARTING_STACK_CAP: usize = 32;
pub const STARTING_CALLSTACK_CAP: usize = 64;
pub const STARTING_SLOTS_CAP: usize = 16;

impl<'a> VM<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            code: &[],
            stack: Vec::with_capacity(STARTING_STACK_CAP),
            call_stack: Vec::with_capacity(STARTING_CALLSTACK_CAP),
            running: false,
            ip: 0,
            fn_name: 0,
            handlers: [Self::unknown_opcode; 0xFF],
            globals: HashMap::new(),
            objects: Slots::with_capacity(STARTING_SLOTS_CAP),
            stack_start: 0,
        }.init_handlers()
         .setup_entry()
    }

    fn setup_entry(mut self) -> Self {
        self.code = self.program.get_fn_code(0).expect("");
        self
    }

    pub fn run(&mut self) -> RtResult {
        self.running = true;
        while self.running {
            let op = self.next_byte()?;
            (self.handlers[op as usize])(self)?;
        }

        match self.stack.is_empty() {
            true => Ok(()),
            false => panic!("stack not empty:\n{:?}", self.stack),
        }
    }

    fn next_byte(&mut self) -> Result<u8, RtError> {
        self.ip += 1;
        match self.ip <= self.code.len() {
            true => Ok(self.code[self.ip - 1]),
            false => Err(RtError::UnexpectedEndOfCode),
        }
    }

    fn next_addr(&mut self) -> Result<usize, RtError> {
        let addr = self.next_dword()? as usize;
        match addr < self.code.len() {
            true => Ok(addr),
            false => Err(RtError::InvalidAddr(addr as u32)),
        }
    }

    fn next_dword(&mut self) -> Result<u32, RtError> {
        use std::convert::TryInto;
        self.ip += 4;
        match self.ip <= self.code.len() {
            true => Ok(u32::from_be_bytes(
                self.code[self.ip-4..self.ip].try_into().unwrap()
            )),
            false => Err(RtError::UnexpectedEndOfCode)
        }
    }

    fn get_const_str(&self, id: u8) -> Result<&str, RtError> {
        match self.program.get_const_str(id) {
            Some(s) => Ok(s),
            None => Err(RtError::InvalidConstStrId(id)),
        } 
    }

    fn repr<W: fmt::Write>(&self, w: &mut W, value: &Value) -> Result<(), RtError> {
        match value {
            Value::Nil => write!(w, "nil").unwrap(),
            Value::Bool(b) => write!(w, "{}", b).unwrap(),
            Value::Num(n) => write!(w, "{}", n).unwrap(),
            Value::ConstStr(id) => write!(w, "{}", self.get_const_str(*id)?.to_owned()).unwrap(),
            Value::Object(object) => write!(w, "{}", object).unwrap(),
            Value::ObjectRef(idx) => match self.objects.get(*idx) {
                Some(object) => write!(w, "{}", object).unwrap(),
                None => return Err(RtError::InvalidObjectSlot(*idx))
            }
        };
        Ok(())
    }
}
