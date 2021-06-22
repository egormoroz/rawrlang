use super::{value::Value, opcode as op, VM, Object, CallFrame};
use super::decoder::{decode_value, DecodeError};

macro_rules! set_handlers {
    ($handlers_array:expr, $name:ident => $handler:ident $(,)?) => {
        $handlers_array[op::$name as usize] = Self::$handler;
    };
    ($handlers_array:expr, $name:ident => $handler:ident, $($names:ident => $handlers:ident),+ $(,)?) => {
        set_handlers!($handlers_array, $name => $handler);
        set_handlers!($handlers_array, $($names => $handlers),+)
    };
}

#[derive(Debug)]
pub enum RtError {
    UnknownOpcode,
    UnexpectedEndOfCode,
    DecodeError(DecodeError),
    EmptyStack,
    InvalidConstStrId(u8),
    InvalidOperand(u8, Value),
    UndefinedGlobal(u8),
    InvalidStackSlot(u8),
    InvalidStackSize(u8),
    InvalidAddr(u32),
    InvalidObjectSlot(usize),
    InvalidFnId(u32),
    UnexpectedRet,
}

type RE = RtError;
pub type RtResult = Result<(), RE>;
pub type HandlerFn<'a> = fn(&mut VM<'a>) -> RtResult;

impl<'a> VM<'a> {
    pub fn init_handlers(mut self) -> Self {
        set_handlers! { self.handlers,
            HLT => hlt,
            PUSH => push, POP => pop, POPN => popn, 
            NEGATE => negate,
            ADD => add, SUB => sub, MUL => mul, DIV => div,
            NOT => not,
            EQUAL => equal, NOT_EQUAL => not_equal,
            LESS => less, LESS_EQUAL => less_equal,
            GREATER => greater, GREATER_EQUAL => greater_equal,
            PRINT => print,
            DEFINE_GLOBAL => define_global, GET_GLOBAL => get_global, SET_GLOBAL => set_global,
            DUPL => dupl, REPL => repl,
            JF => jf, JMP => jmp,
            CALL => call, RET => ret,
        }
        self
    }

    fn try_pop(&mut self) -> Result<Value, RtError> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Err(RE::EmptyStack),
        }        
    }

    fn top_cloned(&self) -> Result<Value, RtError> {
        match self.stack.last() {
            Some(v) => Ok(v.clone()),
            None => Err(RE::EmptyStack),
        }
    }

    fn pop_num(&mut self, op: u8) -> Result<f64, RtError> {
        match self.stack.pop() {
            Some(Value::Num(x)) => Ok(x),
            Some(v) => Err(RE::InvalidOperand(op, v)),
            None => Err(RE::EmptyStack),
        }
    }

    pub fn unknown_opcode(&mut self) -> RtResult {
        Err(RE::UnknownOpcode)
    }

    fn hlt(&mut self) -> RtResult {
        self.running = false;
        Ok(())
    }

    fn push(&mut self) -> RtResult {
        match decode_value(&self.program, self.code, &mut self.ip) {
            Ok(v) => self.stack.push(v),
            Err(e) => return Err(RE::DecodeError(e)),
        };
        Ok(())
    }

    fn pop(&mut self) -> RtResult {
        self.try_pop().map(|_| ())
    }

    fn popn(&mut self) -> RtResult {
        let n = self.next_byte()? as usize;
        if self.stack.len() >= n {
            self.stack.truncate(self.stack.len() - n);
            Ok(())
        } else {
            Err(RE::InvalidStackSize(n as u8))
        }
    }

    fn print(&mut self) -> RtResult {
        let val = self.try_pop()?;
        let mut buf = String::new();
        self.repr(&mut buf, &val)?;
        println!("{}", buf);
        Ok(())
    }

    fn negate(&mut self) -> RtResult {
        let x = self.pop_num(op::NEGATE)?;
        self.stack.push(Value::Num(-x));
        Ok(())
    }

    fn add(&mut self) -> RtResult {
        let lhs = self.try_pop()?;
        match lhs {
            Value::Num(lhs) => {
                let rhs = self.pop_num(op::ADD)?;
                self.stack.push(Value::Num(lhs + rhs));
                Ok(())
            },
            Value::ConstStr(id) => {
                let rhs = self.try_pop()?;
                let lhs = self.get_const_str(id)?;
                let mut s = lhs.to_owned();
                self.repr(&mut s, &rhs)?;
                self.stack.push(Value::Object(Object::Str(s)));
                Ok(())
            },
            Value::ObjectRef(slot) => match self.objects.get(slot) {
                Some(Object::Str(s)) => {
                    let mut s = s.clone();
                    let val = self.try_pop()?;
                    self.repr(&mut s, &val)?;
                    self.stack.push(Value::Object(Object::Str(s)));
                    Ok(())
                },
                Some(_) => Err(RE::InvalidOperand(op::ADD, lhs)),
                None => Err(RE::InvalidObjectSlot(slot)),                                
            },
            Value::Object(Object::Str(mut s)) => {
                let val = self.try_pop()?;
                self.repr(&mut s, &val)?;
                self.stack.push(Value::Object(Object::Str(s)));
                Ok(())
            },
            v => Err(RE::InvalidOperand(op::ADD, v))
        }
    }

    fn sub(&mut self) -> RtResult {
        let (lhs, rhs) = (self.pop_num(op::SUB)?, self.pop_num(op::SUB)?);
        self.stack.push(Value::Num(lhs - rhs));
        Ok(())
    }

    fn div(&mut self) -> RtResult {
        let (lhs, rhs) = (self.pop_num(op::DIV)?, self.pop_num(op::DIV)?);
        self.stack.push(Value::Num(lhs / rhs));
        Ok(())
    }

    fn mul(&mut self) -> RtResult {
        let (lhs, rhs) = (self.pop_num(op::MUL)?, self.pop_num(op::MUL)?);
        self.stack.push(Value::Num(lhs * rhs));
        Ok(())
    }

    fn not(&mut self) -> RtResult {
        match self.stack.pop() {
            Some(Value::Bool(b)) => self.stack.push(Value::Bool(!b)),
            Some(v) => return Err(RtError::InvalidOperand(op::NOT, v)),
            None => return Err(RtError::EmptyStack),
        };
        Ok(())
    }

    fn equal(&mut self) -> RtResult {
        let (lhs, rhs) = (self.try_pop()?, self.try_pop()?);
        self.stack.push(Value::Bool(lhs == rhs));
        Ok(())
    }

    fn not_equal(&mut self) -> RtResult {
        let (lhs, rhs) = (self.try_pop()?, self.try_pop()?);
        self.stack.push(Value::Bool(lhs != rhs));
        Ok(())
    }

    fn less(&mut self) -> RtResult {
        let (lhs, rhs) = (self.pop_num(op::LESS)?, self.pop_num(op::LESS)?);
        self.stack.push(Value::Bool(lhs < rhs));
        Ok(())
    }

    fn less_equal(&mut self) -> RtResult {
        let (lhs, rhs) = (self.pop_num(op::LESS_EQUAL)?, self.pop_num(op::LESS_EQUAL)?);
        self.stack.push(Value::Bool(lhs <= rhs));
        Ok(())
    }

    fn greater(&mut self) -> RtResult {
        let (lhs, rhs) = (self.pop_num(op::GREATER)?, self.pop_num(op::GREATER)?);
        self.stack.push(Value::Bool(lhs > rhs));
        Ok(())
    }

    fn greater_equal(&mut self) -> RtResult {
        let (lhs, rhs) = (self.pop_num(op::GREATER_EQUAL)?, self.pop_num(op::GREATER_EQUAL)?);
        self.stack.push(Value::Bool(lhs >= rhs));
        Ok(())
    }

    fn define_global(&mut self) -> RtResult {
        let sidx = self.next_byte()?;
        match self.program.get_const_str(sidx) {
            Some(_) => (),
            None => return Err(RE::InvalidConstStrId(sidx)),
        };

        let value = self.try_pop()?;
        self.globals.insert(sidx, value);

        Ok(())
    }

    fn get_global(&mut self) -> RtResult {
        let sidx = self.next_byte()?;
        match self.globals.get(&sidx) {
            Some(v) => self.stack.push(v.clone()),
            None => return Err(RE::UndefinedGlobal(sidx)),
        };
        Ok(())
    }

    fn set_global(&mut self) -> RtResult {
        use std::collections::hash_map::Entry;
        let sidx = self.next_byte()?;
        let value = self.top_cloned()?;
        match self.globals.entry(sidx) {
            e @ Entry::Occupied(_) => { e.and_modify(|v| *v = value); Ok(()) },
            _ => Err(RE::UndefinedGlobal(sidx)),
        }
    }

    //duplicate stack value(usually used for locals)
    fn dupl(&mut self) -> RtResult {
        let idx = self.next_byte()? as usize;
        if self.stack_start + idx < self.stack.len() {
            let val = self.stack[self.stack_start + idx].clone();
            self.stack.push(val);
            Ok(())
        } else {
            Err(RE::InvalidStackSlot(idx as u8))
        }
    }

    //replace stack value (usually used for locals)
    fn repl(&mut self) -> RtResult {
        let idx = self.next_byte()? as usize;
        let new_val = self.top_cloned()?;
        if self.stack_start + idx < self.stack.len() {
            self.stack[self.stack_start + idx] = new_val;
            Ok(())
        } else {
            Err(RE::InvalidStackSlot(idx as u8))
        }
    }

    fn jmp(&mut self) -> RtResult {
        let addr = self.next_addr()?;
        self.ip = addr as usize;
        Ok(())
    }

    //jump if false
    fn jf(&mut self) -> RtResult {
        let addr = self.next_addr()?;
        match self.stack.pop() {
            Some(Value::Bool(false)) => { self.ip = addr; Ok(()) }
            Some(Value::Bool(true)) => Ok(()),
            Some(v) => Err(RE::InvalidOperand(op::JF, v)),
            None => Err(RE::EmptyStack),
        }
    }

    fn call(&mut self) -> RtResult {
        let fdx = self.next_dword()? as usize;
        
        if let Some(info) = self.program.get_fn_info(fdx) {
            self.call_stack.push(CallFrame {
                caller_code: self.code,
                name: self.fn_name,
                ret_addr: self.ip, 
                caller_stack_start: self.stack_start,
            });
            self.code = self.program.get_fn_code(fdx).unwrap();
            self.ip = 0;
            self.fn_name = info.name;
            self.stack_start = self.stack.len() - info.anary as usize;

            Ok(())
        } else {
            Err(RE::InvalidFnId(fdx as u32))
        }
    }

    fn ret(&mut self) -> RtResult {
        let n = self.next_byte()? as usize;
        let result = self.try_pop()?;
        match n <= self.stack.len() {
            true => self.stack.truncate(self.stack.len() - n),
            false => return Err(RE::InvalidStackSize(n as u8))
        };
        self.stack.push(result);

        if let Some(frame) = self.call_stack.pop() {
            self.code = frame.caller_code;
            self.fn_name = frame.name;
            self.ip = frame.ret_addr;
            self.stack_start = frame.caller_stack_start;
            Ok(())
        } else {
            Err(RE::UnexpectedRet)
        }
    }
}
