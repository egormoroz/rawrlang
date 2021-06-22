use super::Object;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Num(f64),
    ConstStr(u8),
    ObjectRef(usize),
    Object(Object),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Num(n) => write!(f, "{}", n),
            Value::ConstStr(s) => write!(f, "<conststr idx={}>", s),
            Value::ObjectRef(n) => write!(f, "<object idx={}>", n),
            Value::Object(o) => write!(f, "{}", o),
        }
    }
}

