use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Str(String),
    Func(usize),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Str(s) => write!(f, "{}", s),
            Object::Func(func) => write!(f, "<function id={}>", func),
        }
    }
}