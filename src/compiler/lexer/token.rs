extern crate phf;
use phf::phf_map;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SourcePos {
    pub line: u32,
    pub col: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    Nil,
    Bool(bool),
    Num(f64),
    Str(&'a str),
    Ident(&'a str),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    pub value: Value<'a>,
    pub tp: usize,
    pub pos: SourcePos,
}

impl<'a> Token<'a> {
    pub fn new(tp: usize, value: Value<'a>, pos: SourcePos) -> Self {
        Self {
            tp,
            value,
            pos
        }
    }

    pub fn without_value(tp: usize, pos: SourcePos) -> Self {
        Self {
            tp,
            pos,
            value: Value::Nil,
        }
    }
}

pub mod tp {
    use crate::declare_consts;
    declare_consts! { usize,
        NIL = 0,
        BOOL,
        NUM,
        IDENT,
        STR,

        LPAREN, RPAREN,
        LCURL, RCURL,

        PLUS, MINUS,
        MUL, DIV,

        DOT, SEMI, COMMA,

        BANG,
        EQUAL, LESS, GREATER,
        NOT_EQUAL, LESS_EQUAL, GREATER_EQUAL,

        ASSIGN,

        FN, LET, PRINT, IF, ELSE, WHILE, FOR, RETURN,

        EOF,
        NUM_OF_TOKENS,
    }
}

pub static KEYWORDS: phf::Map<&'static str, usize> = phf_map! {
    "fn" => tp::FN,
    "let" => tp::LET,
    "print" => tp::PRINT,
    "if" => tp::IF,
    "else" => tp::ELSE,
    "while" => tp::WHILE,
    "for" => tp::FOR,
    "return" => tp::RETURN,
};

pub static TAGS: phf::Map<char, usize> = phf_map! {
    '(' => tp::LPAREN,
    ')' => tp::RPAREN,
    '+' => tp::PLUS,
    '-' => tp::MINUS,
    '*' => tp::MUL,
    '/' => tp::DIV,
    '!' => tp::BANG,
    '<' => tp::LESS,
    '>' => tp::GREATER,
    '.' => tp::DOT,
    ';' => tp::SEMI,
    ',' => tp::COMMA,
    '=' => tp::ASSIGN,
    '{' => tp::LCURL,
    '}' => tp::RCURL,
};
