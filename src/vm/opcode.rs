use crate::declare_consts;

declare_consts! { u8,
    HLT = 0,
    PUSH, POP, POPN,

    NEGATE,
    ADD, SUB, MUL, DIV,

    NOT,
    EQUAL, NOT_EQUAL,
    LESS, LESS_EQUAL,
    GREATER, GREATER_EQUAL,

    PRINT,
    DEFINE_GLOBAL, GET_GLOBAL, SET_GLOBAL,
    DUPL, REPL,

    JMP, JF,

    CALL, RET,

    NUM_OF_OPCODES,
}