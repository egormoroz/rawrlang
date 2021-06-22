pub mod parser;
pub mod lexer;
use crate::vm::{program::{Program, Function}, opcode as op};

use parser::{ast::*, stmt::*};
use lexer::token::{Token, Value, tp};
use std::collections::HashMap;

#[derive(Debug)]
pub enum CompilationError<'a> {
    ParseError(parser::ParseError<'a>),
    DuplicateFnDecl(Token<'a>),
    RetNotInFn(Token<'a>),
    UndeclaredFn(Token<'a>),
}

type CR<'a> = Result<(), CompilationError<'a>>;
type CE<'a> = CompilationError<'a>;

struct Func {
    name: u8,
    anary: u8,
    code: Vec<u8>,
}

#[derive(Debug, Clone, Copy)]
struct Local<'a> {
    depth: u32,
    name: &'a str,
}

pub struct Compiler<'a> {
    const_strs: HashMap<&'a str, u8>,
    locals: Vec<Local<'a>>,
    stack_start: usize,
    depth: u32,

    fns: Vec<Func>,
    current_fn: usize,
}

impl<'a> Compiler<'a> {
    pub fn compile(stmts: &Vec<Stmt<'a>>) -> Result<Program, CompilationError<'a>> {
        let mut instance = Self {
            const_strs: HashMap::new(),
            locals: vec![],
            stack_start: 0,
            depth: 0,
            fns: vec![],
            current_fn: 0,
        }.init_entry();

        for i in stmts {
            i.accept(&mut instance)?;
        }

        // println!("{:?}", instance.code);
        // println!("{:?}", instance.fns_code);

        Ok(instance.finalize())
    }

    fn init_entry(mut self) -> Self {
        let name = self.define_const_str(".entry");
        self.fns.push(Func { name, anary: 0, code: vec![] });
        self
    }

    fn finalize(mut self) -> Program {
        self.fns[0].code.push(op::HLT);
        let (str_atlas, str_data) = self.gen_const_table();

        let code_len: usize = self.fns
            .iter()
            .map(|i| i.code.len())
            .sum();

        let mut code = Vec::with_capacity(code_len);
        let mut fn_table = vec![];
        for i in self.fns {
            fn_table.push(Function {
                name: i.name,
                anary: i.anary,
                ip: code.len(),
                len: i.code.len(),
            });
            code.extend(i.code);
        }

        Program::new(str_atlas, str_data, fn_table, code)
    }

    fn gen_const_table(&self) -> (Vec<(usize, usize)>, String) {
        let mut len = 0;
        let mut a = Vec::with_capacity(self.const_strs.len());
        for (k, v) in &self.const_strs {
            len += k.len();
            a.push((*v, *k));
        }
        a.sort_unstable_by(|lhs, rhs| lhs.0.cmp(&rhs.0));

        let mut data = String::with_capacity(len);
        let mut atlas = Vec::with_capacity(a.len());

        for (_, i) in a {
            atlas.push((data.len(), data.len() + i.len()));
            data.push_str(i);
        }

        (atlas, data)
    }

    fn emit_byte(&mut self, b: u8) {
        self.fns[self.current_fn].code.push(b);
    }

    fn emit_slice(&mut self, slice: &[u8]) {
        self.fns[self.current_fn].code.extend_from_slice(slice);
    }

    fn get_local_ip(&self) -> usize {
        self.fns[self.current_fn].code.len()
    }

    fn replace_bytes(&mut self, offset: usize, src: &[u8]) {
        let dst = &mut self.fns[self.current_fn].code;
        assert!(offset + src.len() <= dst.len());
        dst[offset..offset+src.len()].copy_from_slice(src);
    }

    fn define_const_str(&mut self, s: &'a str) -> u8 {
        let len = self.const_strs.len() as u8;
        *self.const_strs.entry(s)
                        .or_insert(len)
    }

    fn asm_push(&mut self, tok_val: &Value<'a>) {
        self.emit_byte(op::PUSH);
        match tok_val {
            Value::Nil => self.emit_byte(0),
            Value::Bool(b) => {
                self.emit_byte(1);
                self.emit_byte(if *b { 1 } else { 0 });
            }
            Value::Num(n) => {
                self.emit_byte(2);
                self.emit_slice(&n.to_be_bytes());
            }
            Value::Str(s) => {
                self.emit_byte(3);
                let idx = self.define_const_str(s);
                self.emit_byte(idx);
            },
            _ => panic!("cannot push other values")
        }
    }

    //caller must push the initial value on the stack
    fn define_var(&mut self, name: &'a str) -> CR<'a> {
        if self.depth > 0 {
            self.locals.push(Local {
                name,
                depth: self.depth,
            });
        } else {
            self.emit_byte(op::DEFINE_GLOBAL);
            let id = self.define_const_str(name);
            self.emit_byte(id);
        }
        Ok(())
    }

    fn resolve_local(&self, name: &'a str) -> Option<u8> {
        let frame = &self.locals[self.stack_start..];
        for (idx, local) in frame.iter().enumerate().rev() {
            assert!(local.depth <= self.depth, "leaked some locals - stack is corrupted");
            if local.name == name {
                return Some(idx as u8)
            }
        }

        None
    }

    fn enter_block(&mut self) {
        self.depth += 1;
    }

    fn leave_block(&mut self) -> u8 {
        let mut n = 0;
        let frame = &self.locals[self.stack_start..];
        for i in frame {
            assert!(i.depth <= self.depth, "leaked some locals - stack is corrupted");
            if i.depth < self.depth {
                break
            }
            n += 1;
        }
        if n > 0 {
            self.locals.truncate(self.locals.len() - n as usize);
        }
        self.depth -= 1;
        n
    }

    fn unpack_ident(val: &Value<'a>) -> &'a str {
        match val {
            Value::Ident(name) => name,
            v => panic!("invalid AST: unexpected var name value type {:?}", v),
        }
    }

    fn define_func(&mut self, fd: &FnDecl<'a>) -> Result<(), CE<'a>> {
        let name = Self::unpack_ident(&fd.name.value);
        let sdx = self.define_const_str(name);

        if let Some(_) = self.fns.iter().find(|el| el.name == sdx) {
            Err(CE::DuplicateFnDecl(fd.name.clone()))
        } else {
            self.fns.push(Func {
                name: sdx,
                anary: fd.params.len() as u8,
                code: vec![]
            });
           
            Ok(())
        }
    }

    fn func_by_name(&self, token: &Token<'a>) -> Result<usize, CE<'a>> {
        let name = Self::unpack_ident(&token.value);
        if let Some(sdx) = self.const_strs.get(name) {
            if let Some(idx) = self.fns.iter().position(|x| x.name == *sdx) {
                return Ok(idx)
            }
        }
        Err(CE::UndeclaredFn(token.clone()))
    }
}

impl<'a> ASTWalker<'a> for Compiler<'a> {
    type Result = Result<(), CE<'a>>;

    fn visit_binary(&mut self, node: &Binary<'a>) -> Self::Result {
        node.right.accept(self)?;
        node.left.accept(self)?;
        self.emit_byte(match node.op.tp {
            tp::PLUS => op::ADD,
            tp::MINUS => op::SUB,
            tp::MUL => op::MUL,
            tp::DIV => op::DIV,
            tp::EQUAL => op::EQUAL,
            tp::NOT_EQUAL => op::NOT_EQUAL,
            tp::LESS => op::LESS,
            tp::LESS_EQUAL => op::LESS_EQUAL,
            tp::GREATER => op::GREATER,
            tp::GREATER_EQUAL => op::GREATER_EQUAL,
            _ => panic!("invalid AST: expected binary operator, but got {:?}", node.op),
        });
        Ok(())
    }

    fn visit_unary(&mut self, node: &Unary<'a>) -> Self::Result {
        node.expr.accept(self)?;
        match node.op.tp {
            tp::MINUS => self.emit_byte(op::NEGATE),
            tp::PLUS => (),
            tp::BANG => self.emit_byte(op::NOT),
            _ => panic!("invalid AST: expected unary operator, but got {:?}", node.op),
        };
        Ok(())
    }

    fn visit_literal(&mut self, node: &Literal<'a>) -> Self::Result {
        self.asm_push(&node.0.value);
        Ok(())
    }

    fn visit_var(&mut self, var: &Variable<'a>) -> Self::Result {
        let name = Self::unpack_ident(&var.name.value);
        if let Some(offset) = self.resolve_local(name) {
            self.emit_byte(op::DUPL);
            self.emit_byte(offset);
        } else {
            let gdx = self.define_const_str(name);
            self.emit_byte(op::GET_GLOBAL);
            self.emit_byte(gdx);
        }
        Ok(())
    }

    fn visit_assignment(&mut self, asnt: &Assignment<'a>) -> Self::Result {
        asnt.src.accept(self)?;
        let name = Self::unpack_ident(&asnt.dst.value);
        if let Some(offset) = self.resolve_local(name) {
            self.emit_byte(op::REPL);
            self.emit_byte(offset);
        } else {
            let gdx = self.define_const_str(name);
            self.emit_byte(op::SET_GLOBAL);
            self.emit_byte(gdx);
        }
        Ok(())
    }

    //nested declarations are not supported yet
    fn visit_fncall(&mut self, fc: &FnCall<'a>) -> Self::Result {
        for i in fc.args.iter().rev() {
            i.accept(self)?;
        }

        let fdx = self.func_by_name(&fc.name)? as u32;
        self.emit_byte(op::CALL);
        self.emit_slice(&fdx.to_be_bytes());

        Ok(())
    }
}

impl<'a> StmtWalker<'a> for Compiler<'a> {
    type Result = Result<(), CE<'a>>;

    fn visit_expr(&mut self, stmt: &Expr<'a>) -> Self::Result {
        stmt.0.accept(self)?;
        //keeping the stack clean
        self.emit_byte(op::POP);
        Ok(())
    }

    fn visit_print(&mut self, stmt: &Print<'a>) -> Self::Result {
        stmt.0.accept(self)?;
        self.emit_byte(op::PRINT);
        Ok(())
    }

    fn visit_vardecl(&mut self, decl: &VarDecl<'a>) -> Self::Result {
        match &decl.initializer {
            Some(n) => n.accept(self)?,
            None => self.asm_push(&Value::Nil),
        };
        self.define_var(Self::unpack_ident(&decl.name.value))
    }

    fn visit_block(&mut self, block: &Block<'a>) -> Self::Result {
        if !block.stmts.is_empty() {
            self.enter_block();
            for i in &block.stmts {
                i.accept(self)?;
            }
            let n = self.leave_block();

            match block.stmts.last().unwrap() {
                Stmt::RetStmt(_) => (),
                _ => {
                    self.emit_byte(op::POPN);
                    self.emit_byte(n);
                }
            }            
        }
        Ok(())
    }

    fn visit_ifstmt(&mut self, stmt: &IfStmt<'a>) -> Self::Result {
        stmt.condition.accept(self)?;

        self.emit_byte(op::JF);
        self.emit_slice(&[0, 0, 0, 0]);
        let if_branch_addr = self.get_local_ip();

        self.visit_block(&stmt.if_branch)?;
        let else_branch_addr;
        if !stmt.else_branch.stmts.is_empty() {
            self.emit_byte(op::JMP);
            self.emit_slice(&[0, 0, 0, 0]);
            else_branch_addr = self.get_local_ip();

            self.visit_block(&stmt.else_branch)?;
            let end_addr = self.get_local_ip();
            self.replace_bytes(else_branch_addr - 4, &(end_addr as u32).to_be_bytes());
        } else {
            else_branch_addr = self.get_local_ip();
        }

        self.replace_bytes(if_branch_addr - 4, &(else_branch_addr as u32).to_be_bytes());

        Ok(())
    }

    fn visit_whilestmt(&mut self, stmt: &WhileStmt<'a>) -> Self::Result {
        let start_addr = self.get_local_ip();
        stmt.condition.accept(self)?;
        self.emit_byte(op::JF);
        self.emit_slice(&[0, 0, 0, 0]);
        let body_addr = self.get_local_ip();

        self.visit_block(&stmt.body)?;

        self.emit_byte(op::JMP);
        self.emit_slice(
            &(start_addr as u32).to_be_bytes()
        );
        let end_addr = self.get_local_ip() as u32;
        self.replace_bytes(body_addr - 4, &end_addr.to_be_bytes());

        Ok(())
    }

    fn visit_fndecl(&mut self, fd: &FnDecl<'a>) -> Self::Result {
        let parent = self.current_fn;
        let old_frame = self.stack_start;
        self.current_fn = self.fns.len();
        self.stack_start = self.locals.len();
        self.define_func(&fd)?;

        self.enter_block();
        for p in &fd.params {
            self.define_var(Self::unpack_ident(&p.value))?;
        }

        for i in &fd.body.stmts {
            i.accept(self)?;
        }
        let n = self.leave_block();
        assert_eq!(self.locals.len(), self.stack_start, "pseudo stack isn't cleaned after function declaration");


        match fd.body.stmts.last() {
            Some(Stmt::RetStmt(_)) => (),
            Some(_) | None => {
                self.asm_push(&Value::Nil);
                self.emit_byte(op::RET);
                self.emit_byte(n);
            }
        }

        self.current_fn = parent;
        self.stack_start = old_frame;

        let fdx = self.current_fn as u32;
        self.emit_byte(op::PUSH);
        self.emit_byte(4); //Value::Object::Function
        self.emit_slice(&fdx.to_be_bytes());
        self.define_var(Self::unpack_ident(&fd.name.value))?;

        Ok(())
    }

    fn visit_retstmt(&mut self, rs: &RetStmt<'a>) -> Self::Result {
        if let Some(expr) = &rs.expr {
            expr.accept(self)?;
        } else {
            self.asm_push(&Value::Nil);
        }
        self.emit_byte(op::RET);
        self.emit_byte((self.locals.len() - self.stack_start) as u8);
        Ok(())
    }
}

pub fn compile(src: &str) -> Result<Program, CompilationError> {
    let lexer = lexer::Lexer::new(src);
    let mut parser = match parser::Parser::new(lexer) {
        Ok(p) => p,
        Err(e) => return Err(CompilationError::ParseError(e)),
    };
    let stmts = match parser.parse() {
        Ok(v) => v,
        Err(e) => return Err(CompilationError::ParseError(e)),
    };

    Compiler::compile(&stmts)
}
