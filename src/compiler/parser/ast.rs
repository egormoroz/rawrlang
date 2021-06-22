use super::Token;

pub type Node<'a> = Box<AST<'a>>;

#[derive(Debug)]
pub struct Literal<'a>(pub Token<'a>);

impl<'a> Literal<'a> {
    pub fn new(token: Token<'a>) -> Node<'a> {
        Box::new(AST::Literal(Self(token)))
    }
}

#[derive(Debug)]
pub struct Binary<'a> {
    pub left: Node<'a>,
    pub op: Token<'a>,
    pub right: Node<'a>,
}

impl<'a> Binary<'a> {
    pub fn new(left: Node<'a>, op: Token<'a>, right: Node<'a>) -> Node<'a> {
        Box::new(AST::Binary(Self {
            left,
            op,
            right
        }))
    }
}

#[derive(Debug)]
pub struct Unary<'a> {
    pub op: Token<'a>,
    pub expr: Node<'a>,
}

impl<'a> Unary<'a> {
    pub fn new(op: Token<'a>, expr: Node<'a>) -> Node<'a> {
        Box::new(AST::Unary(Self {
            op,
            expr
        }))
    }
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub name: Token<'a>,
}

impl<'a> Variable<'a> {
    pub fn new(name: Token<'a>) -> Node<'a> {
        Box::new(AST::Variable(Self {
            name,
        }))
    }
}

#[derive(Debug)]
pub struct Assignment<'a> {
    pub dst: Token<'a>,
    pub src: Node<'a>,
}

impl<'a> Assignment<'a> {
    pub fn new(dst: Token<'a>, src: Node<'a>) -> Node<'a> {
        Box::new(AST::Assignment(Self {
            dst,
            src,
        }))
    }
}

#[derive(Debug)]
pub struct FnCall<'a> {
    pub name: Token<'a>,
    pub args: Vec<Node<'a>>,
}

impl<'a> FnCall<'a> {
    pub fn new(name: Token<'a>, args: Vec<Node<'a>>) -> Node<'a> {
        Box::new(AST::FnCall(Self {
            name,
            args
        }))
    }
}

#[derive(Debug)]
pub enum AST<'a> {
    Literal(Literal<'a>),
    Binary(Binary<'a>),
    Unary(Unary<'a>),
    Variable(Variable<'a>),
    Assignment(Assignment<'a>),
    FnCall(FnCall<'a>),
}

impl<'a> AST<'a> {
    pub fn accept<W: ASTWalker<'a>>(&self, w: &mut W) -> W::Result {
        match self {
            AST::Binary(n) => w.visit_binary(n),
            AST::Unary(n) => w.visit_unary(n),
            AST::Literal(n) => w.visit_literal(n),
            AST::Variable(v) => w.visit_var(v),
            AST::Assignment(anmt) => w.visit_assignment(anmt),
            AST::FnCall(fc) => w.visit_fncall(fc),
        }        
    }
}

pub trait ASTWalker<'a> {
    type Result;

    fn visit_binary(&mut self, node: &Binary<'a>) -> Self::Result;
    fn visit_unary(&mut self, node: &Unary<'a>) -> Self::Result;
    fn visit_literal(&mut self, node: &Literal<'a>) -> Self::Result;
    fn visit_var(&mut self, var: &Variable<'a>) -> Self::Result;
    fn visit_assignment(&mut self, anmt: &Assignment<'a>) -> Self::Result;
    fn visit_fncall(&mut self, fc: &FnCall<'a>) -> Self::Result;
}
