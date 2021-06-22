use super::ast::Node;
use super::Token;

#[derive(Debug)]
pub struct Expr<'a>(pub Node<'a>);
#[derive(Debug)]
pub struct Print<'a>(pub Node<'a>);

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub name: Token<'a>,
    pub initializer: Option<Node<'a>>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

impl<'a> Block<'a> {
    pub fn empty() -> Self {
        Self {
            stmts: vec![]
        }
    }
}

#[derive(Debug)]
pub struct IfStmt<'a> {
    pub condition: Node<'a>,
    pub if_branch: Block<'a>,
    pub else_branch: Block<'a>,
}

impl<'a> IfStmt<'a> {
    pub fn new(condition: Node<'a>,
               if_branch: Block<'a>,
               else_branch: Block<'a>) -> Stmt<'a>
    {
        Stmt::IfStatement(Self {
            condition,
            if_branch,
            else_branch,
        })
    }
}

#[derive(Debug)]
pub struct WhileStmt<'a> {
    pub condition: Node<'a>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct FnDecl<'a> {
    pub name: Token<'a>,
    pub params: Vec<Token<'a>>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct RetStmt<'a> {
    pub token: Token<'a>,
    pub expr: Option<Node<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Print(Print<'a>),
    VarDecl(VarDecl<'a>),
    Block(Block<'a>),
    IfStatement(IfStmt<'a>),
    WhileStmt(WhileStmt<'a>),
    FnDecl(FnDecl<'a>),
    RetStmt(RetStmt<'a>),
}

impl<'a> Stmt<'a> {
    pub fn accept<SW: StmtWalker<'a>>(&self, w: &mut SW) -> SW::Result {
        match self {
            Stmt::Expr(e) => w.visit_expr(e),
            Stmt::Print(e) => w.visit_print(e),
            Stmt::VarDecl(d) => w.visit_vardecl(d),
            Stmt::Block(b) => w.visit_block(b),
            Stmt::IfStatement(is) => w.visit_ifstmt(is),
            Stmt::WhileStmt(ws) => w.visit_whilestmt(ws),
            Stmt::FnDecl(fd) => w.visit_fndecl(fd),
            Stmt::RetStmt(rs) => w.visit_retstmt(rs),
        }
    }
}

pub trait StmtWalker<'a> {
    type Result;

    fn visit_expr(&mut self, stmt: &Expr<'a>) -> Self::Result;
    fn visit_print(&mut self, stmt: &Print<'a>) -> Self::Result;
    fn visit_vardecl(&mut self, decl: &VarDecl<'a>) -> Self::Result;
    fn visit_block(&mut self, block: &Block<'a>) -> Self::Result;
    fn visit_ifstmt(&mut self, stmt: &IfStmt<'a>) -> Self::Result;
    fn visit_whilestmt(&mut self, stmt: &WhileStmt<'a>) -> Self::Result;
    fn visit_fndecl(&mut self, fd: &FnDecl<'a>) -> Self::Result;
    fn visit_retstmt(&mut self, rs: &RetStmt<'a>) -> Self::Result;
}

