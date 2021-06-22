pub mod ast;
pub mod stmt;

use ast::*;
use stmt::*;

use super::lexer::{Lexer, LxError, token::{Token, tp}};

#[derive(Debug)]
pub enum ParseError<'a> {
    UnexpectedToken {
        expected: usize,
        got: Token<'a>,
    },
    ExpectedPrefixExpr {
        got: Token<'a>,
    },
    ExpectedInfixExpr {
        got: Token<'a>,
    },
    LxError(LxError),
    InvalidDst(AST<'a>),
}

pub type ParseResult<'a> = Result<Node<'a>, ParseError<'a>>;
pub type ParsedStmt<'a> = Result<Stmt<'a>, ParseError<'a>>;

type ParseFn<'a> = fn(&mut Parser<'a>, Token<'a>) -> ParseResult<'a>;
type InfixFn<'a> = fn(&mut Parser<'a>, Token<'a>, Node<'a>) -> ParseResult<'a>;
type BP = u8;

#[derive(Copy, Clone)]
struct ParseRule<'a> {
    prefix: ParseFn<'a>,
    infix: InfixFn<'a>,
    bp: BP,
}

impl<'a> ParseRule<'a> {
    pub fn default() -> Self {
        Self {
            prefix: Self::default_prefix,
            infix: Self::default_infix,
            bp: 0,
        }
    }

    pub fn new(prefix: ParseFn<'a>, infix: InfixFn<'a>, bp: BP) -> Self {
        Self { prefix, infix, bp }
    }

    pub fn with_prefix(prefix: ParseFn<'a>, bp: BP) -> Self {
        Self {
            prefix,
            infix: Self::default_infix,
            bp,
        }
    }

    pub fn with_infix(infix: InfixFn<'a>, bp: BP) -> Self {
        Self {
            prefix: Self::default_prefix,
            infix,
            bp
        }
    }

    fn default_prefix(_: &mut Parser, got: Token<'a>) -> ParseResult<'a> {
        Err(ParseError::ExpectedPrefixExpr { got })
    }

    fn default_infix(_: &mut Parser, got: Token<'a>, _: Node<'a>) -> ParseResult<'a> {
        Err(ParseError::ExpectedInfixExpr { got })
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token<'a>,
    rules: [ParseRule<'a>; tp::NUM_OF_TOKENS],
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Result<Self, ParseError<'a>>{
        let first = match lexer.next_token() {
            Ok(tkn) => tkn,
            Err(e) => return Err(ParseError::LxError(e)),
        };
        Ok(Self {
            lexer,
            current_token: first,
            rules: [ParseRule::default(); tp::NUM_OF_TOKENS],
        }.init_rules())
    }

    fn init_rules(mut self) -> Self {
        self.rules[tp::NIL] = ParseRule::with_prefix(Parser::literal, 0);
        self.rules[tp::NUM] = ParseRule::with_prefix(Parser::literal, 0);
        self.rules[tp::BOOL] = ParseRule::with_prefix(Parser::literal, 0);
        self.rules[tp::STR] = ParseRule::with_prefix(Parser::literal, 0);
        self.rules[tp::IDENT] = ParseRule::with_prefix(Parser::ident, 0);
        self.rules[tp::LPAREN] = ParseRule::with_prefix(Parser::parens, 0);
        self.rules[tp::BANG] = ParseRule::with_prefix(Parser::unary, 0);

        self.rules[tp::PLUS] = ParseRule::with_infix(Parser::binary, 10);
        self.rules[tp::MUL] = ParseRule::with_infix(Parser::binary, 20);
        self.rules[tp::DIV] = ParseRule::with_infix(Parser::binary, 20);

        self.rules[tp::MINUS] = ParseRule::new(Parser::unary,Parser::binary, 10);

        self.rules[tp::EQUAL] = ParseRule::with_infix(Parser::binary, 5);
        self.rules[tp::LESS] = ParseRule::with_infix(Parser::binary, 5);
        self.rules[tp::GREATER] = ParseRule::with_infix(Parser::binary, 5);
        self.rules[tp::NOT_EQUAL] = ParseRule::with_infix(Parser::binary, 5);
        self.rules[tp::LESS_EQUAL] = ParseRule::with_infix(Parser::binary, 5);
        self.rules[tp::GREATER_EQUAL] = ParseRule::with_infix(Parser::binary, 5);

        self.rules[tp::ASSIGN] = ParseRule::with_infix(Parser::assignment, 1);

        self

    }

    pub fn parse(&mut self) -> Result<Vec<Stmt<'a>>, ParseError<'a>> {
        let mut result = vec![];
        while self.current_token.tp != tp::EOF {
            result.push(self.decl()?);
        }
        Ok(result)
    }

    fn eat(&mut self, tp: usize) -> Result<(), ParseError<'a>> {
        if self.current_token.tp == tp {
            self.next_token()?;
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: tp,
                got: self.current_token.clone(),
            })
        }
    }

    //sets self.current_token to next tokens and returns its old value
    fn next_token(&mut self) -> Result<Token<'a>, ParseError<'a>> {
        Ok(std::mem::replace(&mut self.current_token, match self.lexer.next_token() {
            Ok(tkn) => tkn,
            Err(e) => return Err(ParseError::LxError(e)),
        }))
    }

    fn decl(&mut self) -> ParsedStmt<'a> {
        match self.current_token.tp {
            tp::LET => self.var_decl(),
            tp::FN => self.func_decl(),
            _ => self.stmt(),
        }
    }

    fn var_decl(&mut self) -> ParsedStmt<'a> {
        self.eat(tp::LET)?;
        if self.current_token.tp == tp::IDENT {
            let name = self.next_token()?;
            let initializer = if self.current_token.tp == tp::ASSIGN {
                self.next_token()?;
                Some(self.expr(0)?)
            } else {
                None
            };

            self.eat(tp::SEMI)?;
            Ok(Stmt::VarDecl(VarDecl { name, initializer }))
        } else {
            Err(ParseError::UnexpectedToken {
                expected: tp::IDENT,
                got: self.current_token.clone(),
            })
        }
    }

    fn stmt(&mut self) -> ParsedStmt<'a> {
        match self.current_token.tp {
            tp::LCURL => self.block().and_then(|b| Ok(Stmt::Block(b))),
            tp::IF => self.if_stmt(),
            tp::WHILE => self.while_stmt(),
            tp::PRINT => self.print_stmt(),
            tp::RETURN => self.ret_stmt(),
            _ => self.expr_stmt(),
        }
    }

    fn block(&mut self) -> Result<Block<'a>, ParseError<'a>> {
        self.eat(tp::LCURL)?;
        let mut stmts = vec![];

        while self.current_token.tp != tp::RCURL {
            stmts.push(self.decl()?);
        }

        self.eat(tp::RCURL)?;
        Ok(Block { stmts })
    }
    
    fn ret_stmt(&mut self) -> ParsedStmt<'a> {
        let token = self.current_token.clone();
        self.eat(tp::RETURN)?;
        let expr = match self.current_token.tp {
            tp::SEMI => None,
            _ => Some(self.expr(0)?),
        };
        self.eat(tp::SEMI)?;
        Ok(Stmt::RetStmt(RetStmt { token, expr }))
    }

    fn func_decl(&mut self) -> ParsedStmt<'a> {
        self.eat(tp::FN)?;
        let name = self.next_token()?;
        if name.tp == tp::IDENT {
            let params = self.param_list()?;
            let body = self.block()?;
            Ok(Stmt::FnDecl(FnDecl { name, params, body }))
        } else {
            Err(ParseError::UnexpectedToken {
                expected: tp::IDENT,
                got: name,
            })
        }
    }

    fn param_list(&mut self) -> Result<Vec<Token<'a>>, ParseError<'a>> {
        self.eat(tp::LPAREN)?;
        let mut params = vec![];

        if self.current_token.tp != tp::RPAREN {
            let token = self.next_token()?;
            match token.tp {
                tp::IDENT => params.push(token),
                _ => return Err(ParseError::UnexpectedToken {
                    expected: tp::IDENT,
                    got: token,
                }),
            };
            
            while self.current_token.tp == tp::COMMA {
                self.next_token()?;
                let token = self.next_token()?;
                match token.tp {
                    tp::IDENT => params.push(token),
                    _ => return Err(ParseError::UnexpectedToken {
                        expected: tp::IDENT,
                        got: token,
                    }),
                };

            }
        }

        self.eat(tp::RPAREN)?;
        Ok(params)
    }

    fn arg_list(&mut self) -> Result<Vec<Node<'a>>, ParseError<'a>> {
        self.eat(tp::LPAREN)?;
        let mut args = vec![];

        if self.current_token.tp != tp::RPAREN {
            args.push(self.expr(0)?);
            
            while self.current_token.tp == tp::COMMA {
                self.next_token()?;
                args.push(self.expr(0)?);
            }
        }

        self.eat(tp::RPAREN)?;
        Ok(args)
    }

    fn if_stmt(&mut self) -> ParsedStmt<'a> {
        self.eat(tp::IF)?;
        let condition = self.expr(0)?;
        let if_branch = self.block()?;
        let else_branch = if self.current_token.tp == tp::ELSE {
            self.eat(tp::ELSE)?;
            self.block()?
        } else {
            Block::empty()
        };
        Ok(IfStmt::new(condition, if_branch, else_branch))
    }

    fn while_stmt(&mut self) -> ParsedStmt<'a> {
        self.eat(tp::WHILE)?;
        let condition = self.expr(0)?;
        let body = self.block()?;
        Ok(Stmt::WhileStmt(WhileStmt { condition, body }))
    }

    fn print_stmt(&mut self) -> ParsedStmt<'a> {
        self.eat(tp::PRINT)?;
        let result = Stmt::Print(Print(self.expr(0)?));
        self.eat(tp::SEMI)?;
        Ok(result)
    }

    fn expr_stmt(&mut self) -> ParsedStmt<'a> {
        let result = Stmt::Expr(Expr(self.expr(0)?));
        self.eat(tp::SEMI)?;
        Ok(result)
    }

    fn expr(&mut self, min_bp: BP) -> ParseResult<'a> {
        let mut t = self.next_token()?;
        let mut left = (self.rules[t.tp].prefix)(self, t)?;

        while self.rules[self.current_token.tp].bp > min_bp {
            t = self.next_token()?;
            left = (self.rules[t.tp].infix)(self, t, left)?;
        }

        Ok(left)
    }

    fn literal(&mut self, token: Token<'a>) -> ParseResult<'a> {
        Ok(Literal::new(token))
    }

    fn ident(&mut self, name: Token<'a>) -> ParseResult<'a> {
        match self.current_token.tp {
            tp::LPAREN => self.func_call(name),
            _ => Ok(Variable::new(name))
        }
    }

    fn binary(&mut self, token: Token<'a>, left: Node<'a>) -> ParseResult<'a> {
        let right = self.expr(self.rules[token.tp].bp)?;
        Ok(Binary::new(left, token, right))
    }

    fn unary(&mut self, token: Token<'a>) -> ParseResult<'a> {
        let expr = self.expr(self.rules[token.tp].bp)?;
        Ok(Unary::new(token, expr))
    }

    fn parens(&mut self, _: Token<'a>) -> ParseResult<'a> {
        let result = self.expr(0)?;
        self.eat(tp::RPAREN)?;
        Ok(result)
    }

    fn assignment(&mut self, _: Token<'a>, dst: Node<'a>) -> ParseResult<'a> {
        let src = self.expr(0)?;
        match *dst {
            AST::Variable(v) => Ok(Assignment::new(v.name, src)),
            a => Err(ParseError::InvalidDst(a)),
        }
    }

    fn func_call(&mut self, name: Token<'a>) -> ParseResult<'a> {
        let args = self.arg_list()?;
        Ok(FnCall::new(name, args))
    }
}
