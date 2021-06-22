pub mod token;
use token::*;
use std::{iter::Peekable, str::CharIndices};

#[derive(Debug)]
pub enum LxError {
    UnexpectedEOF(SourcePos),
    UnknownLexeme(SourcePos),
}

pub struct Lexer<'a> {
    src: &'a str,
    it: Peekable<CharIndices<'a>>,
    col: u32,
    line: u32,
}

pub type LxResult<'a> = Result<Token<'a>, LxError>;

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            it: src.char_indices().peekable(),
            col: 0,
            line: 1,
        }
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            col: self.col,
            line: self.line,
        }
    }

    fn skip_comment(&mut self) {
        //no need to keep track of column here
        while let Some((_, ch)) = self.it.next() {
            if ch == '\n' {
                self.col = 0;
                self.line += 1;
                break
            }
        }
    }

    fn consume_identifier(&mut self, p: usize) -> LxResult<'a> {
        let mut n = 1;
        let col = self.col;
        while let Some((_, ch)) = self.it.peek() {
            match *ch {
                ch if ch.is_alphanumeric() || ch == '_' => n += 1,
                _ => break,
            };
            self.col += 1;
            self.it.next().unwrap();
        }

        let pos = SourcePos {
            col,
            line: self.line,
        };

        Ok(match &self.src[p..p+n] {
            "nil" => Token::new(tp::NIL, Value::Nil, pos),
            "true" => Token::new(tp::BOOL, Value::Bool(true), pos),
            "false" => Token::new(tp::BOOL, Value::Bool(false), pos),
            name => match KEYWORDS.get(name) {
                Some(tp) => Token::without_value(*tp, pos),
                None => Token::new(tp::IDENT, Value::Ident(name), pos),
            },
        })
    }

    fn consume_number(&mut self, p: usize) -> LxResult<'a> {
        let col = self.col;
        let mut n = 1;
        let mut encountered_dot= false;
        while let Some((_, ch)) = self.it.peek() {
            match ch {
                ch if ch.is_digit(10) => (),
                '.' if !encountered_dot => {
                    encountered_dot = true;
                },
                _ => break,
            };
            n += 1;
            self.col += 1;
            self.it.next().unwrap();
        }

        Ok(Token {
            tp: tp::NUM,
            value: Value::Num(self.src[p..p+n].parse().unwrap()),
            pos: SourcePos {
                col,
                line: self.line
            },
        })
    }

    fn consume_string(&mut self, p: usize) -> LxResult<'a> {
        let col = self.col;
        self.col += 1;
        let line = self.line;
        let p = p + 1;
        let mut n = 0;
        let mut closed = false;
        while let Some((_, ch)) = self.it.next() {
            self.col += 1;
            match ch {
                '"' => { closed = true; break },
                '\n' => {
                    self.line += 1;
                    self.col = 0;
                },
                _ => (),
            };
            n += 1;
        }

        if !closed {
            return Err(LxError::UnexpectedEOF(self.current_pos()))
        }

        Ok(Token {
            tp: tp::STR,
            value: Value::Str(&self.src[p..p+n]),
            pos: SourcePos { line, col }
        })
    }

    fn check_double_tags(&mut self, first: char) -> Option<Token<'a>> {
        let pos = self.current_pos();
        if let Some((_, second)) = self.it.peek() {
            let advance = |l: &mut Lexer| { l.it.next(); l.col += 1; };
            match (first, second) {
                ('/', '/') => { self.skip_comment(); None },
                ('=', '=') => { advance(self); Some(Token::without_value(tp::EQUAL, pos)) },
                ('!', '=') => { advance(self); Some(Token::without_value(tp::NOT_EQUAL, pos)) },
                ('<', '=') => { advance(self); Some(Token::without_value(tp::LESS_EQUAL, pos)) },
                ('>', '=') => { advance(self); Some(Token::without_value(tp::GREATER_EQUAL, pos)) },
                _ => None
            }
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> LxResult<'a> {
        while let Some((p, ch)) = self.it.next() {
            self.col += 1;
            match ch {
                ch if ch.is_whitespace() => match ch {
                    '\n' => {
                        self.line += 1;
                        self.col = 0;
                    },
                    _ => continue,
                },
                ch if ch.is_alphabetic() || ch == '_' => return self.consume_identifier(p),
                ch if ch.is_digit(10) => return self.consume_number(p),
                '"' => return self.consume_string(p),
                ch => match self.check_double_tags(ch) {
                    Some(t) => return Ok(t),
                    None => match TAGS.get(&ch) {
                        Some(tp) => return Ok(Token::without_value(
                            *tp, 
                            self.current_pos()
                        )),
                        None => return Err(LxError::UnknownLexeme(self.current_pos()))
                    }
                },
            }
        }

        Ok(Token::without_value(
            tp::EOF, 
            SourcePos {
                line: self.line,
                col: self.col,
            }
        ))
    }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, LxError> {
    let mut tokens = vec![];
    let mut lexer = Lexer::new(src);

    loop {
        tokens.push(lexer.next_token()?);
        if tokens.last().unwrap().tp == tp::EOF {
            break Ok(tokens);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn verify_tps(src: &str, tkns: &[usize]) {
        let mut lexer = Lexer::new(src);
        for expected in tkns {
            let got = lexer.next_token().unwrap().tp;
            assert_eq!(got, *expected);
        }

        assert_eq!(lexer.next_token().unwrap().tp, tp::EOF);
    }

    #[test]
    fn empty() {
        verify_tps("", &[]);
    }

    #[test]
    fn num() {
        let v = tokenize("69 22.8").unwrap();
        assert_eq!((v[0].tp, v[1].tp), (tp::NUM, tp::NUM));

        match (&v[0].value, &v[1].value) {
            (Value::Num(x), Value::Num(y)) => {
                assert!((x - 69.).abs() <= f64::EPSILON);
                assert!((y - 22.8).abs() <= f64::EPSILON);
            },
            (x, y) => panic!("mismatched: {:?} and {:?}", x, y),
        }
    }

    #[test]
    fn ops_and_parens() {
        use tp::*;
        verify_tps("+-*/()", 
            &[PLUS, MINUS, MUL, DIV, LPAREN, RPAREN]
        )
    }

    #[test]
    fn string() {
        assert_eq!(
            tokenize("\"hi\"").unwrap(),
            vec![
                Token::new(
                    tp::STR, 
                    Value::Str("hi"), 
                    SourcePos { line: 1, col: 1 }
                ),
                Token::without_value(
                    tp::EOF,
                    SourcePos { line: 1, col: 5 }
                )
            ]
        );
    }

    #[test]
    fn idents() {
        verify_tps("let fn", &[tp::LET, tp::FN]);
    }
}
