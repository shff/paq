#![allow(clippy::cognitive_complexity)]

use std::collections::HashSet;

pub fn get_deps(source: &str) -> HashSet<String> {
    let mut lex = Lexer::new(source);
    let mut deps = HashSet::new();
    loop {
        match lex.advance() {
            Ok(Tt::Id("require")) => {
                if lex.advance() == Ok(Tt::Operator("(")) {
                    match lex.advance() {
                        Ok(Tt::StrLitSgl(s)) | Ok(Tt::StrLitDbl(s)) => {
                            if lex.advance() == Ok(Tt::Operator(")")) {
                                deps.insert(String::from(&s[1..s.len() - 1]));
                            }
                        }
                        _ => {}
                    }
                }
            }
            Ok(Tt::Eof) | Err(_) => return deps,
            _ => {}
        }
    }
}

// Following code taken from esparse crate

const OPS: &[&str] = &[
    "{", "(", ")", "[", "]", ";", ",", "~", "?", ":", "}", "<<=", "<<", "<=", "<", ">>>=", ">>>",
    ">>=", ">>", ">=", ">", "=>", "===", "==", "=", "!==", "!=", "!", "++", "+=", "+", "--", "-=",
    "-", "**=", "**", "*=", "*", "%=", "%", "&&", "&=", "&", "||", "|=", "|", "^=", "^",
];

#[derive(Debug)]
pub struct Lexer<'s> {
    stream: Stream<'s>,
    here: Result<Tt<'s>, Error>,
}

#[derive(Debug)]
pub struct Stream<'s> {
    chars: std::str::Chars<'s>,
    curr: &'s str,
    ncurr: &'s str,
    here: Option<char>,
    next: Option<char>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
    ExpectedExponent,
    UnterminatedTemplateLiteral,
    UnterminatedStringLiteral,
    UnterminatedRegExpLiteral,
    UnterminatedMultilineComment,
    UnmatchedRbrace,
    Unexpected(char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tt<'s> {
    Id(&'s str),
    StrLitSgl(&'s str),
    StrLitDbl(&'s str),
    RegExpLit(&'s str, &'s str),
    NumLitBin(&'s str),
    NumLitOct(&'s str),
    NumLitDec(&'s str),
    NumLitHex(&'s str),
    TemplateNoSub(&'s str),
    TemplateStart(&'s str),
    TemplateMiddle(&'s str),
    TemplateEnd(&'s str),
    Keyword(&'s str),
    Operator(&'s str),
    Eof,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        let chars = input.chars();
        let mut stream = Stream {
            curr: chars.as_str(),
            ncurr: chars.as_str(),
            chars,
            here: None,
            next: None,
        };
        stream.advance();
        stream.advance();
        let mut lexer = Lexer {
            stream,
            here: Ok(Tt::Eof),
        };
        lexer.advance().unwrap();
        lexer
    }

    pub fn advance(&mut self) -> Result<Tt<'s>, Error> {
        let tok = self.read_tok();
        std::mem::replace(&mut self.here, tok)
    }

    fn read_tok(&mut self) -> Result<Tt<'s>, Error> {
        while let Some(c) = self.stream.here {
            match c {
                _ if c.is_whitespace() => {
                    self.stream.advance();
                }
                '/' => match self.stream.next {
                    Some('*') => {
                        self.stream.advance();
                        self.stream.advance();
                        'outer: loop {
                            match self.stream.here {
                                Some('*') => loop {
                                    self.stream.advance();
                                    match self.stream.here {
                                        Some('/') => {
                                            self.stream.advance();
                                            break 'outer;
                                        }
                                        Some('*') => {}
                                        Some(_) => {
                                            self.stream.advance();
                                            break;
                                        }
                                        None => return Err(Error::UnterminatedMultilineComment),
                                    }
                                },
                                Some(_) => {
                                    self.stream.advance();
                                }
                                None => return Err(Error::UnterminatedMultilineComment),
                            }
                        }
                    }
                    Some('/') => {
                        self.stream.advance();
                        self.stream.advance();
                        self.stream.skip_while(char::is_whitespace);
                    }
                    _ => break,
                },
                _ => break,
            }
        }

        let start = self.stream.curr;
        let here = match self.stream.advance() {
            Some(c) => c,
            None => return Ok(Tt::Eof),
        };

        for op in OPS {
            if start.starts_with(op) {
                return Ok(Tt::Operator(op));
            }
        }

        match here {
            '`' => {
                let result;
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            if let Some('\r') = self.stream.advance() {
                                self.stream.eat('\n');
                            }
                        }
                        Some('`') => {
                            result = Ok(Tt::TemplateNoSub(self.stream.str_from(start)));
                            break;
                        }
                        Some(_) => {}
                        None => return Err(Error::UnterminatedTemplateLiteral),
                    }
                }
                result
            }
            '"' => {
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            if let Some('\r') = self.stream.advance() {
                                self.stream.eat('\n');
                            }
                        }
                        Some('"') => break,
                        Some('\n') | Some('\r') | Some('\u{2028}') | Some('\u{2029}') | None => {
                            return Err(Error::UnterminatedStringLiteral)
                        }
                        Some(_) => {}
                    }
                }
                Ok(Tt::StrLitDbl(self.stream.str_from(start)))
            }
            '\'' => {
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            if let Some('\r') = self.stream.advance() {
                                self.stream.eat('\n');
                            }
                        }
                        Some('\'') => break,
                        Some('\n') | Some('\r') | Some('\u{2028}') | Some('\u{2029}') | None => {
                            return Err(Error::UnterminatedStringLiteral)
                        }
                        Some(_) => {}
                    }
                }
                Ok(Tt::StrLitSgl(self.stream.str_from(start)))
            }
            '/' => match self.here {
                Ok(Tt::Operator(")"))
                | Ok(Tt::Operator("}"))
                | Ok(Tt::TemplateEnd(_))
                | Ok(Tt::TemplateNoSub(_))
                | Ok(Tt::StrLitSgl(_))
                | Ok(Tt::StrLitDbl(_))
                | Ok(Tt::RegExpLit(_, _))
                | Ok(Tt::NumLitBin(_))
                | Ok(Tt::NumLitOct(_))
                | Ok(Tt::NumLitDec(_))
                | Ok(Tt::NumLitHex(_))
                | Ok(Tt::Id(_))
                | Ok(Tt::Keyword("this"))
                | Ok(Tt::Keyword("super")) => match self.stream.here {
                    Some('=') => {
                        self.stream.advance();
                        Ok(Tt::Operator("/="))
                    }
                    _ => Ok(Tt::Operator("/")),
                },
                _ => {
                    loop {
                        match self.stream.advance() {
                            Some('\\') => {
                                if let Some('\r') = self.stream.advance() {
                                    self.stream.eat('\n');
                                }
                            }
                            Some('/') => break,
                            Some('[') => loop {
                                match self.stream.advance() {
                                    Some('\\') => {
                                        if let Some('\r') = self.stream.advance() {
                                            self.stream.eat('\n');
                                        }
                                    }
                                    Some(']') => break,
                                    Some('\n') | Some('\r') | Some('\u{2028}')
                                    | Some('\u{2029}') | None => {
                                        return Err(Error::UnterminatedRegExpLiteral)
                                    }
                                    Some(_) => {}
                                }
                            },
                            Some('\n') | Some('\r') | Some('\u{2028}') | Some('\u{2029}')
                            | None => return Err(Error::UnterminatedRegExpLiteral),
                            Some(_) => {}
                        }
                    }
                    let flags_start = self.stream.curr;
                    self.stream.skip_while(|c| match c {
                        'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_' | '\u{200C}' | '\u{200D}' => {
                            true
                        }
                        _ => c.is_alphanumeric(),
                    });
                    let source = self.stream.str_from(start);
                    let flags = self.stream.str_from(flags_start);
                    Ok(Tt::RegExpLit(source, flags))
                }
            },
            '.' => match self.stream.here {
                Some('.') => match self.stream.next {
                    Some('.') => {
                        self.stream.advance();
                        self.stream.advance();
                        Ok(Tt::Operator("..."))
                    }
                    _ => Ok(Tt::Operator(".")),
                },
                Some('0'..='9') => {
                    self.stream.advance();
                    self.stream.skip_while(|c| c.is_digit(10));
                    match self.stream.here {
                        Some('e') | Some('E') => {
                            self.stream.advance();
                            match self.stream.here {
                                Some('-') | Some('+') | Some('0'..='9') => {
                                    self.stream.advance();
                                    self.stream.skip_while(|c| c.is_digit(10));
                                }
                                _ => return Err(Error::ExpectedExponent),
                            }
                        }
                        _ => {}
                    };
                    Ok(Tt::NumLitDec(self.stream.str_from(start)))
                }
                Some(_) | None => Ok(Tt::Operator(".")),
            },
            '0' => match self.stream.here {
                Some('b') | Some('B') => {
                    self.stream.advance();
                    self.stream.skip_while(|c| c.is_digit(2));
                    Ok(Tt::NumLitBin(self.stream.str_from(start)))
                }
                Some('o') | Some('O') => {
                    self.stream.advance();
                    self.stream.skip_while(|c| c.is_digit(8));
                    Ok(Tt::NumLitOct(self.stream.str_from(start)))
                }
                Some('x') | Some('X') => {
                    self.stream.advance();
                    self.stream.skip_while(|c| c.is_digit(16));
                    Ok(Tt::NumLitHex(self.stream.str_from(start)))
                }
                Some('.') => {
                    self.stream.advance();
                    self.stream.skip_while(|c| c.is_digit(10));
                    match self.stream.here {
                        Some('e') | Some('E') => {
                            self.stream.advance();
                            match self.stream.here {
                                Some('-') | Some('+') | Some('0'..='9') => {
                                    self.stream.advance();
                                    self.stream.skip_while(|c| c.is_digit(10));
                                    Ok(Tt::NumLitDec(self.stream.str_from(start)))
                                }
                                _ => Err(Error::ExpectedExponent),
                            }
                        }
                        _ => Ok(Tt::NumLitDec(self.stream.str_from(start))),
                    }
                }
                Some('e') | Some('E') => {
                    self.stream.advance();
                    match self.stream.here {
                        Some('-') | Some('+') | Some('0'..='9') => {
                            self.stream.advance();
                            self.stream.skip_while(|c| c.is_digit(10));
                            Ok(Tt::NumLitDec(self.stream.str_from(start)))
                        }
                        _ => Err(Error::ExpectedExponent),
                    }
                }
                _ => Ok(Tt::NumLitDec(self.stream.str_from(start))),
            },
            '1'..='9' => {
                self.stream.skip_while(|c| c.is_digit(10));
                match self.stream.here {
                    Some('.') => {
                        self.stream.advance();
                        self.stream.skip_while(|c| c.is_digit(10));
                        match self.stream.here {
                            Some('e') | Some('E') => {
                                self.stream.advance();
                                match self.stream.here {
                                    Some('-') | Some('+') | Some('0'..='9') => {
                                        self.stream.advance();
                                        self.stream.skip_while(|c| c.is_digit(10));
                                    }
                                    _ => return Err(Error::ExpectedExponent),
                                }
                            }
                            _ => {}
                        };
                    }
                    Some('e') | Some('E') => {
                        self.stream.advance();
                        match self.stream.here {
                            Some('-') | Some('+') | Some('0'..='9') => {
                                self.stream.advance();
                                self.stream.skip_while(|c| c.is_digit(10));
                            }
                            _ => return Err(Error::ExpectedExponent),
                        }
                    }
                    _ => {}
                };
                Ok(Tt::NumLitDec(self.stream.str_from(start)))
            }
            e if e.is_alphanumeric() || e == '$' || e == '_' => {
                self.stream.skip_while(|c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_' | '\u{200C}' | '\u{200D}' => true,
                    _ => c.is_alphanumeric(),
                });
                let id = self.stream.str_from(start);
                match id {
                    "null" | "true" | "false" | "await" | "break" | "case" | "catch" | "class"
                    | "const" | "continue" | "debugger" | "default" | "delete" | "do" | "else"
                    | "export" | "extends" | "finally" | "for" | "function" | "if" | "import"
                    | "in" | "instanceof" | "new" | "return" | "super" | "switch" | "this"
                    | "throw" | "try" | "typeof" | "var" | "void" | "while" | "with" | "yield" => {
                        Ok(Tt::Keyword(id))
                    }
                    _ => Ok(Tt::Id(id)),
                }
            }
            _ => Err(Error::Unexpected(here)),
        }
    }
}

impl<'s> Stream<'s> {
    fn str_from(&self, start: &'s str) -> &'s str {
        let a = self.curr.as_ptr() as usize;
        let b = start.as_ptr() as usize;
        &start[..(a - b)]
    }

    fn eat(&mut self, c: char) -> bool {
        match self.here {
            Some(cc) if c == cc => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn skip_while<F>(&mut self, mut f: F)
    where
        F: FnMut(char) -> bool,
    {
        while let Some(c) = self.here {
            if f(c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.curr = self.ncurr;
        self.ncurr = self.chars.as_str();
        let next = self.chars.next();

        std::mem::replace(&mut self.here, std::mem::replace(&mut self.next, next))
    }
}
