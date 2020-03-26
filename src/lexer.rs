use std::collections::HashSet;

pub fn get_deps(source: &str) -> HashSet<String> {
    let mut lex = Lexer::new(source);
    let mut deps = HashSet::new();
    loop {
        match lex.advance().tt {
            Tt::Id("require") => {
                if lex.advance().tt == Tt::Lparen {
                    match lex.advance().tt {
                        Tt::StrLitSgl(s) | Tt::StrLitDbl(s) => {
                            if lex.advance().tt == Tt::Rparen {
                                deps.insert(String::from(&s[1..s.len() - 1]));
                            }
                        }
                        _ => {}
                    }
                }
            }
            Tt::Eof => return deps,
            _ => {}
        }
    }
}

// Following code taken from esparse crate

use std::{char, mem};
use unicode_xid::UnicodeXID;

macro_rules! matches {
    ($expression:expr, $($pattern:tt)+) => {
        match $expression {
            $($pattern)+ => true,
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'s> {
    stream: Stream<'s>,
    here: Tok<'s>,
    frame: LexFrame,
    stack: Vec<LexFrame>,
    error: Option<Error>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tok<'s> {
    pub tt: Tt<'s>,
    pub span: (usize, usize),
}

#[derive(Debug)]
pub struct Stream<'s> {
    input: &'s str,
    pos: usize,
    here: Option<char>,
    next_pos: usize,
    next_width: usize,
    next: Option<char>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    ExpectedExponent,
    UnterminatedTemplateLiteral,
    UnterminatedStringLiteral,
    UnterminatedRegExpLiteral,
    UnterminatedMultilineComment,
    UnmatchedRbrace,
    Unexpected(char),
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: (usize, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LexFrame {
    Outer,
    Template,
    Brace,
}

#[allow(missing_docs)]
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
    Lbrace,
    Rbrace,
    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Eof,
    Err,
}

macro_rules! eat_s {
    (@collect $stream:expr, { $($($($p:tt)...+)|+ => $e:expr ,)* }, _ => $else:expr $(,)*) => {
        match $stream.here {
            $($(Some($($p)..=+))|+ => {
                $stream.advance();
                $e
            })*
            _ => $else
        }
    };
    (@collect $stream:expr, { $($($($p:tt)...+)|+ => $e:expr ,)* }, $($($q:tt)...+)|+ => $f:expr, $($t:tt)+) => {
        eat_s!(@collect $stream, { $($($($p)...+)|+ => $e ,)* $($($q)...+)|+ => $f, }, $($t)+)
    };
    ($stream:expr, $($t:tt)+) => {
        eat_s!(@collect $stream, {}, $($t)+)
    };
}

impl<'s> Lexer<'s> {
    #[inline]
    pub fn new(input: &'s str) -> Self {
        let mut stream = Stream {
            input,
            pos: 0,
            here: None,
            next_pos: 0,
            next_width: 0,
            next: None,
        };
        stream.advance();
        stream.advance();
        let mut lexer = Lexer {
            stream,
            here: Tok {
                tt: Tt::Eof,
                span: (0, 0),
            },
            frame: LexFrame::Outer,
            stack: Vec::new(),
            error: None,
        };
        lexer.advance();
        lexer
    }

    pub fn advance(&mut self) -> Tok<'s> {
        let tok = self.read_tok();
        mem::replace(&mut self.here, tok)
    }

    #[inline(always)]
    fn read_tok(&mut self) -> Tok<'s> {
        match self.stream.skip_ws() {
            Some(x) => x,
            None => {
                let span = (self.stream.pos, self.stream.pos);
                self.error = Some(Error {
                    kind: ErrorKind::UnterminatedMultilineComment,
                    span,
                });
                while let Some(_) = self.stream.advance() {}
                return Tok { tt: Tt::Err, span };
            }
        };

        let start = self.stream.pos;
        let here = match self.stream.advance() {
            Some(c) => c,
            None => {
                return Tok {
                    tt: Tt::Eof,
                    span: (start, start),
                }
            }
        };

        macro_rules! mark_error {
            ($kind:expr) => {{
                let span = (start, self.stream.pos);
                self.error = Some(Error {
                    kind: $kind,
                    span: (start, self.stream.pos),
                });
                while let Some(_) = self.stream.advance() {}
                return Tok { tt: Tt::Err, span };
            }};
        }

        let tt = match here {
            '{' => {
                self.stack
                    .push(mem::replace(&mut self.frame, LexFrame::Brace));
                Tt::Lbrace
            }
            '(' => Tt::Lparen,
            ')' => Tt::Rparen,
            '[' => Tt::Lbracket,
            ']' => Tt::Rbracket,
            ';' => Tt::Operator(";"),
            ',' => Tt::Operator(","),

            '<' => eat_s!(self.stream,
                '<' => eat_s!(self.stream,
                    '=' => Tt::Operator("<<="),
                    _ => Tt::Operator("<<"),
                ),
                '=' => Tt::Operator("<="),
                _ => Tt::Operator("<"),
            ),
            '>' => eat_s!(self.stream,
                '>' => eat_s!(self.stream,
                    '>' => eat_s!(self.stream,
                        '=' => Tt::Operator(">>>="),
                        _ => Tt::Operator(">>>"),
                    ),
                    '=' => Tt::Operator(">>="),
                    _ => Tt::Operator(">>"),
                ),
                '=' => Tt::Operator(">="),
                _ => Tt::Operator(">"),
            ),
            '=' => eat_s!(self.stream,
                '>' => Tt::Operator("=>"),
                '=' => eat_s!(self.stream,
                    '=' => Tt::Operator("==="),
                    _ => Tt::Operator("=="),
                ),
                _ => Tt::Operator("="),
            ),
            '!' => eat_s!(self.stream,
                '=' => eat_s!(self.stream,
                    '=' => Tt::Operator("!=="),
                    _ => Tt::Operator("!="),
                ),
                _ => Tt::Operator("!"),
            ),
            '+' => eat_s!(self.stream,
                '+' => Tt::Operator("++"),
                '=' => Tt::Operator("+="),
                _ => Tt::Operator("+"),
            ),
            '-' => eat_s!(self.stream,
                '-' => Tt::Operator("--"),
                '=' => Tt::Operator("-="),
                _ => Tt::Operator("-"),
            ),
            '*' => eat_s!(self.stream,
                '*' => eat_s!(self.stream,
                    '=' => Tt::Operator("**="),
                    _ => Tt::Operator("**"),
                ),
                '=' => Tt::Operator("*="),
                _ => Tt::Operator("*"),
            ),
            '%' => eat_s!(self.stream,
                '=' => Tt::Operator("%="),
                _ => Tt::Operator("%"),
            ),
            '&' => eat_s!(self.stream,
                '&' => Tt::Operator("&&"),
                '=' => Tt::Operator("&="),
                _ => Tt::Operator("&"),
            ),
            '|' => eat_s!(self.stream,
                '|' => Tt::Operator("||"),
                '=' => Tt::Operator("|="),
                _ => Tt::Operator("|"),
            ),
            '^' => eat_s!(self.stream,
                '=' => Tt::Operator("^="),
                _ => Tt::Operator("^"),
            ),
            '~' => Tt::Operator("~"),
            '?' => Tt::Operator("?"),
            ':' => Tt::Operator(":"),

            '}' => match self.frame {
                LexFrame::Template => {
                    let result;
                    loop {
                        match self.stream.advance() {
                            Some('\\') => {
                                if let Some('\u{000D}') = self.stream.advance() {
                                    self.stream.eat('\u{000A}');
                                }
                            }
                            Some('`') => {
                                self.frame = match self.stack.pop() {
                                    Some(f) => f,
                                    None => unreachable!(),
                                };
                                result = Tt::TemplateEnd(self.stream.str_from(start));
                                break;
                            }
                            Some('$') => {
                                if self.stream.eat('{') {
                                    result = Tt::TemplateMiddle(self.stream.str_from(start));
                                    break;
                                }
                            }
                            Some(_) => {}
                            None => mark_error!(ErrorKind::UnterminatedTemplateLiteral),
                        }
                    }
                    result
                }
                LexFrame::Brace => {
                    self.frame = match self.stack.pop() {
                        Some(f) => f,
                        None => unreachable!(),
                    };
                    Tt::Rbrace
                }
                LexFrame::Outer => mark_error!(ErrorKind::UnmatchedRbrace),
            },
            '`' => {
                let result;
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            if let Some('\u{000D}') = self.stream.advance() {
                                self.stream.eat('\u{000A}');
                            }
                        }
                        Some('`') => {
                            result = Tt::TemplateNoSub(self.stream.str_from(start));
                            break;
                        }
                        Some('$') => {
                            if self.stream.eat('{') {
                                self.stack
                                    .push(mem::replace(&mut self.frame, LexFrame::Template));
                                result = Tt::TemplateStart(self.stream.str_from(start));
                                break;
                            }
                        }
                        Some(_) => {}
                        None => mark_error!(ErrorKind::UnterminatedTemplateLiteral),
                    }
                }
                result
            }

            '"' => {
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            if let Some('\u{000D}') = self.stream.advance() {
                                self.stream.eat('\u{000A}');
                            }
                        }
                        Some('"') => break,
                        Some('\u{000A}') | Some('\u{000D}') | Some('\u{2028}')
                        | Some('\u{2029}') | None => {
                            mark_error!(ErrorKind::UnterminatedStringLiteral)
                        }
                        Some(_) => {}
                    }
                }
                Tt::StrLitDbl(self.stream.str_from(start))
            }
            '\'' => {
                loop {
                    match self.stream.advance() {
                        Some('\\') => {
                            if let Some('\u{000D}') = self.stream.advance() {
                                self.stream.eat('\u{000A}');
                            }
                        }
                        Some('\'') => break,
                        Some('\u{000A}') | Some('\u{000D}') | Some('\u{2028}')
                        | Some('\u{2029}') | None => {
                            mark_error!(ErrorKind::UnterminatedStringLiteral)
                        }
                        Some(_) => {}
                    }
                }
                Tt::StrLitSgl(self.stream.str_from(start))
            }

            '/' => match self.here.tt {
                Tt::Rparen
                | Tt::Rbracket
                | Tt::TemplateEnd(_)
                | Tt::TemplateNoSub(_)
                | Tt::StrLitSgl(_)
                | Tt::StrLitDbl(_)
                | Tt::RegExpLit(_, _)
                | Tt::NumLitBin(_)
                | Tt::NumLitOct(_)
                | Tt::NumLitDec(_)
                | Tt::NumLitHex(_)
                | Tt::Id(_)
                | Tt::Keyword("this")
                | Tt::Keyword("super") => eat_s!(self.stream,
                    '=' => Tt::Operator("/="),
                    _ => Tt::Operator("/"),
                ),
                _ => {
                    loop {
                        match self.stream.advance() {
                            Some('\\') => {
                                if let Some('\u{000D}') = self.stream.advance() {
                                    self.stream.eat('\u{000A}');
                                }
                            }
                            Some('/') => break,
                            Some('[') => loop {
                                match self.stream.advance() {
                                    Some('\\') => {
                                        if let Some('\u{000D}') = self.stream.advance() {
                                            self.stream.eat('\u{000A}');
                                        }
                                    }
                                    Some(']') => break,
                                    Some('\u{000A}') | Some('\u{000D}') | Some('\u{2028}')
                                    | Some('\u{2029}') | None => {
                                        mark_error!(ErrorKind::UnterminatedRegExpLiteral)
                                    }
                                    Some(_) => {}
                                }
                            },
                            Some('\u{000A}') | Some('\u{000D}') | Some('\u{2028}')
                            | Some('\u{2029}') | None => {
                                mark_error!(ErrorKind::UnterminatedRegExpLiteral)
                            }
                            Some(_) => {}
                        }
                    }
                    let flags_start = self.stream.pos;
                    self.stream.skip_while(|c| match c {
                        'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_' | '\u{200C}' | '\u{200D}' => {
                            true
                        }
                        _ => UnicodeXID::is_xid_continue(c),
                    });
                    let source = self.stream.str_from(start);
                    let flags = self.stream.str_from(flags_start);
                    Tt::RegExpLit(source, flags)
                }
            },
            '.' => match self.stream.here {
                Some('.') => match self.stream.next {
                    Some('.') => {
                        self.stream.advance();
                        self.stream.advance();
                        Tt::Operator("...")
                    }
                    Some(_) | None => Tt::Operator("."),
                },
                Some('0'..='9') => {
                    self.stream.advance();
                    self.stream.skip_while(|c| c.is_digit(10));
                    eat_s!(self.stream,
                        'e' | 'E' => eat_s!(self.stream,
                            '-' | '+' | '0'...'9' => {
                                self.stream.skip_while(|c| c.is_digit(10));
                            },
                            _ => {
                                mark_error!(ErrorKind::ExpectedExponent)
                            },
                        ),
                        _ => {},
                    );
                    Tt::NumLitDec(self.stream.str_from(start))
                }
                Some(_) | None => Tt::Operator("."),
            },
            '0' => eat_s!(self.stream,
                'b' | 'B' => {
                    self.stream.skip_while(|c| c.is_digit(2));
                    Tt::NumLitBin(self.stream.str_from(start))
                },
                'o' | 'O' => {
                    self.stream.skip_while(|c| c.is_digit(8));
                    Tt::NumLitOct(self.stream.str_from(start))
                },
                'x' | 'X' => {
                    self.stream.skip_while(|c| c.is_digit(16));
                    Tt::NumLitHex(self.stream.str_from(start))
                },
                '.' => {
                    self.stream.skip_while(|c| c.is_digit(10));
                    eat_s!(self.stream,
                        'e' | 'E' => eat_s!(self.stream,
                            '-' | '+' | '0'...'9' => {
                                self.stream.skip_while(|c| c.is_digit(10));
                                Tt::NumLitDec(self.stream.str_from(start))
                            },
                            _ => {
                                mark_error!(ErrorKind::ExpectedExponent)
                            },
                        ),
                        _ => {
                            Tt::NumLitDec(self.stream.str_from(start))
                        },
                    )
                },
                'e' | 'E' => eat_s!(self.stream,
                    '-' | '+' | '0'...'9' => {
                        self.stream.skip_while(|c| c.is_digit(10));
                        Tt::NumLitDec(self.stream.str_from(start))
                    },
                    _ => {
                        mark_error!(ErrorKind::ExpectedExponent)
                    },
                ),
                _ => Tt::NumLitDec(self.stream.str_from(start)),
            ),
            '1'..='9' => {
                self.stream.skip_while(|c| c.is_digit(10));
                eat_s!(self.stream,
                    '.' => {
                        self.stream.skip_while(|c| c.is_digit(10));
                        eat_s!(self.stream,
                            'e' | 'E' => eat_s!(self.stream,
                                '-' | '+' | '0'...'9' => {
                                    self.stream.skip_while(|c| c.is_digit(10));
                                },
                                _ => {
                                    mark_error!(ErrorKind::ExpectedExponent)
                                },
                            ),
                            _ => {},
                        );
                    },
                    'e' | 'E' => eat_s!(self.stream,
                        '-' | '+' | '0'...'9' => {
                            self.stream.skip_while(|c| c.is_digit(10));
                        },
                        _ => {
                            mark_error!(ErrorKind::ExpectedExponent)
                        },
                    ),
                    _ => {},
                );
                Tt::NumLitDec(self.stream.str_from(start))
            }

            _ => {
                if matches!(here, 'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_')
                    || UnicodeXID::is_xid_start(here)
                {
                    self.stream.skip_while(|c| match c {
                        'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_' | '\u{200C}' | '\u{200D}' => {
                            true
                        }
                        _ => UnicodeXID::is_xid_continue(c),
                    });
                    let id = self.stream.str_from(start);
                    match id {
                        "null" | "true" | "false" | "await" | "break" | "case" | "catch"
                        | "class" | "const" | "continue" | "debugger" | "default" | "delete"
                        | "do" | "else" | "export" | "extends" | "finally" | "for" | "function"
                        | "if" | "import" | "in" | "instanceof" | "new" | "return" | "super"
                        | "switch" | "this" | "throw" | "try" | "typeof" | "var" | "void"
                        | "while" | "with" | "yield" => Tt::Keyword(id),
                        _ => Tt::Id(id),
                    }
                } else {
                    mark_error!(ErrorKind::Unexpected(here))
                }
            }
        };
        Tok {
            tt,
            span: (start, self.stream.pos),
        }
    }
}

impl<'s> Stream<'s> {
    #[inline]
    fn str_from(&self, start: usize) -> &'s str {
        &self.input[start..self.pos]
    }

    #[inline]
    fn eat(&mut self, c: char) -> bool {
        match self.here {
            Some(cc) if c == cc => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    #[inline]
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

    #[inline]
    fn skip_ws(&mut self) -> Option<(&'s str, bool)> {
        let start_pos = self.pos;
        while let Some(c) = self.here {
            match c {
                _ if c.is_whitespace() => {
                    self.advance();
                }
                '/' => match self.next {
                    Some('*') => {
                        self.advance();
                        self.advance();
                        'outer: loop {
                            match self.here {
                                Some('*') => loop {
                                    self.advance();
                                    match self.here {
                                        Some('/') => {
                                            self.advance();
                                            break 'outer;
                                        }
                                        Some('*') => {}
                                        Some(_) => {
                                            self.advance();
                                            break;
                                        }
                                        None => return None,
                                    }
                                },
                                Some(_) => {
                                    self.advance();
                                }
                                None => return None,
                            }
                        }
                    }
                    Some('/') => {
                        self.advance();
                        self.advance();
                        self.skip_while(|c| c.is_whitespace());
                    }
                    _ => break,
                },
                _ => break,
            }
        }
        Some((self.str_from(start_pos), false))
    }

    #[inline]
    fn advance(&mut self) -> Option<char> {
        let next = {
            self.pos = self.next_pos;
            self.next_pos += self.next_width;

            if self.next_pos >= self.input.len() {
                self.next_width = 0;
                None
            } else {
                let (next, width) = unsafe { char_at_unchecked(self.input, self.next_pos) };
                self.next_width = width;
                Some(next)
            }
        };
        mem::replace(&mut self.here, mem::replace(&mut self.next, next))
    }
}

#[inline]
unsafe fn char_at_unchecked(s: &str, n: usize) -> (char, usize) {
    let b = s.as_bytes();
    let b0 = b[n];

    let (width, code) = if b0 & 0x80 == 0 {
        (1, b0 as u32)
    } else if b0 & 0xe0 == 0xc0 {
        (2, ((b0 & 0x1f) as u32) << 6 | (b[n + 1] & 0x3f) as u32)
    } else if b0 & 0xf0 == 0xe0 {
        (
            3,
            ((b0 & 0x0f) as u32) << 12 | ((b[n + 1] & 0x3f) as u32) << 6 | (b[n + 2] & 0x3f) as u32,
        )
    } else if b0 & 0xf1 == 0xf0 {
        (
            4,
            ((b0 & 0x07) as u32) << 18
                | ((b[n + 1] & 0x3f) as u32) << 12
                | ((b[n + 2] & 0x3f) as u32) << 6
                | (b[n + 3] & 0x3f) as u32,
        )
    } else {
        panic!("invalid utf-8 sequence")
    };
    (char::from_u32_unchecked(code), width)
}
