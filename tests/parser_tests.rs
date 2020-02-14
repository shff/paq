use paq::js::parser::{block, expression, Expression, Operator, Statement};

#[test]
fn test_comments() {
    assert_eq!(
        block("  // hello\n asdf"),
        Ok((
            "",
            vec![Statement::Expression(Expression::Ident(String::from(
                "asdf"
            )))]
        ))
    );
}

#[test]
fn test_statement() {
    assert_eq!(block("continue}"), Ok(("}", vec![Statement::Continue])));
    assert_eq!(
        block("{continue}"),
        Ok(("", vec![Statement::Block(vec![Statement::Continue])]))
    );
    assert_eq!(
        block("continue;continue;"),
        Ok(("", vec![Statement::Continue, Statement::Continue]))
    );
    assert_eq!(
        block(" continue ; continue ; "),
        Ok((" ", vec![Statement::Continue, Statement::Continue]))
    );
    assert_eq!(block("continue"), Ok(("", vec![Statement::Continue])));
    assert_eq!(
        block("continue\n1"),
        Ok((
            "",
            vec![
                Statement::Continue,
                Statement::Expression(Expression::Double(1.0))
            ]
        ))
    );
    assert_eq!(
        block("continue; 1"),
        Ok((
            "",
            vec![
                Statement::Continue,
                Statement::Expression(Expression::Double(1.0))
            ]
        ))
    );
    assert_eq!(block("break;"), Ok(("", vec![Statement::Break])));
    assert_eq!(
        block(" break ; break ; "),
        Ok((" ", vec![Statement::Break, Statement::Break]))
    );
    assert_eq!(block("break\n"), Ok(("", vec![Statement::Break])));
    assert_eq!(
        block("return 1;"),
        Ok(("", vec![Statement::Return(Some(Expression::Double(1.0)))]))
    );
    assert_eq!(
        block("throw 1;"),
        Ok(("", vec![Statement::Throw(Some(Expression::Double(1.0)))]))
    );
    assert_eq!(
        block("return 1\n"),
        Ok(("", vec![Statement::Return(Some(Expression::Double(1.0)))]))
    );
    assert_eq!(
        block("return\n1\n"),
        Ok(("", vec![Statement::Return(Some(Expression::Double(1.0)))]))
    );
    assert_eq!(
        block("return; 1\n"),
        Ok((
            "",
            vec![
                Statement::Return(None),
                Statement::Expression(Expression::Double(1.0))
            ]
        ))
    );
    assert_eq!(
        block(" return 1 ; return 1 ; "),
        Ok((
            " ",
            vec![
                Statement::Return(Some(Expression::Double(1.0))),
                Statement::Return(Some(Expression::Double(1.0)))
            ]
        ))
    );
    assert_eq!(block("return;"), Ok(("", vec![Statement::Return(None)])));
    assert_eq!(block("return"), Ok(("", vec![Statement::Return(None)])));
    assert_eq!(block("return\n"), Ok(("", vec![Statement::Return(None)])));
    assert_eq!(
        block("a = 2;"),
        Ok((
            "",
            vec![Statement::Expression(Expression::Binary(
                Operator::Assign,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Double(2.0))
            ))]
        ))
    );
    assert_eq!(
        block("var a = 2;"),
        Ok((
            "",
            vec![Statement::Declaration((
                Operator::Var,
                vec![Expression::Binary(
                    Operator::Assign,
                    Box::new(Expression::Ident(String::from("a"))),
                    Box::new(Expression::Double(2.0))
                )]
            ))]
        ))
    );
    assert_eq!(
        block("let a = 2, b = 3"),
        Ok((
            "",
            vec![Statement::Declaration((
                Operator::Let,
                vec![
                    Expression::Binary(
                        Operator::Assign,
                        Box::new(Expression::Ident(String::from("a"))),
                        Box::new(Expression::Double(2.0))
                    ),
                    Expression::Binary(
                        Operator::Assign,
                        Box::new(Expression::Ident(String::from("b"))),
                        Box::new(Expression::Double(3.0))
                    ),
                ]
            ))]
        ))
    );
    assert_eq!(
        block("let a=2,b=3"),
        Ok((
            "",
            vec![Statement::Declaration((
                Operator::Let,
                vec![
                    Expression::Binary(
                        Operator::Assign,
                        Box::new(Expression::Ident(String::from("a"))),
                        Box::new(Expression::Double(2.0))
                    ),
                    Expression::Binary(
                        Operator::Assign,
                        Box::new(Expression::Ident(String::from("b"))),
                        Box::new(Expression::Double(3.0))
                    ),
                ]
            ))]
        ))
    );
    assert_eq!(
        block("const x = [];"),
        Ok((
            "",
            vec![Statement::Declaration((
                Operator::Const,
                vec![Expression::Binary(
                    Operator::Assign,
                    Box::new(Expression::Ident(String::from("x"))),
                    Box::new(Expression::List(vec![]))
                )]
            ))]
        ))
    );
    assert_eq!(
        block("a = 2"),
        Ok((
            "",
            vec![Statement::Expression(Expression::Binary(
                Operator::Assign,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Double(2.0))
            ))]
        ))
    );
    assert_eq!(
        block("a = G"),
        Ok((
            "",
            vec![Statement::Expression(Expression::Binary(
                Operator::Assign,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Ident(String::from("G")))
            ))]
        ))
    );
    assert_eq!(
        block("abc = G"),
        Ok((
            "",
            vec![Statement::Expression(Expression::Binary(
                Operator::Assign,
                Box::new(Expression::Ident(String::from("abc"))),
                Box::new(Expression::Ident(String::from("G")))
            ))]
        ))
    );
    assert_eq!(
        block("const empty = G()"),
        Ok((
            "",
            vec![Statement::Declaration((
                Operator::Const,
                vec![Expression::Binary(
                    Operator::Assign,
                    Box::new(Expression::Ident(String::from("empty"))),
                    Box::new(Expression::Binary(
                        Operator::Call,
                        Box::new(Expression::Ident(String::from("G"))),
                        Box::new(Expression::Args(vec![]))
                    ))
                )]
            ))]
        ))
    );
    assert_eq!(
        block("z"),
        Ok((
            "",
            vec![Statement::Expression(Expression::Ident(String::from("z")))]
        ))
    );
    assert_eq!(
        block("if(true){return;}"),
        Ok((
            "",
            vec![Statement::If((
                Box::new(Expression::Ident(String::from("true"))),
                Box::new(Statement::Block(vec![Statement::Return(None)])),
                None
            ))]
        ))
    );
    assert_eq!(
        block("if(true)return;"),
        Ok((
            "",
            vec![Statement::If((
                Box::new(Expression::Ident(String::from("true"))),
                Box::new(Statement::Return(None)),
                None
            ))]
        ))
    );
    assert_eq!(
        block("if(true)return;else break;"),
        Ok((
            "",
            vec![Statement::If((
                Box::new(Expression::Ident(String::from("true"))),
                Box::new(Statement::Return(None)),
                Some(Box::new(Statement::Break))
            ))]
        ))
    );
    assert_eq!(
        block("if ( true )\n {\n return; \n}\n else \n { break; }"),
        Ok((
            "",
            vec![Statement::If((
                Box::new(Expression::Ident(String::from("true"))),
                Box::new(Statement::Block(vec![Statement::Return(None)])),
                Some(Box::new(Statement::Block(vec![Statement::Break])))
            ))]
        ))
    );
    assert_eq!(
        block("while(true){return;}"),
        Ok((
            "",
            vec![Statement::While((
                Box::new(Expression::Ident(String::from("true"))),
                Box::new(Statement::Block(vec![Statement::Return(None)]))
            ))]
        ))
    );
    assert_eq!(
        block(" while ( true ) return ; "),
        Ok((
            " ",
            vec![Statement::While((
                Box::new(Expression::Ident(String::from("true"))),
                Box::new(Statement::Return(None))
            ))]
        ))
    );
    assert_eq!(
        block("for(0;0;0){return;}"),
        Ok((
            "",
            vec![Statement::For((
                (
                    Some(Box::new(Statement::Expression(Expression::Double(0.0)))),
                    Some(Expression::Double(0.0)),
                    Some(Expression::Double(0.0))
                ),
                Box::new(Statement::Block(vec![Statement::Return(None)]))
            ))]
        ))
    );
    assert_eq!(
        block("for ( 0 ; 0 ; 0 ) return ; "),
        Ok((
            " ",
            vec![Statement::For((
                (
                    Some(Box::new(Statement::Expression(Expression::Double(0.0)))),
                    Some(Expression::Double(0.0)),
                    Some(Expression::Double(0.0))
                ),
                Box::new(Statement::Return(None))
            ))]
        ))
    );
    assert_eq!(
        block("for ( ; ; ) return ; "),
        Ok((
            " ",
            vec![Statement::For((
                (None, None, None),
                Box::new(Statement::Return(None))
            ))]
        ))
    );
}

#[test]
fn test_string() {
    assert_eq!(
        expression("\"\""),
        Ok(("", Expression::Str(String::from(""))))
    );
    assert_eq!(
        expression(" \"\" "),
        Ok((" ", Expression::Str(String::from(""))))
    );
    assert_eq!(
        expression(" \"a\" "),
        Ok((" ", Expression::Str(String::from("a"))))
    );
    assert_eq!(
        expression(" \"Example\" "),
        Ok((" ", Expression::Str(String::from("Example"))))
    );
    assert_eq!(
        expression("\"\\     a\""),
        Ok(("", Expression::Str(String::from("a"))))
    );
    assert_eq!(
        expression("\"\\   b  a\""),
        Ok(("", Expression::Str(String::from("b  a"))))
    );
    assert_eq!(
        expression("\"\\n\\n\""),
        Ok(("", Expression::Str(String::from("\n\n"))))
    );
    assert_eq!(
        expression("\"✅\""),
        Ok(("", Expression::Str(String::from("✅"))))
    );
    assert_eq!(
        expression("\"\\n\""),
        Ok(("", Expression::Str(String::from("\n"))))
    );
    assert_eq!(
        expression("\"\\r\""),
        Ok(("", Expression::Str(String::from("\r"))))
    );
    assert_eq!(
        expression("\"\\t\""),
        Ok(("", Expression::Str(String::from("\t"))))
    );
    assert_eq!(
        expression("\"\\b\""),
        Ok(("", Expression::Str(String::from("\u{08}"))))
    );
    assert_eq!(
        expression("\"\\v\""),
        Ok(("", Expression::Str(String::from("\u{0B}"))))
    );
    assert_eq!(
        expression("\"\\f\""),
        Ok(("", Expression::Str(String::from("\u{0C}"))))
    );
    assert_eq!(
        expression("\"\\\\\""),
        Ok(("", Expression::Str(String::from("\\"))))
    );
    assert_eq!(
        expression("\"\\/\""),
        Ok(("", Expression::Str(String::from("/"))))
    );
    assert_eq!(
        expression("\"\\\"\""),
        Ok(("", Expression::Str(String::from("\""))))
    );
    assert_eq!(
        expression("\"\\\'\""),
        Ok(("", Expression::Str(String::from("'"))))
    );
}

#[test]
fn test_single_quoted_string() {
    assert_eq!(
        expression("''"),
        Ok(("", Expression::Str(String::from(""))))
    );
    assert_eq!(
        expression(" '' "),
        Ok((" ", Expression::Str(String::from(""))))
    );
    assert_eq!(
        expression(" 'a' "),
        Ok((" ", Expression::Str(String::from("a"))))
    );
    assert_eq!(
        expression(" 'Example' "),
        Ok((" ", Expression::Str(String::from("Example"))))
    );
    assert_eq!(
        expression("'\\     a'"),
        Ok(("", Expression::Str(String::from("a"))))
    );
    assert_eq!(
        expression("'\\   b  a'"),
        Ok(("", Expression::Str(String::from("b  a"))))
    );
    assert_eq!(
        expression("'\\n\\n'"),
        Ok(("", Expression::Str(String::from("\n\n"))))
    );
    assert_eq!(
        expression("\"✅\""),
        Ok(("", Expression::Str(String::from("✅"))))
    );
    assert_eq!(
        expression("'\\n'"),
        Ok(("", Expression::Str(String::from("\n"))))
    );
    assert_eq!(
        expression("'\\r'"),
        Ok(("", Expression::Str(String::from("\r"))))
    );
    assert_eq!(
        expression("'\\t'"),
        Ok(("", Expression::Str(String::from("\t"))))
    );
    assert_eq!(
        expression("'\\b'"),
        Ok(("", Expression::Str(String::from("\u{08}"))))
    );
    assert_eq!(
        expression("'\\v'"),
        Ok(("", Expression::Str(String::from("\u{0B}"))))
    );
    assert_eq!(
        expression("'\\f'"),
        Ok(("", Expression::Str(String::from("\u{0C}"))))
    );
    assert_eq!(
        expression("'\\\\'"),
        Ok(("", Expression::Str(String::from("\\"))))
    );
    assert_eq!(
        expression("'\\/'"),
        Ok(("", Expression::Str(String::from("/"))))
    );
    assert_eq!(
        expression("'\\\"'"),
        Ok(("", Expression::Str(String::from("\""))))
    );
    assert_eq!(
        expression("'\\''"),
        Ok(("", Expression::Str(String::from("'"))))
    );
}

#[test]
fn test_double() {
    assert_eq!(expression("0"), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("1"), Ok(("", Expression::Double(1.0))));
    assert_eq!(expression("2.2"), Ok(("", Expression::Double(2.2))));
    assert_eq!(expression("3."), Ok(("", Expression::Double(3.0))));
    assert_eq!(expression(".4"), Ok(("", Expression::Double(0.4))));
    assert_eq!(expression("1e2"), Ok(("", Expression::Double(100.0))));
    assert_eq!(expression("1e-2"), Ok(("", Expression::Double(0.01))));
    assert_eq!(expression("0"), Ok(("", Expression::Double(0.0))));
    assert_eq!(
        expression("123456789"),
        Ok(("", Expression::Double(123456789.0)))
    );
    assert_eq!(expression("0."), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123."), Ok(("", Expression::Double(123.0))));
    assert_eq!(expression(".012300"), Ok(("", Expression::Double(0.0123))));
    assert_eq!(expression("0.012300"), Ok(("", Expression::Double(0.0123))));
    assert_eq!(
        expression("123.045600"),
        Ok(("", Expression::Double(123.0456)))
    );
    assert_eq!(expression(".123e0"), Ok(("", Expression::Double(0.123))));
    assert_eq!(expression("0.123e0"), Ok(("", Expression::Double(0.123))));
    assert_eq!(
        expression("123.456e0"),
        Ok(("", Expression::Double(123.456)))
    );
    assert_eq!(expression(".123e01"), Ok(("", Expression::Double(1.23))));
    assert_eq!(expression("0.123e01"), Ok(("", Expression::Double(1.23))));
    assert_eq!(
        expression("123.456e02"),
        Ok(("", Expression::Double(12345.6)))
    );
    assert_eq!(expression(".123e+4"), Ok(("", Expression::Double(1230.0))));
    assert_eq!(expression("0.123e+4"), Ok(("", Expression::Double(1230.0))));
    assert_eq!(
        expression("123.456e+4"),
        Ok(("", Expression::Double(1234560.0)))
    );
    assert_eq!(
        expression(".123e-4"),
        Ok(("", Expression::Double(0.0000123)))
    );
    assert_eq!(
        expression("0.123e-4"),
        Ok(("", Expression::Double(0.0000123)))
    );
    assert_eq!(
        expression("123.456e-4"),
        Ok(("", Expression::Double(0.0123456)))
    );
    assert_eq!(expression("0e0"), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123e0"), Ok(("", Expression::Double(123.0))));
    assert_eq!(expression("0e01"), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123e02"), Ok(("", Expression::Double(12300.0))));
    assert_eq!(expression("0e+4"), Ok(("", Expression::Double(0.0))));
    assert_eq!(
        expression("123e+4"),
        Ok(("", Expression::Double(1230000.0)))
    );
    assert_eq!(expression("0e-4"), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123e-4"), Ok(("", Expression::Double(0.0123))));
}

#[test]
fn test_octal() {
    assert_eq!(expression("0o123"), Ok(("", Expression::Octal(0o123))));
    assert_eq!(expression("0o111"), Ok(("", Expression::Octal(0o111))));
    assert_eq!(expression("0o0"), Ok(("", Expression::Octal(0o0))));
    assert_eq!(expression("0o03 "), Ok((" ", Expression::Octal(0o3))));
    assert_eq!(expression("0o012 "), Ok((" ", Expression::Octal(0o12))));
    assert_eq!(
        expression("0o07654321 "),
        Ok((" ", Expression::Octal(0o7654321)))
    );
}

#[test]
fn test_hexadecimal() {
    assert_eq!(expression("0x3 "), Ok((" ", Expression::Hexadecimal(0x3))));
    assert_eq!(
        expression("0x0123789"),
        Ok(("", Expression::Hexadecimal(0x0123789)))
    );
    assert_eq!(
        expression("0xABCDEF"),
        Ok(("", Expression::Hexadecimal(0xabcdef)))
    );
    assert_eq!(
        expression("0xabcdef"),
        Ok(("", Expression::Hexadecimal(0xabcdef)))
    );
}

#[test]
fn test_binarynum() {
    assert_eq!(expression("0b0"), Ok(("", Expression::BinaryNum(0b0))));
    assert_eq!(expression("0b1"), Ok(("", Expression::BinaryNum(0b1))));
    assert_eq!(
        expression("0b01010"),
        Ok(("", Expression::BinaryNum(0b01010)))
    );
    assert_eq!(
        expression("0b1010111"),
        Ok(("", Expression::BinaryNum(0b1010111)))
    );
}

#[test]
fn test_identifier() {
    assert_eq!(
        expression("hello"),
        Ok(("", Expression::Ident(String::from("hello"))))
    );
    assert_eq!(
        expression("e"),
        Ok(("", Expression::Ident(String::from("e"))))
    );
}

#[test]
fn test_list() {
    assert_eq!(expression(" [ ] "), Ok((" ", Expression::List(vec![]))));
    assert_eq!(
        expression("[[]]"),
        Ok(("", Expression::List(vec![Expression::List(vec![])])))
    );
    assert_eq!(expression("[]"), Ok(("", Expression::List(vec![]))));
    assert_eq!(
        expression("[ 1 ]"),
        Ok(("", Expression::List(vec![Expression::Double(1.0)])))
    );
    assert_eq!(
        expression("[ 1, 2 ]"),
        Ok((
            "",
            Expression::List(vec![Expression::Double(1.0), Expression::Double(2.0)])
        ))
    );
    assert_eq!(
        expression("[ 1, \"2\" ]"),
        Ok((
            "",
            Expression::List(vec![
                Expression::Double(1.0),
                Expression::Str(String::from("2"))
            ])
        ))
    );
    assert_eq!(
        expression("[ ...a, 1 ]"),
        Ok((
            "",
            Expression::List(vec![
                Expression::Splat(Box::new(Expression::Ident(String::from("a")))),
                Expression::Double(1.0)
            ])
        ))
    );
    assert_eq!(
        expression("[ ...[], 1 ]"),
        Ok((
            "",
            Expression::List(vec![
                Expression::Splat(Box::new(Expression::List(vec![]))),
                Expression::Double(1.0)
            ])
        ))
    );
}

#[test]
fn test_object() {
    assert_eq!(
        expression("{\"a\": 1}"),
        Ok((
            "",
            Expression::Object(vec![Expression::KeyValue((
                Box::new(Expression::Str(String::from("a"))),
                Box::new(Expression::Double(1.0))
            ))])
        ))
    );
    assert_eq!(
        expression("{a: 1, b: 2}"),
        Ok((
            "",
            Expression::Object(vec![
                Expression::KeyValue((
                    Box::new(Expression::Str(String::from("a"))),
                    Box::new(Expression::Double(1.0))
                )),
                Expression::KeyValue((
                    Box::new(Expression::Str(String::from("b"))),
                    Box::new(Expression::Double(2.0))
                )),
            ])
        ))
    );
    assert_eq!(
        expression("{[1]: 1, [\"a\"]: 2}"),
        Ok((
            "",
            Expression::Object(vec![
                Expression::KeyValue((
                    Box::new(Expression::Double(1.0)),
                    Box::new(Expression::Double(1.0))
                )),
                Expression::KeyValue((
                    Box::new(Expression::Str(String::from("a"))),
                    Box::new(Expression::Double(2.0))
                )),
            ])
        ))
    );
    assert_eq!(
        expression("{a: {}}"),
        Ok((
            "",
            Expression::Object(vec![Expression::KeyValue((
                Box::new(Expression::Str(String::from("a"))),
                Box::new(Expression::Object(vec![]))
            ))])
        ))
    );
    assert_eq!(
        expression("{ ...a }"),
        Ok((
            "",
            Expression::Object(vec![Expression::Splat(Box::new(Expression::Ident(
                String::from("a")
            )))])
        ))
    );
    assert_eq!(
        expression("{ ...[] }"),
        Ok((
            "",
            Expression::Object(vec![Expression::Splat(Box::new(Expression::List(vec![])))])
        ))
    );
    assert_eq!(
        expression("{ a, b }"),
        Ok((
            "",
            Expression::Object(vec![
                Expression::Ident(String::from("a")),
                Expression::Ident(String::from("b"))
            ])
        ))
    );
}

#[test]
fn test_parenthesis() {
    assert_eq!(
        expression("(1)"),
        Ok(("", Expression::Paren(Box::new(Expression::Double(1.0)))))
    );
    assert_eq!(
        expression("([])"),
        Ok(("", Expression::Paren(Box::new(Expression::List(vec![])))))
    );
    assert_eq!(
        expression(" ( 1 ) "),
        Ok((" ", Expression::Paren(Box::new(Expression::Double(1.0)))))
    );
    assert_eq!(
        expression(" ( [ ] ) "),
        Ok((" ", Expression::Paren(Box::new(Expression::List(vec![])))))
    );
}

#[test]
fn test_closure() {
    assert_eq!(
        expression("(a, b) => 1 + 1"),
        Ok((
            "",
            Expression::Closure((
                vec![
                    Expression::Parameter((String::from("a"), None)),
                    Expression::Parameter((String::from("b"), None))
                ],
                Box::new(Expression::Binary(
                    Operator::Add,
                    Box::new(Expression::Double(1.0)),
                    Box::new(Expression::Double(1.0))
                ))
            ))
        ))
    );
    assert_eq!(
        expression("(a) => ({})"),
        Ok((
            "",
            Expression::Closure((
                vec![Expression::Parameter((String::from("a"), None))],
                Box::new(Expression::Paren(Box::new(Expression::Object(vec![]))))
            ))
        ))
    );
}

#[test]
fn test_function() {
    assert_eq!(
        expression("function(){}"),
        Ok(("", Expression::Function((None, vec![], vec![]))))
    );
    assert_eq!(
        expression("function f(x, y){ return x; }"),
        Ok((
            "",
            Expression::Function((
                Some(String::from("f")),
                vec![
                    Expression::Parameter((String::from("x"), None)),
                    Expression::Parameter((String::from("y"), None))
                ],
                vec![Statement::Return(Some(Expression::Ident(String::from(
                    "x"
                ))))]
            ))
        ))
    );
    assert_eq!(
        expression("function f ( x, y) { return x }"),
        Ok((
            "",
            Expression::Function((
                Some(String::from("f")),
                vec![
                    Expression::Parameter((String::from("x"), None)),
                    Expression::Parameter((String::from("y"), None))
                ],
                vec![Statement::Return(Some(Expression::Ident(String::from(
                    "x"
                ))))]
            ))
        ))
    );
}

#[test]
fn test_generator() {
    assert_eq!(
        expression("function*() {}"),
        Ok(("", Expression::Generator((None, vec![], vec![]))))
    );
}

#[test]
fn test_mutation() {
    assert_eq!(
        expression(" 1 = 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Assign,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 += 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignAdd,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 -= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignSub,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 %= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignMod,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 *= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignMul,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 /= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignDiv,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 <<= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignLeft,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >>>= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignURight,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >>= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignRight,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 &= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignAnd,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ^= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignXor,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 |= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::AssignOr,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 = 2 = 3 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Assign,
                Box::new(Expression::Binary(
                    Operator::Assign,
                    Box::new(Expression::Double(1.0)),
                    Box::new(Expression::Double(2.0))
                )),
                Box::new(Expression::Double(3.0))
            )
        ))
    );
}

#[test]
fn test_ternary() {
    assert_eq!(
        expression("1 ? 2 : 3"),
        Ok((
            "",
            Expression::Ternary(
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0)),
                Box::new(Expression::Double(3.0))
            )
        ))
    );
}

#[test]
fn test_comparison() {
    assert_eq!(
        expression(" 1 == 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Eq,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 != 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::NotEq,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 > 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Gt,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 < 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Lt,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::GtEq,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 <= 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::LtEq,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 === 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::StrictEq,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 !== 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::StrictNotEq,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 instanceof 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::InstanceOf,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 in 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::In,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 == 2 == 3 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Eq,
                Box::new(Expression::Binary(
                    Operator::Eq,
                    Box::new(Expression::Double(1.0)),
                    Box::new(Expression::Double(2.0))
                )),
                Box::new(Expression::Double(3.0))
            )
        ))
    );
}

#[test]
fn test_logic() {
    assert_eq!(
        expression(" 1 || 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::And,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 && 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Or,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ?? 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Coalesce,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
}

#[test]
fn test_bitwise() {
    assert_eq!(
        expression(" 1 | 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::BitOr,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ^ 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::BitXor,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 & 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::BitAnd,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );

    assert_eq!(
        expression(" 1 >> 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::RBit,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >>> 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::URBit,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 << 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::LBit,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
}

#[test]
fn test_arithmetic() {
    assert_eq!(
        expression(" 1 + 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Add,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 - 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Sub,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 * 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Mul,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 / 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Div,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 % 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Mod,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ** 2 "),
        Ok((
            " ",
            Expression::Binary(
                Operator::Pow,
                Box::new(Expression::Double(1.0)),
                Box::new(Expression::Double(2.0))
            )
        ))
    );
}

#[test]
fn test_prefix() {
    assert_eq!(
        expression(" ++ 2 "),
        Ok((
            " ",
            Expression::Unary(Operator::Incr, Box::new(Expression::Double(2.0)))
        ))
    );
    assert_eq!(
        expression(" -- 2 "),
        Ok((
            " ",
            Expression::Unary(Operator::Dec, Box::new(Expression::Double(2.0)))
        ))
    );
    assert_eq!(
        expression(" + 2 "),
        Ok((
            " ",
            Expression::Unary(Operator::Add, Box::new(Expression::Double(2.0)))
        ))
    );
    assert_eq!(
        expression(" - 2 "),
        Ok((
            " ",
            Expression::Unary(Operator::Sub, Box::new(Expression::Double(2.0)))
        ))
    );
    assert_eq!(
        expression(" ! 2 "),
        Ok((
            " ",
            Expression::Unary(Operator::Not, Box::new(Expression::Double(2.0)))
        ))
    );
    assert_eq!(
        expression(" !!2 "),
        Ok((
            " ",
            Expression::Unary(
                Operator::Not,
                Box::new(Expression::Unary(
                    Operator::Not,
                    Box::new(Expression::Double(2.0))
                ))
            )
        ))
    );
    assert_eq!(
        expression(" ! ! 2 "),
        Ok((
            " ",
            Expression::Unary(
                Operator::Not,
                Box::new(Expression::Unary(
                    Operator::Not,
                    Box::new(Expression::Double(2.0))
                ))
            )
        ))
    );

    assert_eq!(
        expression("typeof a"),
        Ok((
            "",
            Expression::Unary(
                Operator::TypeOf,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression("void a"),
        Ok((
            "",
            Expression::Unary(
                Operator::Void,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression("delete a"),
        Ok((
            "",
            Expression::Unary(
                Operator::Delete,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression("await a"),
        Ok((
            "",
            Expression::Unary(
                Operator::Await,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression("yield a"),
        Ok((
            "",
            Expression::Unary(
                Operator::Yield,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression("yield* a"),
        Ok((
            "",
            Expression::Unary(
                Operator::YieldStar,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression("new a"),
        Ok((
            "",
            Expression::Unary(
                Operator::New,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
}

#[test]
fn test_postfix() {
    assert_eq!(
        expression(" a++"),
        Ok((
            "",
            Expression::Unary(
                Operator::Incr,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a--"),
        Ok((
            "",
            Expression::Unary(
                Operator::Dec,
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
}

#[test]
fn test_action() {
    assert_eq!(
        expression(" a?.a"),
        Ok((
            "",
            Expression::Binary(
                Operator::Optional,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a[a]"),
        Ok((
            "",
            Expression::Binary(
                Operator::Array,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a [ a ]"),
        Ok((
            "",
            Expression::Binary(
                Operator::Array,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a(a)"),
        Ok((
            "",
            Expression::Binary(
                Operator::Call,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Args(vec![
                    (Expression::Ident(String::from("a")))
                ]))
            )
        ))
    );
    assert_eq!(
        expression(" a ( a )"),
        Ok((
            "",
            Expression::Binary(
                Operator::Call,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Args(vec![
                    (Expression::Ident(String::from("a")))
                ]))
            )
        ))
    );
    assert_eq!(
        expression(" a.a"),
        Ok((
            "",
            Expression::Binary(
                Operator::Dot,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a()"),
        Ok((
            "",
            Expression::Binary(
                Operator::Call,
                Box::new(Expression::Ident(String::from("a"))),
                Box::new(Expression::Args(vec![]))
            )
        ))
    );
}

#[test]
fn test_complex() {
    fn assert_complete(i: &str) {
        assert!(block(i).is_ok());
        assert_eq!(block(i).unwrap().0, "");
    }

    assert_complete("a = b");
    assert_complete("1");
    assert_complete("1==1? 1+1 : 1-1");
    assert_complete("1==1? 1+1 : 1-1");
    assert_complete("-1");
    assert_complete("!1");
    assert_complete("-1");
    assert_complete("(1)");
    assert_complete("1*(1+1)");
    assert_complete("x");
    assert_complete("x");
    assert_complete("a+b+c+d");
    assert_complete("a-b-c-d");
    assert_complete("a*b*c*d");
    assert_complete("a/b/c/d");
    assert_complete("1**1**1");
    assert_complete("x*x*x");
    assert_complete("1 + 1 || 1 == 1 ^ 1 != 1/1 - 1");
    assert_complete("first += second += third");
    assert_complete("one += two /= 12");
    assert_complete("x = a && b == c + d * !z[0]++ || d ? 2 : 3");
    assert_complete(" a . b . c");
    assert_complete("a.b.c[7]");
    assert_complete("a()");
    assert_complete("a()[0]()");
    assert_complete("(a) => 1 + 1");
    assert_complete("(a) => [ 1 + 1 ]");
    assert_complete("(a) => ({a: 1})");
    assert_complete("((a) => a + 1)(1)");
    assert_complete("if (1 + 1 == 2) { return true; }");
    assert_complete("if ( true ) \n { \n return ; \n }");
    assert_complete("if (true) break; else continue;");
    assert_complete("if (true) for (0;0;0) break;");
    assert_complete("if (true) for (0;0;0) break;\nelse continue;");
    assert_complete("if (true) for (0;0;0) break; else continue;");
    assert_complete("if (true) 2; else 1;");
}

#[test]
fn test_nesting_bench() {
    let start = std::time::Instant::now();

    let elapsed = start.elapsed();
    assert!(elapsed.as_millis() < 10);
}

#[test]
fn text_fixtures() {
    fn assert_parses(path: &str) {
        let fixtures = std::env::current_dir().unwrap().join("tests/fixtures/parser");
        let fullpath = &fixtures.join(path);
        let source = std::fs::read_to_string(&fullpath).expect("Can't open file");
        let start = std::time::Instant::now();
        let ast = block(&source);
        let elapsed = start.elapsed();
        println!("Time: {:?} - {}", elapsed, path);
        assert!(ast.is_ok());
        assert_eq!(ast.unwrap().0.trim(), "", "Expected ''. File: {}", path);
    }
    assert_parses("basic.js");
    assert_parses("require.js");
    assert_parses("exports.js");
    assert_parses("crazy-indent.js");
    assert_parses("peter.js");
    assert_parses("itt_prelude.js");
    assert_parses("matrix.js");
    assert_parses("math.js");
}
