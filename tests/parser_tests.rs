use paq::parser::{block, expression, get_deps, transform, Node};

#[test]
fn text_fixtures() {
    fn assert_parses(path: &str) {
        let cur_dir = std::env::current_dir().unwrap();
        let fixtures = cur_dir.join("tests/fixtures/parser");
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

#[test]
fn test_parser_deps() {
    fn assert_dep(path: &str, substring: &str) {
        let cur_dir = std::env::current_dir().unwrap();
        let entry = cur_dir.join("tests/fixtures/parser_deps").join(path);
        let source = std::fs::read_to_string(&entry).unwrap();
        let ast = block(&source);
        let result = get_deps(ast.unwrap().1);
        assert!(result.contains(&String::from(substring)))
    }
    assert_dep("deps-modules.js", "peter");
    assert_dep("deps-crazy-indent.js", "./math.js");
    assert_dep("deps-double-quotes.js", "./math");
    assert_dep("deps-modules-2.js", "itt");
    assert_dep("deps-relative.js", "./math.js");
    assert_dep("deps-multiple.js", "math");
    assert_dep("deps-multiple.js", "./index.js");
    assert_dep("deps-multiple.js", "fs");
    assert_dep("deps-comments.js", "fs");
    assert_dep("deps-if.js", "lodash");
    assert_dep("deps-else.js", "lodash");
    assert_dep("deps-for.js", "lodash");
    assert_dep("deps-for-in.js", "lodash");
    assert_dep("deps-for-of.js", "lodash");
    assert_dep("deps-while.js", "lodash");
    assert_dep("deps-walker-all.js", "lodash");
    assert_dep("deps-walker-all.js", "underscore");
    assert_dep("deps-walker-all.js", "debounce");
    assert_dep("deps-walker-all.js", "assert");
    assert_dep("deps-lazy.js", "lazily-loaded");
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
fn test_comments() {
    assert_eq!(
        block("  // hello\n asdf"),
        Ok(("", Node::Block(vec![Node::Ident(String::from("asdf"))])))
    );
}

#[test]
fn test_transform() {
    assert_eq!(
        transform(block("import 'y';").unwrap().1),
        Node::Block(vec![Node::Binary(
            "(",
            Box::new(Node::Ident(String::from("require"))),
            Box::new(Node::Args(vec![Node::Str(String::from("y"))]))
        )])
    );
}

#[test]
fn test_statement() {
    assert_eq!(
        block("import 'y';"),
        Ok((
            ";",
            Node::Block(vec![Node::Import((
                None,
                Box::new(Node::Str(String::from("y")))
            ))])
        ))
    );
    assert_eq!(
        block("import x from 'y';"),
        Ok((
            ";",
            Node::Block(vec![Node::Import((
                Some(Box::new(Node::Ident(String::from("x")))),
                Box::new(Node::Str(String::from("y")))
            ))])
        ))
    );
    assert_eq!(
        block("continue}"),
        Ok(("}", Node::Block(vec![Node::Continue])))
    );
    assert_eq!(
        block("{continue}"),
        Ok(("", Node::Block(vec![Node::Block(vec![Node::Continue])])))
    );
    assert_eq!(
        block("continue;continue;"),
        Ok(("", Node::Block(vec![Node::Continue, Node::Continue])))
    );
    assert_eq!(
        block(" continue ; continue ; "),
        Ok((" ", Node::Block(vec![Node::Continue, Node::Continue])))
    );
    assert_eq!(
        block("continue"),
        Ok(("", Node::Block(vec![Node::Continue])))
    );
    assert_eq!(
        block("continue\n1"),
        Ok(("", Node::Block(vec![Node::Continue, Node::Double(1.0)])))
    );
    assert_eq!(
        block("continue; 1"),
        Ok(("", Node::Block(vec![Node::Continue, Node::Double(1.0)])))
    );
    assert_eq!(block("break;"), Ok(("", Node::Block(vec![Node::Break]))));
    assert_eq!(
        block(" break ; break ; "),
        Ok((" ", Node::Block(vec![Node::Break, Node::Break])))
    );
    assert_eq!(block("break\n"), Ok(("", Node::Block(vec![Node::Break]))));
    assert_eq!(
        block("return 1;"),
        Ok((
            "",
            Node::Block(vec![Node::Return(Some(Box::new(Node::Double(1.0))))])
        ))
    );
    assert_eq!(
        block("throw 1;"),
        Ok((
            "",
            Node::Block(vec![Node::Throw(Box::new(Node::Double(1.0)))])
        ))
    );
    assert_eq!(
        block("return 1\n"),
        Ok((
            "",
            Node::Block(vec![Node::Return(Some(Box::new(Node::Double(1.0))))])
        ))
    );
    assert_eq!(
        block("return\n1\n"),
        Ok((
            "",
            Node::Block(vec![Node::Return(Some(Box::new(Node::Double(1.0))))])
        ))
    );
    assert_eq!(
        block("return; 1\n"),
        Ok(("", Node::Block(vec![Node::Return(None), Node::Double(1.0)])))
    );
    assert_eq!(
        block(" return 1 ; return 1 ; "),
        Ok((
            " ",
            Node::Block(vec![
                Node::Return(Some(Box::new(Node::Double(1.0)))),
                Node::Return(Some(Box::new(Node::Double(1.0))))
            ])
        ))
    );
    assert_eq!(
        block("return;"),
        Ok(("", Node::Block(vec![Node::Return(None)])))
    );
    assert_eq!(
        block("return"),
        Ok(("", Node::Block(vec![Node::Return(None)])))
    );
    assert_eq!(
        block("return\n"),
        Ok(("", Node::Block(vec![Node::Return(None)])))
    );
    assert_eq!(
        block("a = 2;"),
        Ok((
            "",
            Node::Block(vec![Node::Binary(
                "=",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Double(2.0))
            )])
        ))
    );
    assert_eq!(
        block("var a = 2;"),
        Ok((
            "",
            Node::Block(vec![Node::Declaration((
                "var",
                vec![Node::Binary(
                    "=",
                    Box::new(Node::Ident(String::from("a"))),
                    Box::new(Node::Double(2.0))
                )]
            ))])
        ))
    );
    assert_eq!(
        block("let a = 2, b = 3"),
        Ok((
            "",
            Node::Block(vec![Node::Declaration((
                "let",
                vec![
                    Node::Binary(
                        "=",
                        Box::new(Node::Ident(String::from("a"))),
                        Box::new(Node::Double(2.0))
                    ),
                    Node::Binary(
                        "=",
                        Box::new(Node::Ident(String::from("b"))),
                        Box::new(Node::Double(3.0))
                    ),
                ]
            ))])
        ))
    );
    assert_eq!(
        block("let a=2,b=3"),
        Ok((
            "",
            Node::Block(vec![Node::Declaration((
                "let",
                vec![
                    Node::Binary(
                        "=",
                        Box::new(Node::Ident(String::from("a"))),
                        Box::new(Node::Double(2.0))
                    ),
                    Node::Binary(
                        "=",
                        Box::new(Node::Ident(String::from("b"))),
                        Box::new(Node::Double(3.0))
                    ),
                ]
            ))])
        ))
    );
    assert_eq!(
        block("const x = [];"),
        Ok((
            "",
            Node::Block(vec![Node::Declaration((
                "const",
                vec![Node::Binary(
                    "=",
                    Box::new(Node::Ident(String::from("x"))),
                    Box::new(Node::List(vec![]))
                )]
            ))])
        ))
    );
    assert_eq!(
        block("a = 2"),
        Ok((
            "",
            Node::Block(vec![Node::Binary(
                "=",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Double(2.0))
            )])
        ))
    );
    assert_eq!(
        block("a = G"),
        Ok((
            "",
            Node::Block(vec![Node::Binary(
                "=",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Ident(String::from("G")))
            )])
        ))
    );
    assert_eq!(
        block("abc = G"),
        Ok((
            "",
            Node::Block(vec![Node::Binary(
                "=",
                Box::new(Node::Ident(String::from("abc"))),
                Box::new(Node::Ident(String::from("G")))
            )])
        ))
    );
    assert_eq!(
        block("const empty = G()"),
        Ok((
            "",
            Node::Block(vec![Node::Declaration((
                "const",
                vec![Node::Binary(
                    "=",
                    Box::new(Node::Ident(String::from("empty"))),
                    Box::new(Node::Binary(
                        "(",
                        Box::new(Node::Ident(String::from("G"))),
                        Box::new(Node::Args(vec![]))
                    ))
                )]
            ))])
        ))
    );
    assert_eq!(
        block("z"),
        Ok(("", Node::Block(vec![Node::Ident(String::from("z"))])))
    );
    assert_eq!(
        block("if(true){return;}"),
        Ok((
            "",
            Node::Block(vec![Node::If((
                Box::new(Node::Paren(Box::new(Node::Ident(String::from("true"))))),
                Box::new(Node::Block(vec![Node::Return(None)])),
                None
            ))])
        ))
    );
    assert_eq!(
        block("if(true)return;"),
        Ok((
            "",
            Node::Block(vec![Node::If((
                Box::new(Node::Paren(Box::new(Node::Ident(String::from("true"))))),
                Box::new(Node::Return(None)),
                None
            ))])
        ))
    );
    assert_eq!(
        block("if(true)return;else break;"),
        Ok((
            "",
            Node::Block(vec![Node::If((
                Box::new(Node::Paren(Box::new(Node::Ident(String::from("true"))))),
                Box::new(Node::Return(None)),
                Some(Box::new(Node::Break))
            ))])
        ))
    );
    assert_eq!(
        block("if ( true )\n {\n return; \n}\n else \n { break; }"),
        Ok((
            "",
            Node::Block(vec![Node::If((
                Box::new(Node::Paren(Box::new(Node::Ident(String::from("true"))))),
                Box::new(Node::Block(vec![Node::Return(None)])),
                Some(Box::new(Node::Block(vec![Node::Break])))
            ))])
        ))
    );
    assert_eq!(
        block("while(true){return;}"),
        Ok((
            "",
            Node::Block(vec![Node::While((
                Box::new(Node::Paren(Box::new(Node::Ident(String::from("true"))))),
                Box::new(Node::Block(vec![Node::Return(None)]))
            ))])
        ))
    );
    assert_eq!(
        block(" while ( true ) return ; "),
        Ok((
            " ",
            Node::Block(vec![Node::While((
                Box::new(Node::Paren(Box::new(Node::Ident(String::from("true"))))),
                Box::new(Node::Return(None))
            ))])
        ))
    );
    assert_eq!(
        block("for(0;0;0){return;}"),
        Ok((
            "",
            Node::Block(vec![Node::For((
                Box::new(Node::ForTrio(vec![
                    Some(Node::Double(0.0)),
                    Some(Node::Double(0.0)),
                    Some(Node::Double(0.0)),
                ])),
                Box::new(Node::Block(vec![Node::Return(None)]))
            ))])
        ))
    );
    assert_eq!(
        block("for ( 0 ; 0 ; 0 ) return ; "),
        Ok((
            " ",
            Node::Block(vec![Node::For((
                Box::new(Node::ForTrio(vec![
                    Some(Node::Double(0.0)),
                    Some(Node::Double(0.0)),
                    Some(Node::Double(0.0))
                ])),
                Box::new(Node::Return(None))
            ))])
        ))
    );
    assert_eq!(
        block("for ( ; ; ) return ; "),
        Ok((
            " ",
            Node::Block(vec![Node::For((
                Box::new(Node::ForTrio(vec![None, None, None])),
                Box::new(Node::Return(None))
            ))])
        ))
    );
    assert_eq!(
        block("for (let x = 1 ; x < 1 ; x++ ) return ; "),
        Ok((
            " ",
            Node::Block(vec![Node::For((
                Box::new(Node::ForTrio(vec![
                    Some(Node::Declaration((
                        "let",
                        vec![Node::Binary(
                            "=",
                            Box::new(Node::Ident(String::from("x"))),
                            Box::new(Node::Double(1.0))
                        )]
                    ))),
                    Some(Node::Binary(
                        "<",
                        Box::new(Node::Ident(String::from("x"))),
                        Box::new(Node::Double(1.0))
                    )),
                    Some(Node::Unary("++", Box::new(Node::Ident(String::from("x")))))
                ])),
                Box::new(Node::Return(None))
            ))])
        ))
    );
    assert_eq!(
        block("for (let x in y) return ; "),
        Ok((
            " ",
            Node::Block(vec![Node::For((
                Box::new(Node::ForIn((
                    Box::new(Node::Variable((
                        "let",
                        Box::new(Node::Ident(String::from("x")))
                    ))),
                    Box::new(Node::Ident(String::from("y")))
                ))),
                Box::new(Node::Return(None))
            ))])
        ))
    );
    assert_eq!(
        block("for (let x of y) return ; "),
        Ok((
            " ",
            Node::Block(vec![Node::For((
                Box::new(Node::ForOf((
                    Box::new(Node::Variable((
                        "let",
                        Box::new(Node::Ident(String::from("x")))
                    ))),
                    Box::new(Node::Ident(String::from("y")))
                ))),
                Box::new(Node::Return(None))
            ))])
        ))
    );
}

#[test]
fn test_string() {
    assert_eq!(expression("\"\""), Ok(("", Node::Str(String::from("")))));
    assert_eq!(expression(" \"\" "), Ok((" ", Node::Str(String::from("")))));
    assert_eq!(
        expression(" \"a\" "),
        Ok((" ", Node::Str(String::from("a"))))
    );
    assert_eq!(
        expression(" \"Example\" "),
        Ok((" ", Node::Str(String::from("Example"))))
    );
    assert_eq!(
        expression("\"\\     a\""),
        Ok(("", Node::Str(String::from("a"))))
    );
    assert_eq!(
        expression("\"\\   b  a\""),
        Ok(("", Node::Str(String::from("b  a"))))
    );
    assert_eq!(
        expression("\"\\n\\n\""),
        Ok(("", Node::Str(String::from("\n\n"))))
    );
    assert_eq!(
        expression("\"✅\""),
        Ok(("", Node::Str(String::from("✅"))))
    );
    assert_eq!(
        expression("\"\\n\""),
        Ok(("", Node::Str(String::from("\n"))))
    );
    assert_eq!(
        expression("\"\\r\""),
        Ok(("", Node::Str(String::from("\r"))))
    );
    assert_eq!(
        expression("\"\\t\""),
        Ok(("", Node::Str(String::from("\t"))))
    );
    assert_eq!(
        expression("\"\\b\""),
        Ok(("", Node::Str(String::from("\u{08}"))))
    );
    assert_eq!(
        expression("\"\\v\""),
        Ok(("", Node::Str(String::from("\u{0B}"))))
    );
    assert_eq!(
        expression("\"\\f\""),
        Ok(("", Node::Str(String::from("\u{0C}"))))
    );
    assert_eq!(
        expression("\"\\\\\""),
        Ok(("", Node::Str(String::from("\\"))))
    );
    assert_eq!(
        expression("\"\\/\""),
        Ok(("", Node::Str(String::from("/"))))
    );
    assert_eq!(
        expression("\"\\\"\""),
        Ok(("", Node::Str(String::from("\""))))
    );
    assert_eq!(
        expression("\"\\\'\""),
        Ok(("", Node::Str(String::from("'"))))
    );
}

#[test]
fn test_single_quoted_string() {
    assert_eq!(expression("''"), Ok(("", Node::Str(String::from("")))));
    assert_eq!(expression(" '' "), Ok((" ", Node::Str(String::from("")))));
    assert_eq!(expression(" 'a' "), Ok((" ", Node::Str(String::from("a")))));
    assert_eq!(
        expression(" 'Example' "),
        Ok((" ", Node::Str(String::from("Example"))))
    );
    assert_eq!(
        expression("'\\     a'"),
        Ok(("", Node::Str(String::from("a"))))
    );
    assert_eq!(
        expression("'\\   b  a'"),
        Ok(("", Node::Str(String::from("b  a"))))
    );
    assert_eq!(
        expression("'\\n\\n'"),
        Ok(("", Node::Str(String::from("\n\n"))))
    );
    assert_eq!(
        expression("\"✅\""),
        Ok(("", Node::Str(String::from("✅"))))
    );
    assert_eq!(expression("'\\n'"), Ok(("", Node::Str(String::from("\n")))));
    assert_eq!(expression("'\\r'"), Ok(("", Node::Str(String::from("\r")))));
    assert_eq!(expression("'\\t'"), Ok(("", Node::Str(String::from("\t")))));
    assert_eq!(
        expression("'\\b'"),
        Ok(("", Node::Str(String::from("\u{08}"))))
    );
    assert_eq!(
        expression("'\\v'"),
        Ok(("", Node::Str(String::from("\u{0B}"))))
    );
    assert_eq!(
        expression("'\\f'"),
        Ok(("", Node::Str(String::from("\u{0C}"))))
    );
    assert_eq!(
        expression("'\\\\'"),
        Ok(("", Node::Str(String::from("\\"))))
    );
    assert_eq!(expression("'\\/'"), Ok(("", Node::Str(String::from("/")))));
    assert_eq!(
        expression("'\\\"'"),
        Ok(("", Node::Str(String::from("\""))))
    );
    assert_eq!(expression("'\\''"), Ok(("", Node::Str(String::from("'")))));
}

#[test]
fn test_double() {
    assert_eq!(expression("0"), Ok(("", Node::Double(0.0))));
    assert_eq!(expression("1"), Ok(("", Node::Double(1.0))));
    assert_eq!(expression("2.2"), Ok(("", Node::Double(2.2))));
    assert_eq!(expression("3."), Ok(("", Node::Double(3.0))));
    assert_eq!(expression(".4"), Ok(("", Node::Double(0.4))));
    assert_eq!(expression("1e2"), Ok(("", Node::Double(100.0))));
    assert_eq!(expression("1e-2"), Ok(("", Node::Double(0.01))));
    assert_eq!(expression("0"), Ok(("", Node::Double(0.0))));
    assert_eq!(expression("123456789"), Ok(("", Node::Double(123456789.0))));
    assert_eq!(expression("0."), Ok(("", Node::Double(0.0))));
    assert_eq!(expression("123."), Ok(("", Node::Double(123.0))));
    assert_eq!(expression(".012300"), Ok(("", Node::Double(0.0123))));
    assert_eq!(expression("0.012300"), Ok(("", Node::Double(0.0123))));
    assert_eq!(expression("123.045600"), Ok(("", Node::Double(123.0456))));
    assert_eq!(expression(".123e0"), Ok(("", Node::Double(0.123))));
    assert_eq!(expression("0.123e0"), Ok(("", Node::Double(0.123))));
    assert_eq!(expression("123.456e0"), Ok(("", Node::Double(123.456))));
    assert_eq!(expression(".123e01"), Ok(("", Node::Double(1.23))));
    assert_eq!(expression("0.123e01"), Ok(("", Node::Double(1.23))));
    assert_eq!(expression("123.456e02"), Ok(("", Node::Double(12345.6))));
    assert_eq!(expression(".123e+4"), Ok(("", Node::Double(1230.0))));
    assert_eq!(expression("0.123e+4"), Ok(("", Node::Double(1230.0))));
    assert_eq!(expression("123.456e+4"), Ok(("", Node::Double(1234560.0))));
    assert_eq!(expression(".123e-4"), Ok(("", Node::Double(0.0000123))));
    assert_eq!(expression("0.123e-4"), Ok(("", Node::Double(0.0000123))));
    assert_eq!(expression("123.456e-4"), Ok(("", Node::Double(0.0123456))));
    assert_eq!(expression("0e0"), Ok(("", Node::Double(0.0))));
    assert_eq!(expression("123e0"), Ok(("", Node::Double(123.0))));
    assert_eq!(expression("0e01"), Ok(("", Node::Double(0.0))));
    assert_eq!(expression("123e02"), Ok(("", Node::Double(12300.0))));
    assert_eq!(expression("0e+4"), Ok(("", Node::Double(0.0))));
    assert_eq!(expression("123e+4"), Ok(("", Node::Double(1230000.0))));
    assert_eq!(expression("0e-4"), Ok(("", Node::Double(0.0))));
    assert_eq!(expression("123e-4"), Ok(("", Node::Double(0.0123))));
}

#[test]
fn test_octal() {
    assert_eq!(expression("0o123"), Ok(("", Node::Octal(0o123))));
    assert_eq!(expression("0o111"), Ok(("", Node::Octal(0o111))));
    assert_eq!(expression("0o0"), Ok(("", Node::Octal(0o0))));
    assert_eq!(expression("0o03 "), Ok((" ", Node::Octal(0o3))));
    assert_eq!(expression("0o012 "), Ok((" ", Node::Octal(0o12))));
    assert_eq!(expression("0o07654321 "), Ok((" ", Node::Octal(0o7654321))));
}

#[test]
fn test_hexadecimal() {
    assert_eq!(expression("0x3 "), Ok((" ", Node::Hexadecimal(0x3))));
    assert_eq!(
        expression("0x0123789"),
        Ok(("", Node::Hexadecimal(0x0123789)))
    );
    assert_eq!(
        expression("0xABCDEF"),
        Ok(("", Node::Hexadecimal(0xabcdef)))
    );
    assert_eq!(
        expression("0xabcdef"),
        Ok(("", Node::Hexadecimal(0xabcdef)))
    );
}

#[test]
fn test_binarynum() {
    assert_eq!(expression("0b0"), Ok(("", Node::BinaryNum(0b0))));
    assert_eq!(expression("0b1"), Ok(("", Node::BinaryNum(0b1))));
    assert_eq!(expression("0b01010"), Ok(("", Node::BinaryNum(0b01010))));
    assert_eq!(
        expression("0b1010111"),
        Ok(("", Node::BinaryNum(0b1010111)))
    );
}

#[test]
fn test_identifier() {
    assert_eq!(
        expression("hello"),
        Ok(("", Node::Ident(String::from("hello"))))
    );
    assert_eq!(expression("e"), Ok(("", Node::Ident(String::from("e")))));
}

#[test]
fn test_list() {
    assert_eq!(expression(" [ ] "), Ok((" ", Node::List(vec![]))));
    assert_eq!(
        expression("[[]]"),
        Ok(("", Node::List(vec![Node::List(vec![])])))
    );
    assert_eq!(expression("[]"), Ok(("", Node::List(vec![]))));
    assert_eq!(
        expression("[ 1 ]"),
        Ok(("", Node::List(vec![Node::Double(1.0)])))
    );
    assert_eq!(
        expression("[ 1, 2 ]"),
        Ok(("", Node::List(vec![Node::Double(1.0), Node::Double(2.0)])))
    );
    assert_eq!(
        expression("[ 1, \"2\" ]"),
        Ok((
            "",
            Node::List(vec![Node::Double(1.0), Node::Str(String::from("2"))])
        ))
    );
    assert_eq!(
        expression("[ ...a, 1 ]"),
        Ok((
            "",
            Node::List(vec![
                Node::Splat(Box::new(Node::Ident(String::from("a")))),
                Node::Double(1.0)
            ])
        ))
    );
    assert_eq!(
        expression("[ ...[], 1 ]"),
        Ok((
            "",
            Node::List(vec![
                Node::Splat(Box::new(Node::List(vec![]))),
                Node::Double(1.0)
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
            Node::Object(vec![Node::KeyValue((
                Box::new(Node::Str(String::from("a"))),
                Box::new(Node::Double(1.0))
            ))])
        ))
    );
    assert_eq!(
        expression("{x: 1, y: 2}"),
        Ok((
            "",
            Node::Object(vec![
                Node::KeyValue((
                    Box::new(Node::Ident(String::from("x"))),
                    Box::new(Node::Double(1.0))
                )),
                Node::KeyValue((
                    Box::new(Node::Ident(String::from("y"))),
                    Box::new(Node::Double(2.0))
                )),
            ])
        ))
    );
    assert_eq!(
        expression("{[1]: 1, [\"a\"]: 2}"),
        Ok((
            "",
            Node::Object(vec![
                Node::KeyValue((Box::new(Node::Double(1.0)), Box::new(Node::Double(1.0)))),
                Node::KeyValue((
                    Box::new(Node::Str(String::from("a"))),
                    Box::new(Node::Double(2.0))
                )),
            ])
        ))
    );
    assert_eq!(
        expression("{a: {}}"),
        Ok((
            "",
            Node::Object(vec![Node::KeyValue((
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Object(vec![]))
            ))])
        ))
    );
    assert_eq!(
        expression("{ ...a }"),
        Ok((
            "",
            Node::Object(vec![Node::Splat(Box::new(Node::Ident(String::from("a"))))])
        ))
    );
    assert_eq!(
        expression("{ ...[] }"),
        Ok((
            "",
            Node::Object(vec![Node::Splat(Box::new(Node::List(vec![])))])
        ))
    );
    assert_eq!(
        expression("{ a, b }"),
        Ok((
            "",
            Node::Object(vec![
                Node::Ident(String::from("a")),
                Node::Ident(String::from("b"))
            ])
        ))
    );
}

#[test]
fn test_parenthesis() {
    assert_eq!(
        expression("(1)"),
        Ok(("", Node::Paren(Box::new(Node::Double(1.0)))))
    );
    assert_eq!(
        expression("([])"),
        Ok(("", Node::Paren(Box::new(Node::List(vec![])))))
    );
    assert_eq!(
        expression(" ( 1 ) "),
        Ok((" ", Node::Paren(Box::new(Node::Double(1.0)))))
    );
    assert_eq!(
        expression(" ( [ ] ) "),
        Ok((" ", Node::Paren(Box::new(Node::List(vec![])))))
    );
}

#[test]
fn test_closure() {
    assert_eq!(
        expression("(a, b) => 1 + 1"),
        Ok((
            "",
            Node::Closure((
                Box::new(Node::Params(vec![
                    Node::Param((Box::new(Node::Ident(String::from("a"))), None)),
                    Node::Param((Box::new(Node::Ident(String::from("b"))), None))
                ])),
                Box::new(Node::Binary(
                    "+",
                    Box::new(Node::Double(1.0)),
                    Box::new(Node::Double(1.0))
                ))
            ))
        ))
    );
    assert_eq!(
        expression("(a) => ({})"),
        Ok((
            "",
            Node::Closure((
                Box::new(Node::Params(vec![Node::Param((
                    Box::new(Node::Ident(String::from("a"))),
                    None
                ))])),
                Box::new(Node::Paren(Box::new(Node::Object(vec![]))))
            ))
        ))
    );
}

#[test]
fn test_function() {
    assert_eq!(
        expression("function(){}"),
        Ok((
            "",
            Node::Function((
                None,
                Box::new(Node::Params(vec![])),
                Box::new(Node::Block(vec![]))
            ))
        ))
    );
    assert_eq!(
        expression("function f(x, y){ return x; }"),
        Ok((
            "",
            Node::Function((
                Some(Box::new(Node::Ident(String::from("f")))),
                Box::new(Node::Params(vec![
                    Node::Param((Box::new(Node::Ident(String::from("x"))), None)),
                    Node::Param((Box::new(Node::Ident(String::from("y"))), None))
                ])),
                Box::new(Node::Block(vec![Node::Return(Some(Box::new(
                    Node::Ident(String::from("x"))
                )))]))
            ))
        ))
    );
    assert_eq!(
        expression("function f ( x, y) { return x }"),
        Ok((
            "",
            Node::Function((
                Some(Box::new(Node::Ident(String::from("f")))),
                Box::new(Node::Params(vec![
                    Node::Param((Box::new(Node::Ident(String::from("x"))), None)),
                    Node::Param((Box::new(Node::Ident(String::from("y"))), None))
                ])),
                Box::new(Node::Block(vec![Node::Return(Some(Box::new(
                    Node::Ident(String::from("x"))
                )))]))
            ))
        ))
    );
}

#[test]
fn test_generator() {
    assert_eq!(
        expression("function*() {}"),
        Ok((
            "",
            Node::Generator((
                None,
                Box::new(Node::Params(vec![])),
                Box::new(Node::Block(vec![]))
            ))
        ))
    );
}

#[test]
fn test_mutation() {
    assert_eq!(
        expression(" 1 = 2 "),
        Ok((
            " ",
            Node::Binary(
                "=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 += 2 "),
        Ok((
            " ",
            Node::Binary(
                "+=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 -= 2 "),
        Ok((
            " ",
            Node::Binary(
                "-=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 %= 2 "),
        Ok((
            " ",
            Node::Binary(
                "%=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 *= 2 "),
        Ok((
            " ",
            Node::Binary(
                "*=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 /= 2 "),
        Ok((
            " ",
            Node::Binary(
                "/=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 <<= 2 "),
        Ok((
            " ",
            Node::Binary(
                "<<=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >>>= 2 "),
        Ok((
            " ",
            Node::Binary(
                ">>>=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >>= 2 "),
        Ok((
            " ",
            Node::Binary(
                ">>=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 &= 2 "),
        Ok((
            " ",
            Node::Binary(
                "&=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ^= 2 "),
        Ok((
            " ",
            Node::Binary(
                "^=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 |= 2 "),
        Ok((
            " ",
            Node::Binary(
                "|=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 = 2 = 3 "),
        Ok((
            " ",
            Node::Binary(
                "=",
                Box::new(Node::Binary(
                    "=",
                    Box::new(Node::Double(1.0)),
                    Box::new(Node::Double(2.0))
                )),
                Box::new(Node::Double(3.0))
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
            Node::Ternary(
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0)),
                Box::new(Node::Double(3.0))
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
            Node::Binary(
                "==",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 != 2 "),
        Ok((
            " ",
            Node::Binary(
                "!=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 > 2 "),
        Ok((
            " ",
            Node::Binary(
                ">",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 < 2 "),
        Ok((
            " ",
            Node::Binary(
                "<",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >= 2 "),
        Ok((
            " ",
            Node::Binary(
                ">=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 <= 2 "),
        Ok((
            " ",
            Node::Binary(
                "<=",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 === 2 "),
        Ok((
            " ",
            Node::Binary(
                "===",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 !== 2 "),
        Ok((
            " ",
            Node::Binary(
                "!==",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 instanceof 2 "),
        Ok((
            " ",
            Node::Binary(
                "instanceof",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 in 2 "),
        Ok((
            " ",
            Node::Binary(
                "in",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 == 2 == 3 "),
        Ok((
            " ",
            Node::Binary(
                "==",
                Box::new(Node::Binary(
                    "==",
                    Box::new(Node::Double(1.0)),
                    Box::new(Node::Double(2.0))
                )),
                Box::new(Node::Double(3.0))
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
            Node::Binary(
                "||",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 && 2 "),
        Ok((
            " ",
            Node::Binary(
                "&&",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ?? 2 "),
        Ok((
            " ",
            Node::Binary(
                "??",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
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
            Node::Binary(
                "|",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ^ 2 "),
        Ok((
            " ",
            Node::Binary(
                "^",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 & 2 "),
        Ok((
            " ",
            Node::Binary(
                "&",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );

    assert_eq!(
        expression(" 1 >> 2 "),
        Ok((
            " ",
            Node::Binary(
                ">>",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 >>> 2 "),
        Ok((
            " ",
            Node::Binary(
                ">>>",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 << 2 "),
        Ok((
            " ",
            Node::Binary(
                "<<",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
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
            Node::Binary(
                "+",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 - 2 "),
        Ok((
            " ",
            Node::Binary(
                "-",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 * 2 "),
        Ok((
            " ",
            Node::Binary(
                "*",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 / 2 "),
        Ok((
            " ",
            Node::Binary(
                "/",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 % 2 "),
        Ok((
            " ",
            Node::Binary(
                "%",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
    assert_eq!(
        expression(" 1 ** 2 "),
        Ok((
            " ",
            Node::Binary(
                "**",
                Box::new(Node::Double(1.0)),
                Box::new(Node::Double(2.0))
            )
        ))
    );
}

#[test]
fn test_prefix() {
    assert_eq!(
        expression(" ++ 2 "),
        Ok((" ", Node::Unary("++", Box::new(Node::Double(2.0)))))
    );
    assert_eq!(
        expression(" -- 2 "),
        Ok((" ", Node::Unary("--", Box::new(Node::Double(2.0)))))
    );
    assert_eq!(
        expression(" + 2 "),
        Ok((" ", Node::Unary("+", Box::new(Node::Double(2.0)))))
    );
    assert_eq!(
        expression(" - 2 "),
        Ok((" ", Node::Unary("-", Box::new(Node::Double(2.0)))))
    );
    assert_eq!(
        expression(" ! 2 "),
        Ok((" ", Node::Unary("!", Box::new(Node::Double(2.0)))))
    );
    assert_eq!(
        expression(" !!2 "),
        Ok((
            " ",
            Node::Unary("!", Box::new(Node::Unary("!", Box::new(Node::Double(2.0)))))
        ))
    );
    assert_eq!(
        expression(" ! ! 2 "),
        Ok((
            " ",
            Node::Unary("!", Box::new(Node::Unary("!", Box::new(Node::Double(2.0)))))
        ))
    );

    assert_eq!(
        expression("typeof a"),
        Ok((
            "",
            Node::Unary("typeof", Box::new(Node::Ident(String::from("a"))))
        ))
    );
    assert_eq!(
        expression("void a"),
        Ok((
            "",
            Node::Unary("void", Box::new(Node::Ident(String::from("a"))))
        ))
    );
    assert_eq!(
        expression("delete a"),
        Ok((
            "",
            Node::Unary("delete", Box::new(Node::Ident(String::from("a"))))
        ))
    );
    assert_eq!(
        expression("await a"),
        Ok((
            "",
            Node::Unary("await", Box::new(Node::Ident(String::from("a"))))
        ))
    );
    assert_eq!(
        expression("yield a"),
        Ok((
            "",
            Node::Unary("yield", Box::new(Node::Ident(String::from("a"))))
        ))
    );
    assert_eq!(
        expression("yield* a"),
        Ok((
            "",
            Node::Unary("yield*", Box::new(Node::Ident(String::from("a"))))
        ))
    );
    assert_eq!(
        expression("new a"),
        Ok((
            "",
            Node::Unary("new", Box::new(Node::Ident(String::from("a"))))
        ))
    );
}

#[test]
fn test_postfix() {
    assert_eq!(
        expression(" a++"),
        Ok((
            "",
            Node::Unary("++", Box::new(Node::Ident(String::from("a"))))
        ))
    );
    assert_eq!(
        expression(" a--"),
        Ok((
            "",
            Node::Unary("--", Box::new(Node::Ident(String::from("a"))))
        ))
    );
}

#[test]
fn test_action() {
    assert_eq!(
        expression(" a?.a"),
        Ok((
            "",
            Node::Binary(
                "?.",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a?.(a)"),
        Ok((
            "",
            Node::Binary(
                "?.(",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Args(vec![Node::Ident(String::from("a"))]))
            )
        ))
    );
    assert_eq!(
        expression(" a?.[a]"),
        Ok((
            "",
            Node::Binary(
                "?.[",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a[a]"),
        Ok((
            "",
            Node::Binary(
                "[",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a [ a ]"),
        Ok((
            "",
            Node::Binary(
                "[",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a(a)"),
        Ok((
            "",
            Node::Binary(
                "(",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Args(vec![(Node::Ident(String::from("a")))]))
            )
        ))
    );
    assert_eq!(
        expression(" a ( a )"),
        Ok((
            "",
            Node::Binary(
                "(",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Args(vec![(Node::Ident(String::from("a")))]))
            )
        ))
    );
    assert_eq!(
        expression(" a.a"),
        Ok((
            "",
            Node::Binary(
                ".",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Ident(String::from("a")))
            )
        ))
    );
    assert_eq!(
        expression(" a()"),
        Ok((
            "",
            Node::Binary(
                "(",
                Box::new(Node::Ident(String::from("a"))),
                Box::new(Node::Args(vec![]))
            )
        ))
    );
}
