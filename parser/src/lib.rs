use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::bytes::streaming::{is_not};
use nom::character::complete::{alphanumeric1, char, hex_digit1, oct_digit1};
use nom::character::streaming::{multispace1};
use nom::combinator::{cut, map, map_res, opt, value, verify};
use nom::error::{context, VerboseError};
use nom::multi::{many0, many1, separated_list};
use nom::number::complete::double;
use nom::sequence::{pair, preceded, separated_pair, delimited, terminated};
use nom::IResult;

type Result<'a, T> = IResult<&'a str, T, VerboseError<&'a str>>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Str(String),
    Ident(String),
    Splat(Box<Expression>),
    Double(f64),
    Octal(u64),
    Hexadecimal(u64),
    BinaryNum(u64),
    List(Vec<Expression>),
    Object(Vec<(Expression, Expression)>),
    Paren(Box<Expression>),
    Pair(Operator, Box<Expression>, Box<Expression>),
    Prefix(Operator, Box<Expression>),
    Postfix(Box<Expression>, Operator),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    StrictEqual,
    StrictNotEqual,
    LogicalAnd,
    LogicalOr,
    Add,
    Sub,
    Mult,
    Div,
    Not,
    Incr,
    Decr,
    Array,
    Application,
    Dot,
    Assign,
    AssignAdd,
    AssignSub,
    AssignMod,
    AssignMul,
    AssignDiv,
}

pub fn expression(i: &str) -> Result<Expression> {
    preceded(ws, alt((
        mutation,
        primitive,
    )))(i)
}

pub fn primitive(i: &str) -> Result<Expression> {
    preceded(ws, alt((
        map(string, Expression::Str),
        map(string2, Expression::Str),
        map(octal, Expression::Octal),
        map(hexadecimal, Expression::Hexadecimal),
        map(binary, Expression::BinaryNum),
        map(double, Expression::Double),
        map(ident, Expression::Ident),
        map(object, Expression::Object),
        map(paren, Expression::Paren),
        map(list, Expression::List),
    )))(i)
}

fn string(i: &str) -> Result<String> {
    let chars = verify(is_not("\"\\"), |s: &str| !s.is_empty());
    let inner = map(many0(alt((chars, escaped_char))), |s| s.join(""));
    context("string", delimited(char('"'), inner, char('"')))(i)
}

fn string2(i: &str) -> Result<String> {
    let chars = verify(is_not("\'\\"), |s: &str| !s.is_empty());
    let inner = map(many0(alt((chars, escaped_char))), |s| s.join(""));
    context("string", delimited(char('\''), inner, char('\'')))(i)
}

fn octal(i: &str) -> Result<u64> {
    let inner = map_res(oct_digit1, |s| u64::from_str_radix(s, 8));
    context("octal", preceded(tag("0o"), cut(inner)))(i)
}

fn hexadecimal(i: &str) -> Result<u64> {
    let inner = map_res(hex_digit1, |s| u64::from_str_radix(s, 16));
    context("hexadecimal", preceded(tag("0x"), cut(inner)))(i)
}

fn binary(i: &str) -> Result<u64> {
    let inner = map_res(alphanumeric1, |s| u64::from_str_radix(s, 2));
    context("binary", preceded(tag("0b"), cut(inner)))(i)
}

fn ident(i: &str) -> Result<String> {
    context("ident", map(many1(alphanumeric1), |s| s.join("")))(i)
}

fn object(i: &str) -> Result<Vec<(Expression, Expression)>> {
    let inner = separated_list(preceded(ws, char(',')), key_value);
    context("object", delimited(char('{'), delimited(ws, cut(inner), ws), char('}')))(i)
}

fn key_value(i: &str) -> Result<(Expression, Expression)> {
    separated_pair(preceded(ws, alt((
        map(string, Expression::Str),
        map(string2, Expression::Str),
        map(ident, Expression::Str),
        delimited(char('['), expression, char(']')),
    ))), cut(preceded(ws, char(':'))), expression)(i)
}

fn paren(i: &str) -> Result<Box<Expression>> {
    context("list", delimited(char('('), map(expression, Box::new), preceded(ws, char(')'))))(i)
}

fn list(i: &str) -> Result<Vec<Expression>> {
    let inner = separated_list(preceded(ws, char(',')), list_item);
    context("list", delimited(char('['), delimited(ws, cut(inner), ws), char(']')))(i)
}

fn list_item(i: &str) -> Result<Expression> {
    alt((
        map(preceded(tag("..."), expression), |e| Expression::Splat(Box::new(e))),
        expression
    ))(i)
}

fn mutation(i: &str) -> Result<Expression> {
    context("mutation", map(pair(comparison, opt(preceded(ws, pair(alt((
        value(Operator::Assign, tag("=")),
        value(Operator::AssignAdd, tag("+=")),
        value(Operator::AssignSub, tag("-=")),
        value(Operator::AssignMod, tag("%=")),
        value(Operator::AssignMul, tag("*=")),
        value(Operator::AssignDiv, tag("/=")),
    )), comparison)))), makepair))(i)
}

fn comparison(i: &str) -> Result<Expression> {
    context("comparison", map(pair(logic_or, opt(preceded(ws, pair(alt((
        value(Operator::StrictEqual, tag("===")),
        value(Operator::Equal, tag("==")),
        value(Operator::StrictNotEqual, tag("!==")),
        value(Operator::NotEqual, tag("!=")),
        value(Operator::GreaterEqual, tag(">=")),
        value(Operator::LessEqual, tag("<=")),
        value(Operator::LessThan, tag("<")),
        value(Operator::GreaterThan, tag(">")),
    )), logic_or)))), makepair))(i)
}

fn logic_or(i: &str) -> Result<Expression> {
    context("logic", map(pair(logic_and, opt(preceded(ws, pair(
        value(Operator::LogicalOr, tag("&&")),
    logic_and)))), makepair))(i)
}

fn logic_and(i: &str) -> Result<Expression> {
    context("logic", map(pair(addition, opt(preceded(ws, pair(
        value(Operator::LogicalAnd, tag("||")),
    addition)))), makepair))(i)
}

fn addition(i: &str) -> Result<Expression> {
    context("addition", map(pair(multiplication, opt(preceded(ws, pair(alt((
        value(Operator::Add, tag("+")),
        value(Operator::Sub, tag("-")),
    )), multiplication)))), makepair))(i)
}

fn multiplication(i: &str) -> Result<Expression> {
    context("multiplication", map(pair(negation, opt(preceded(ws, pair(alt((
        value(Operator::Mult, tag("*")),
        value(Operator::Div, tag("/")),
    )), negation)))), makepair))(i)
}

fn negation(i: &str) -> Result<Expression> {
    context("negation", preceded(ws, map(pair(many0(preceded(ws,
        value(Operator::Not, tag("!")))),
    prefix), makechain)))(i)
}

fn prefix(i: &str) -> Result<Expression> {
    context("prefix", preceded(ws, map(pair(opt(alt((
        value(Operator::Incr, tag("++")),
        value(Operator::Decr, tag("--")),
        value(Operator::Add, tag("+")),
        value(Operator::Sub, tag("-")),
    ))), postfix), makeprefix)))(i)
}

fn postfix(i: &str) -> Result<Expression> {
    context("postfix", preceded(ws, map(pair(application, opt(preceded(ws, alt((
        value(Operator::Incr, tag("++")),
        value(Operator::Decr, tag("--")),
    ))))), makepostfix)))(i)
}

fn application(i: &str) -> Result<Expression> {
    context("application", preceded(ws, map(pair(primitive, opt(preceded(ws, alt((
        pair(value(Operator::Array, char('[')), terminated(primitive, char(']'))),
        pair(value(Operator::Application, char('(')), terminated(primitive, char(')'))),
        pair(value(Operator::Dot, char('.')), preceded(ws, map(ident, Expression::Ident))),
    ))))), makepair)))(i)
}

fn escaped_char(i: &str) -> Result<&str> {
    // let hex = map_opt(map_res(alphanumeric1, |s| u32::from_str_radix(s, 16)), std::char::from_u32);
    preceded(char('\\'), alt((
        value("\n", char('n')),
        value("\r", char('r')),
        value("\t", char('t')),
        value("\u{0B}", char('v')),
        value("\u{08}", char('b')),
        value("\u{0C}", char('f')),
        value("\\", char('\\')),
        value("/", char('/')),
        value("\"", char('"')),
        value("\'", char('\'')),
        value("", multispace1),
    )))(i)
}

fn ws(i: &str) -> Result<&str> {
    take_while(|c: char| c.is_whitespace())(i)
}

fn makepair(e: (Expression, Option<(Operator, Expression)>)) -> Expression {
    match e.1 {
        Some((op,r)) => Expression::Pair(op, Box::new(e.0), Box::new(r)),
        None => e.0
    }
}

fn makeprefix(e: (Option<Operator>, Expression)) -> Expression {
    match e.0 {
        Some(a) => Expression::Prefix(a, Box::new(e.1)),
        None => e.1
    }
}

fn makepostfix(e: (Expression, Option<Operator>)) -> Expression {
    match e.1 {
        Some(a) => Expression::Postfix(Box::new(e.0), a),
        None => e.0
    }
}

fn makechain(e: (Vec<Operator>, Expression)) -> Expression {
    e.0.iter().fold(e.1, |acc, item| Expression::Prefix(*item, Box::new(acc)))
}

#[test]
fn test_string() {
    assert_eq!(expression("\"\""), Ok(("", Expression::Str(String::from("")))));
    assert_eq!(expression(" \"\" "), Ok((" ", Expression::Str(String::from("")))));
    assert_eq!(expression(" \"a\" "), Ok((" ", Expression::Str(String::from("a")))));
    assert_eq!(expression(" \"Example\" "), Ok((" ", Expression::Str(String::from("Example")))));
    assert_eq!(expression("\"\\     a\""), Ok(("", Expression::Str(String::from("a")))));
    assert_eq!(expression("\"\\   b  a\""), Ok(("", Expression::Str(String::from("b  a")))));
    assert_eq!(expression("\"\\n\\n\""), Ok(("", Expression::Str(String::from("\n\n")))));
    assert_eq!(expression("\"✅\""), Ok(("", Expression::Str(String::from("✅")))));
    assert_eq!(expression("\"\\n\""), Ok(("", Expression::Str(String::from("\n")))));
    assert_eq!(expression("\"\\r\""), Ok(("", Expression::Str(String::from("\r")))));
    assert_eq!(expression("\"\\t\""), Ok(("", Expression::Str(String::from("\t")))));
    assert_eq!(expression("\"\\b\""), Ok(("", Expression::Str(String::from("\u{08}")))));
    assert_eq!(expression("\"\\v\""), Ok(("", Expression::Str(String::from("\u{0B}")))));
    assert_eq!(expression("\"\\f\""), Ok(("", Expression::Str(String::from("\u{0C}")))));
    assert_eq!(expression("\"\\\\\""), Ok(("", Expression::Str(String::from("\\")))));
    assert_eq!(expression("\"\\/\""), Ok(("", Expression::Str(String::from("/")))));
    assert_eq!(expression("\"\\\"\""), Ok(("", Expression::Str(String::from("\"")))));
    assert_eq!(expression("\"\\\'\""), Ok(("", Expression::Str(String::from("'")))));
}

#[test]
fn test_single_quoted_string() {
    assert_eq!(expression("''"), Ok(("", Expression::Str(String::from("")))));
    assert_eq!(expression(" '' "), Ok((" ", Expression::Str(String::from("")))));
    assert_eq!(expression(" 'a' "), Ok((" ", Expression::Str(String::from("a")))));
    assert_eq!(expression(" 'Example' "), Ok((" ", Expression::Str(String::from("Example")))));
    assert_eq!(expression("'\\     a'"), Ok(("", Expression::Str(String::from("a")))));
    assert_eq!(expression("'\\   b  a'"), Ok(("", Expression::Str(String::from("b  a")))));
    assert_eq!(expression("'\\n\\n'"), Ok(("", Expression::Str(String::from("\n\n")))));
    assert_eq!(expression("\"✅\""), Ok(("", Expression::Str(String::from("✅")))));
    assert_eq!(expression("'\\n'"), Ok(("", Expression::Str(String::from("\n")))));
    assert_eq!(expression("'\\r'"), Ok(("", Expression::Str(String::from("\r")))));
    assert_eq!(expression("'\\t'"), Ok(("", Expression::Str(String::from("\t")))));
    assert_eq!(expression("'\\b'"), Ok(("", Expression::Str(String::from("\u{08}")))));
    assert_eq!(expression("'\\v'"), Ok(("", Expression::Str(String::from("\u{0B}")))));
    assert_eq!(expression("'\\f'"), Ok(("", Expression::Str(String::from("\u{0C}")))));
    assert_eq!(expression("'\\\\'"), Ok(("", Expression::Str(String::from("\\")))));
    assert_eq!(expression("'\\/'"), Ok(("", Expression::Str(String::from("/")))));
    assert_eq!(expression("'\\\"'"), Ok(("", Expression::Str(String::from("\"")))));
    assert_eq!(expression("'\\''"), Ok(("", Expression::Str(String::from("'")))));
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
    assert_eq!(expression("123456789"), Ok(("", Expression::Double(123456789.0))));
    assert_eq!(expression("0."), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123."), Ok(("", Expression::Double(123.0))));
    assert_eq!(expression(".012300"), Ok(("", Expression::Double(0.0123))));
    assert_eq!(expression("0.012300"), Ok(("", Expression::Double(0.0123))));
    assert_eq!(expression("123.045600"), Ok(("", Expression::Double(123.0456))));
    assert_eq!(expression(".123e0"), Ok(("", Expression::Double(0.123))));
    assert_eq!(expression("0.123e0"), Ok(("", Expression::Double(0.123))));
    assert_eq!(expression("123.456e0"), Ok(("", Expression::Double(123.456))));
    assert_eq!(expression(".123e01"), Ok(("", Expression::Double(1.23))));
    assert_eq!(expression("0.123e01"), Ok(("", Expression::Double(1.23))));
    assert_eq!(expression("123.456e02"), Ok(("", Expression::Double(12345.6))));
    assert_eq!(expression(".123e+4"), Ok(("", Expression::Double(1230.0))));
    assert_eq!(expression("0.123e+4"), Ok(("", Expression::Double(1230.0))));
    assert_eq!(expression("123.456e+4"), Ok(("", Expression::Double(1234560.0))));
    assert_eq!(expression(".123e-4"), Ok(("", Expression::Double(0.0000123))));
    assert_eq!(expression("0.123e-4"), Ok(("", Expression::Double(0.0000123))));
    assert_eq!(expression("123.456e-4"), Ok(("", Expression::Double(0.0123456))));
    assert_eq!(expression("0e0"), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123e0"), Ok(("", Expression::Double(123.0))));
    assert_eq!(expression("0e01"), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123e02"), Ok(("", Expression::Double(12300.0))));
    assert_eq!(expression("0e+4"), Ok(("", Expression::Double(0.0))));
    assert_eq!(expression("123e+4"), Ok(("", Expression::Double(1230000.0))));
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
    assert_eq!(expression("0o07654321 "), Ok((" ", Expression::Octal(0o7654321))));
}

#[test]
fn test_hexadecimal() {
    assert_eq!(expression("0x3 "), Ok((" ", Expression::Hexadecimal(0x3))));
    assert_eq!(expression("0x0123789"), Ok(("", Expression::Hexadecimal(0x0123789))));
    assert_eq!(expression("0xABCDEF"), Ok(("", Expression::Hexadecimal(0xabcdef))));
    assert_eq!(expression("0xabcdef"), Ok(("", Expression::Hexadecimal(0xabcdef))));
}

#[test]
fn test_binarynum() {
    assert_eq!(expression("0b0"), Ok(("", Expression::BinaryNum(0b0))));
    assert_eq!(expression("0b1"), Ok(("", Expression::BinaryNum(0b1))));
    assert_eq!(expression("0b01010"), Ok(("", Expression::BinaryNum(0b01010))));
    assert_eq!(expression("0b1010111"), Ok(("", Expression::BinaryNum(0b1010111))));
}

#[test]
fn test_identifier() {
    assert_eq!(expression("hello"), Ok(("", Expression::Ident(String::from("hello")))));
}

#[test]
fn test_list() {
    assert_eq!(expression(" [ ] "), Ok((" ", Expression::List(vec![]))));
    assert_eq!(expression("[[]]"), Ok(("", Expression::List(vec![Expression::List(vec![])]))));
    assert_eq!(expression("[]"), Ok(("", Expression::List(vec![]))));
    assert_eq!(expression("[ 1 ]"), Ok(("", Expression::List(vec![ Expression::Double(1.0) ]))));
    assert_eq!(expression("[ 1, 2 ]"), Ok(("", Expression::List(vec![ Expression::Double(1.0), Expression::Double(2.0) ]))));
    assert_eq!(expression("[ 1, \"2\" ]"), Ok(("", Expression::List(vec![ Expression::Double(1.0), Expression::Str(String::from("2")) ]))));
    assert_eq!(expression("[ ...a, 1 ]"), Ok(("", Expression::List(vec![ Expression::Splat(Box::new(Expression::Ident(String::from("a")))), Expression::Double(1.0) ]))));
}

#[test]
fn test_object() {
    assert_eq!(expression("{\"a\": 1}"), Ok(("", Expression::Object(vec![(Expression::Str(String::from("a")), Expression::Double(1.0))]))));
    assert_eq!(expression("{a: 1, b: 2}"), Ok(("", Expression::Object(vec![
        (Expression::Str(String::from("a")), Expression::Double(1.0)),
        (Expression::Str(String::from("b")), Expression::Double(2.0)),
    ]))));
    assert_eq!(expression("{[1]: 1, [\"a\"]: 2}"), Ok(("", Expression::Object(vec![
        (Expression::Double(1.0), Expression::Double(1.0)),
        (Expression::Str(String::from("a")), Expression::Double(2.0)),
    ]))));
}

#[test]
fn test_parenthesis() {
    assert_eq!(expression("(1)"), Ok(("", Expression::Paren(Box::new(Expression::Double(1.0))))));
    assert_eq!(expression("([])"), Ok(("", Expression::Paren(Box::new(Expression::List(vec![]))))));
    assert_eq!(expression(" ( 1 ) "), Ok((" ", Expression::Paren(Box::new(Expression::Double(1.0))))));
    assert_eq!(expression(" ( [ ] ) "), Ok((" ", Expression::Paren(Box::new(Expression::List(vec![]))))));
}

#[test]
fn test_mutation() {
    assert_eq!(expression(" 1 = 2 "), Ok((" ", Expression::Pair(Operator::Assign, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 += 2 "), Ok((" ", Expression::Pair(Operator::AssignAdd, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 -= 2 "), Ok((" ", Expression::Pair(Operator::AssignSub, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 %= 2 "), Ok((" ", Expression::Pair(Operator::AssignMod, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 *= 2 "), Ok((" ", Expression::Pair(Operator::AssignMul, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 /= 2 "), Ok((" ", Expression::Pair(Operator::AssignDiv, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
}

#[test]
fn test_comparison() {
    assert_eq!(expression(" 1 == 2 "), Ok((" ", Expression::Pair(Operator::Equal, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 != 2 "), Ok((" ", Expression::Pair(Operator::NotEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 > 2 "), Ok((" ", Expression::Pair(Operator::GreaterThan, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 < 2 "), Ok((" ", Expression::Pair(Operator::LessThan, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 >= 2 "), Ok((" ", Expression::Pair(Operator::GreaterEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 <= 2 "), Ok((" ", Expression::Pair(Operator::LessEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 === 2 "), Ok((" ", Expression::Pair(Operator::StrictEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 !== 2 "), Ok((" ", Expression::Pair(Operator::StrictNotEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
}

#[test]
fn test_logic() {
    assert_eq!(expression(" 1 || 2 "), Ok((" ", Expression::Pair(Operator::LogicalAnd, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 && 2 "), Ok((" ", Expression::Pair(Operator::LogicalOr, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
}

#[test]
fn test_arithmetic() {
    assert_eq!(expression(" 1 + 2 "), Ok((" ", Expression::Pair(Operator::Add, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 - 2 "), Ok((" ", Expression::Pair(Operator::Sub, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 * 2 "), Ok((" ", Expression::Pair(Operator::Mult, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 / 2 "), Ok((" ", Expression::Pair(Operator::Div, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
}

#[test]
fn test_prefix() {
    assert_eq!(expression(" ++ 2 "), Ok((" ", Expression::Prefix(Operator::Incr, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" -- 2 "), Ok((" ", Expression::Prefix(Operator::Decr, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" + 2 "), Ok((" ", Expression::Prefix(Operator::Add, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" - 2 "), Ok((" ", Expression::Prefix(Operator::Sub, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" ! 2 "), Ok((" ", Expression::Prefix(Operator::Not, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" !!2 "), Ok((" ", Expression::Prefix(Operator::Not, Box::new(Expression::Prefix(Operator::Not, Box::new(Expression::Double(2.0))))))));
    assert_eq!(expression(" ! ! 2 "), Ok((" ", Expression::Prefix(Operator::Not, Box::new(Expression::Prefix(Operator::Not, Box::new(Expression::Double(2.0))))))));
}

#[test]
fn test_postfix() {
    assert_eq!(expression(" a++"), Ok(("", Expression::Postfix(Box::new(Expression::Ident(String::from("a"))), Operator::Incr))));
    assert_eq!(expression(" a--"), Ok(("", Expression::Postfix(Box::new(Expression::Ident(String::from("a"))), Operator::Decr))));
}

#[test]
fn test_application() {
    assert_eq!(expression(" a[a] "), Ok((" ", Expression::Pair(Operator::Array, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression(" a(a) "), Ok((" ", Expression::Pair(Operator::Application, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression(" a.a"), Ok(("", Expression::Pair(Operator::Dot, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
}

#[test]
fn test_nesting_bench() {
    let start = std::time::Instant::now();
    expression("((((((((1.0))))))))").unwrap();
    expression("[[[[[[[[1.0]]]]]]]]").unwrap();
    expression("{a:{a:{a:{a:{a:{a:{a:{a:1.0}}}}}}}}").unwrap();
    let elapsed = start.elapsed();
    assert!(elapsed.as_millis() < 10);
}
