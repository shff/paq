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

    Unary(Operator, Box<Expression>),
    Binary(Operator, Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Assign, AssignAdd, AssignSub, AssignPow, AssignMod, AssignMul, AssignDiv,
    AssignLeft, AssignRight, AssignURight, AssignAnd, AssignXor, AssignOr,
    Equal, NotEqual, LessThan, GreaterThan, LessEqual, GreaterEqual, StrictEqual, StrictNotEqual,
    LogicalAnd, LogicalOr, Coalesce, BitwiseOr, BitwiseXor, BitwiseAnd,
    Add, Sub, Mult, Div, Mod, Power, URightShift, RightShift, LeftShift,
    Not, Incr, Decr,
    Array, Application, Dot, Optional,
    InstanceOf, In, TypeOf, Void, Delete, Await, Yield,
}

pub fn expression(i: &str) -> Result<Expression> {
    preceded(ws, yieldd)(i)
}

fn yieldd(i: &str) -> Result<Expression> {
    context("yield", preceded(ws, map(pair(many0(
        value(Operator::Yield, tag("yield")),
    ), mutation), makechain)))(i)
}

fn mutation(i: &str) -> Result<Expression> {
    context("mutation", map(pair(ternary, many0(preceded(ws, pair(alt((
        value(Operator::Assign, tag("=")),
        value(Operator::AssignAdd, tag("+=")),
        value(Operator::AssignSub, tag("-=")),
        value(Operator::AssignPow, tag("**=")),
        value(Operator::AssignMul, tag("*=")),
        value(Operator::AssignDiv, tag("/=")),
        value(Operator::AssignMod, tag("%=")),
        value(Operator::AssignLeft, tag("<<=")),
        value(Operator::AssignURight, tag(">>>=")),
        value(Operator::AssignRight, tag(">>=")),
        value(Operator::AssignAnd, tag("&=")),
        value(Operator::AssignXor, tag("^=")),
        value(Operator::AssignOr, tag("|=")),
    )), ternary)))), makechain2))(i)
}

fn ternary(i: &str) -> Result<Expression> {
    context("ternary", preceded(ws, map(pair(equality, opt(preceded(preceded(ws, tag("?")),
        separated_pair(preceded(ws, equality), preceded(ws, tag(":")),
          preceded(ws, equality))))), maketernary)))(i)
}

fn equality(i: &str) -> Result<Expression> {
    context("equality", map(pair(comparison, many0(preceded(ws, pair(alt((
        value(Operator::StrictEqual, tag("===")),
        value(Operator::Equal, tag("==")),
        value(Operator::StrictNotEqual, tag("!==")),
        value(Operator::NotEqual, tag("!=")),
    )), comparison)))), makechain2))(i)
}

fn comparison(i: &str) -> Result<Expression> {
    context("comparison", map(pair(bitwise, many0(preceded(ws, pair(alt((
        value(Operator::GreaterEqual, tag(">=")),
        value(Operator::LessEqual, tag("<=")),
        value(Operator::GreaterThan, tag(">")),
        value(Operator::LessThan, tag("<")),
        value(Operator::InstanceOf, tag("instanceof")),
        value(Operator::In, tag("in")),
    )), bitwise)))), makechain2))(i)
}

fn bitwise(i: &str) -> Result<Expression> {
    context("bitwise", map(pair(logic_or, many0(preceded(ws, pair(alt((
        value(Operator::URightShift, tag(">>>")),
        value(Operator::RightShift, tag(">>")),
        value(Operator::LeftShift, tag("<<")),
    )), logic_or)))), makechain2))(i)
}

fn logic_or(i: &str) -> Result<Expression> {
    context("logic_or", map(pair(logic_and, many0(preceded(ws, pair(
        value(Operator::LogicalOr, tag("&&")),
    logic_and)))), makechain2))(i)
}

fn logic_and(i: &str) -> Result<Expression> {
    context("logic_and", map(pair(coalesce, many0(preceded(ws, pair(
        value(Operator::LogicalAnd, tag("||")),
    coalesce)))), makechain2))(i)
}

fn coalesce(i: &str) -> Result<Expression> {
    context("coalesce", map(pair(bitwise_or, many0(preceded(ws, pair(
        value(Operator::Coalesce, tag("??")),
    bitwise_or)))), makechain2))(i)
}

fn bitwise_or(i: &str) -> Result<Expression> {
    context("bitwise_or", map(pair(bitwise_xor, many0(preceded(ws, pair(
        value(Operator::BitwiseOr, tag("|")),
    bitwise_xor)))), makechain2))(i)
}

fn bitwise_xor(i: &str) -> Result<Expression> {
    context("bitwise_xor", map(pair(bitwise_and, many0(preceded(ws, pair(
        value(Operator::BitwiseXor, tag("^")),
    bitwise_and)))), makechain2))(i)
}

fn bitwise_and(i: &str) -> Result<Expression> {
    context("bitwise_and", map(pair(addition, many0(preceded(ws, pair(
        value(Operator::BitwiseAnd, tag("&")),
    addition)))), makechain2))(i)
}

fn addition(i: &str) -> Result<Expression> {
    context("addition", map(pair(multiplication, many0(preceded(ws, pair(alt((
        value(Operator::Add, tag("+")),
        value(Operator::Sub, tag("-")),
    )), multiplication)))), makechain2))(i)
}

fn multiplication(i: &str) -> Result<Expression> {
    context("multiplication", map(pair(power, many0(preceded(ws, pair(alt((
        value(Operator::Mult, tag("*")),
        value(Operator::Div, tag("/")),
        value(Operator::Mod, tag("%")),
    )), power)))), makechain2))(i)
}

fn power(i: &str) -> Result<Expression> {
    context("power", map(pair(negation, many0(preceded(ws, pair(
        value(Operator::Power, tag("**")),
    negation)))), makechain2))(i)
}

fn negation(i: &str) -> Result<Expression> {
    context("negation", preceded(ws, map(pair(many0(preceded(ws,
        value(Operator::Not, tag("!")))),
    prefix), makechain)))(i)
}

fn prefix(i: &str) -> Result<Expression> {
    context("prefix", preceded(ws, map(pair(many0(alt((
        value(Operator::Incr, tag("++")),
        value(Operator::Decr, tag("--")),
        value(Operator::Add, tag("+")),
        value(Operator::Sub, tag("-")),
        value(Operator::TypeOf, tag("typeof")),
        value(Operator::Void, tag("void")),
        value(Operator::Delete, tag("delete")),
        value(Operator::Await , tag("await ")),
    ))), postfix), makechain)))(i)
}

fn postfix(i: &str) -> Result<Expression> {
    context("postfix", preceded(ws, map(pair(action, many0(preceded(ws, alt((
        value(Operator::Incr, tag("++")),
        value(Operator::Decr, tag("--")),
    ))))), makechainb)))(i)
}

fn action(i: &str) -> Result<Expression> {
    context("action", preceded(ws, map(pair(primitive, many0(preceded(ws, alt((
        pair(value(Operator::Array, char('[')), terminated(expression, char(']'))),
        pair(value(Operator::Optional, tag("?.")), preceded(ws, map(ident, Expression::Ident))),
        pair(value(Operator::Dot, char('.')), preceded(ws, map(ident, Expression::Ident))),
        pair(value(Operator::Application, char('(')), terminated(expression, char(')'))),
    ))))), makechain2)))(i)
}

fn primitive(i: &str) -> Result<Expression> {
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
    let inner = separated_list(preceded(ws, char(',')), alt((splat, expression)));
    context("list", delimited(char('['), delimited(ws, cut(inner), ws), char(']')))(i)
}

fn splat(i: &str) -> Result<Expression> {
    map(map(preceded(tag("..."), expression), Box::new), Expression::Splat)(i)
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

fn maketernary(e: (Expression, Option<(Expression, Expression)>)) -> Expression {
    match e.1 {
        Some((b,c)) => Expression::Ternary(Box::new(e.0), Box::new(b), Box::new(c)),
        None => e.0,
    }
}

fn makechain(e: (Vec<Operator>, Expression)) -> Expression {
    e.0.iter().fold(e.1, |acc, op| Expression::Unary(*op, Box::new(acc)))
}

fn makechainb(e: (Expression, Vec<Operator>)) -> Expression {
    e.1.iter().fold(e.0, |acc, op| Expression::Unary(*op, Box::new(acc)))
}

fn makechain2(e: (Expression, Vec<(Operator, Expression)>)) -> Expression {
    e.1.iter().fold(e.0, |a,(op,b)| Expression::Binary(*op, Box::new(a), Box::new(b.clone())))
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
    assert_eq!(expression("[ ...[], 1 ]"), Ok(("", Expression::List(vec![ Expression::Splat(Box::new(Expression::List(vec![]))), Expression::Double(1.0) ]))));
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
    assert_eq!(expression("{a: {}}"), Ok(("", Expression::Object(vec![
        (Expression::Str(String::from("a")), Expression::Object(vec![])),
    ]))));
}

#[test]
fn test_parenthesis() {
    assert_eq!(expression("(1)"), Ok(("", Expression::Paren(Box::new(Expression::Double(1.0))))));
    assert_eq!(expression("([])"), Ok(("", Expression::Paren(Box::new(Expression::List(vec![]))))));
    assert_eq!(expression(" ( 1 ) "), Ok((" ", Expression::Paren(Box::new(Expression::Double(1.0))))));
    assert_eq!(expression(" ( [ ] ) "), Ok((" ", Expression::Paren(Box::new(Expression::List(vec![]))))));
    // assert_eq!(expression("()"), Ok(("", Expression::Paren(Box::new(Expression::Double(1.0))))));
}

#[test]
fn test_mutation() {
    assert_eq!(expression(" 1 = 2 "), Ok((" ", Expression::Binary(Operator::Assign, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 += 2 "), Ok((" ", Expression::Binary(Operator::AssignAdd, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 -= 2 "), Ok((" ", Expression::Binary(Operator::AssignSub, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 %= 2 "), Ok((" ", Expression::Binary(Operator::AssignMod, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 *= 2 "), Ok((" ", Expression::Binary(Operator::AssignMul, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 /= 2 "), Ok((" ", Expression::Binary(Operator::AssignDiv, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 <<= 2 "), Ok((" ", Expression::Binary(Operator::AssignLeft, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 >>>= 2 "), Ok((" ", Expression::Binary(Operator::AssignURight, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 >>= 2 "), Ok((" ", Expression::Binary(Operator::AssignRight, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 &= 2 "), Ok((" ", Expression::Binary(Operator::AssignAnd, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 ^= 2 "), Ok((" ", Expression::Binary(Operator::AssignXor, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 |= 2 "), Ok((" ", Expression::Binary(Operator::AssignOr, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 = 2 = 3 "), Ok((" ", Expression::Binary(Operator::Assign, Box::new(Expression::Binary(Operator::Assign, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0)))), Box::new(Expression::Double(3.0))))));
}

#[test]
fn test_ternary() {
    assert_eq!(expression("1 ? 2 : 3"), Ok(("", Expression::Ternary(Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0)), Box::new(Expression::Double(3.0))))));
}

#[test]
fn test_comparison() {
    assert_eq!(expression(" 1 == 2 "), Ok((" ", Expression::Binary(Operator::Equal, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 != 2 "), Ok((" ", Expression::Binary(Operator::NotEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 > 2 "), Ok((" ", Expression::Binary(Operator::GreaterThan, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 < 2 "), Ok((" ", Expression::Binary(Operator::LessThan, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 >= 2 "), Ok((" ", Expression::Binary(Operator::GreaterEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 <= 2 "), Ok((" ", Expression::Binary(Operator::LessEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 === 2 "), Ok((" ", Expression::Binary(Operator::StrictEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 !== 2 "), Ok((" ", Expression::Binary(Operator::StrictNotEqual, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 instanceof 2 "), Ok((" ", Expression::Binary(Operator::InstanceOf, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 in 2 "), Ok((" ", Expression::Binary(Operator::In, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 == 2 == 3 "), Ok((" ", Expression::Binary(Operator::Equal, Box::new(Expression::Binary(Operator::Equal, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0)))), Box::new(Expression::Double(3.0))))));
}

#[test]
fn test_logic() {
    assert_eq!(expression(" 1 || 2 "), Ok((" ", Expression::Binary(Operator::LogicalAnd, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 && 2 "), Ok((" ", Expression::Binary(Operator::LogicalOr, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 ?? 2 "), Ok((" ", Expression::Binary(Operator::Coalesce, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
}

#[test]
fn test_bitwise() {
    assert_eq!(expression(" 1 | 2 "), Ok((" ", Expression::Binary(Operator::BitwiseOr, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 ^ 2 "), Ok((" ", Expression::Binary(Operator::BitwiseXor, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 & 2 "), Ok((" ", Expression::Binary(Operator::BitwiseAnd, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));

    assert_eq!(expression(" 1 >> 2 "), Ok((" ", Expression::Binary(Operator::RightShift, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 >>> 2 "), Ok((" ", Expression::Binary(Operator::URightShift, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 << 2 "), Ok((" ", Expression::Binary(Operator::LeftShift, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
}

#[test]
fn test_arithmetic() {
    assert_eq!(expression(" 1 + 2 "), Ok((" ", Expression::Binary(Operator::Add, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 - 2 "), Ok((" ", Expression::Binary(Operator::Sub, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 * 2 "), Ok((" ", Expression::Binary(Operator::Mult, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 / 2 "), Ok((" ", Expression::Binary(Operator::Div, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 % 2 "), Ok((" ", Expression::Binary(Operator::Mod, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" 1 ** 2 "), Ok((" ", Expression::Binary(Operator::Power, Box::new(Expression::Double(1.0)), Box::new(Expression::Double(2.0))))));
}

#[test]
fn test_prefix() {
    assert_eq!(expression(" ++ 2 "), Ok((" ", Expression::Unary(Operator::Incr, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" -- 2 "), Ok((" ", Expression::Unary(Operator::Decr, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" + 2 "), Ok((" ", Expression::Unary(Operator::Add, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" - 2 "), Ok((" ", Expression::Unary(Operator::Sub, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" ! 2 "), Ok((" ", Expression::Unary(Operator::Not, Box::new(Expression::Double(2.0))))));
    assert_eq!(expression(" !!2 "), Ok((" ", Expression::Unary(Operator::Not, Box::new(Expression::Unary(Operator::Not, Box::new(Expression::Double(2.0))))))));
    assert_eq!(expression(" ! ! 2 "), Ok((" ", Expression::Unary(Operator::Not, Box::new(Expression::Unary(Operator::Not, Box::new(Expression::Double(2.0))))))));

    assert_eq!(expression("typeof a"), Ok(("", Expression::Unary(Operator::TypeOf, Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression("void a"), Ok(("", Expression::Unary(Operator::Void, Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression("delete a"), Ok(("", Expression::Unary(Operator::Delete, Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression("await a"), Ok(("", Expression::Unary(Operator::Await, Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression("yield a"), Ok(("", Expression::Unary(Operator::Yield, Box::new(Expression::Ident(String::from("a")))))));
}

#[test]
fn test_postfix() {
    assert_eq!(expression(" a++"), Ok(("", Expression::Unary(Operator::Incr, Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression(" a--"), Ok(("", Expression::Unary(Operator::Decr, Box::new(Expression::Ident(String::from("a")))))));
}

#[test]
fn test_action() {
    assert_eq!(expression(" a?.a"), Ok(("", Expression::Binary(Operator::Optional, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression(" a[a]"), Ok(("", Expression::Binary(Operator::Array, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression(" a(a)"), Ok(("", Expression::Binary(Operator::Application, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
    assert_eq!(expression(" a.a"), Ok(("", Expression::Binary(Operator::Dot, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
    // assert_eq!(expression(" a()"), Ok(("", Expression::Binary(Operator::Application, Box::new(Expression::Ident(String::from("a"))), Box::new(Expression::Ident(String::from("a")))))));
}

#[test]
fn test_complex() {
    fn assert_complete(i: &str) {
        assert!(expression(i).is_ok());
        assert_eq!(expression(i).unwrap().0, "");
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
    // assert_complete("a()");
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
