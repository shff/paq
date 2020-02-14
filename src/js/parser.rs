use crate::js::combinators::*;

const RESERVED: &[&str] = &["const", "for"];

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Block(Vec<Statement>),
    If((Box<Expression>, Box<Statement>, Option<Box<Statement>>)),
    While((Box<Expression>, Box<Statement>)),
    For(((Option<Box<Statement>>, Option<Expression>, Option<Expression>), Box<Statement>)),
    Declaration((Operator, Vec<Expression>)),
    Return(Option<Expression>),
    Throw(Option<Expression>),
    Continue,
    Break,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Str(String),
    Ident(String),
    Double(f64),
    Octal(u64),
    Hexadecimal(u64),
    BinaryNum(u64),
    List(Vec<Expression>),
    Object(Vec<Expression>),
    Paren(Box<Expression>),
    Closure((Vec<Expression>, Box<Expression>)),
    Function((Option<String>, Vec<Expression>, Vec<Statement>)),
    Generator((Option<String>, Vec<Expression>, Vec<Statement>)),
    Unary(Operator, Box<Expression>),
    Binary(Operator, Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),

    Args(Vec<Expression>),
    Splat(Box<Expression>),
    KeyValue((Box<Expression>, Box<Expression>)),
    Parameter((String, Option<Box<Expression>>)),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Add, Sub, Mul, Div, Mod, Pow, URBit, RBit, LBit, Not, Incr, Dec,
    Eq, NotEq, Lt, Gt, LtEq, GtEq, StrictEq, StrictNotEq,
    And, Or, Coalesce, BitOr, BitXor, BitAnd,
    Assign, AssignAdd, AssignSub, AssignPow, AssignMod, AssignMul, AssignDiv,
    AssignLeft, AssignRight, AssignURight, AssignAnd, AssignXor, AssignOr,
    InstanceOf, In, TypeOf, Void, Delete, Await, Yield, YieldStar, New,
    Array, Call, Dot, Optional,
    Var, Let, Const,
}

pub fn block(i: &str) -> ParseResult<Vec<Statement>> {
    many(statement)(i)
}

fn statement(i: &str) -> ParseResult<Statement> {
    ws(choice((
        map(codeblock, Statement::Block),
        map(if_block, Statement::If),
        map(while_block, Statement::While),
        map(for_block, Statement::For),
        single_line_statements,
    )))(i)
}

fn single_line_statements(i: &str) -> ParseResult<Statement> {
    let end = choice((ws(eoi), ws(tag(";")), take_while(|c| c == '\n' || c == '\r'), value(ws(peek(tag("}"))), "")));
    left(choice((
        map(tag("continue"), |_| Statement::Continue),
        map(tag("break"), |_| Statement::Break),
        map(right(tag("return"), opt(expression)), Statement::Return),
        map(right(tag("throw"), opt(expression)), Statement::Throw),
        assignment,
    )), end)(i)
}

fn assignment(i: &str) -> ParseResult<Statement> {
    choice((
        map(declaration, Statement::Declaration),
        map(expression, Statement::Expression),
    ))(i)
}

fn declaration(i: &str) -> ParseResult<(Operator, Vec<Expression>)> {
    ws(pair(choice((
        value(tag("var"), Operator::Var),
        value(tag("let"), Operator::Let),
        value(tag("const"), Operator::Const),
    )), chain(ws(tag(",")), mutation)))(i)
}

fn if_block(i: &str) -> ParseResult<(Box<Expression>, Box<Statement>, Option<Box<Statement>>)> {
    let else_block = ws(right(tag("else"), boxed(statement)));
    let inner = trio(paren, boxed(statement), opt(else_block));
    ws(right(tag("if"), inner))(i)
}

fn while_block(i: &str) -> ParseResult<(Box<Expression>, Box<Statement>)> {
    let inner = pair(paren, boxed(statement));
    ws(right(tag("while"), inner))(i)
}

fn for_block(i: &str) -> ParseResult<((Option<Box<Statement>>, Option<Expression>, Option<Expression>), Box<Statement>)> {
    let trio = trio(opt(boxed(assignment)), right(ws(tag(";")), opt(expression)),
        right(ws(tag(";")), opt(expression)));
    let inner = pair(middle(ws(tag("(")), trio, ws(tag(")"))), boxed(statement));
    ws(right(tag("for"), inner))(i)
}

pub fn expression(i: &str) -> ParseResult<Expression> {
    map(prefix(choice((
        value(tag("yield*"), Operator::YieldStar),
        value(tag("yield"), Operator::Yield),
    )), mutation), makechain)(i)
}

fn mutation(i: &str) -> ParseResult<Expression> {
    map(infix(ternary, choice((
        value(tag("="), Operator::Assign),
        value(tag("+="), Operator::AssignAdd),
        value(tag("-="), Operator::AssignSub),
        value(tag("**="), Operator::AssignPow),
        value(tag("*="), Operator::AssignMul),
        value(tag("/="), Operator::AssignDiv),
        value(tag("%="), Operator::AssignMod),
        value(tag("<<="), Operator::AssignLeft),
        value(tag(">>>="), Operator::AssignURight),
        value(tag(">>="), Operator::AssignRight),
        value(tag("&="), Operator::AssignAnd),
        value(tag("^="), Operator::AssignXor),
        value(tag("|="), Operator::AssignOr),
    ))), makechain2)(i)
}

fn ternary(i: &str) -> ParseResult<Expression> {
    let conds = right(ws(tag("?")), outer(
        equality, ws(tag(":")), equality));
    ws(map(pair(equality, many(conds)), maketernary))(i)
}

fn equality(i: &str) -> ParseResult<Expression> {
    map(infix(comparison, choice((
        value(tag("==="), Operator::StrictEq),
        value(tag("=="), Operator::Eq),
        value(tag("!=="), Operator::StrictNotEq),
        value(tag("!="), Operator::NotEq),
    ))), makechain2)(i)
}

fn comparison(i: &str) -> ParseResult<Expression> {
    map(infix(bitwise, choice((
        value(tag(">="), Operator::GtEq),
        value(tag("<="), Operator::LtEq),
        value(tag(">"), Operator::Gt),
        value(tag("<"), Operator::Lt),
        value(tag("instanceof"), Operator::InstanceOf),
        value(tag("in"), Operator::In),
    ))), makechain2)(i)
}

fn bitwise(i: &str) -> ParseResult<Expression> {
    map(infix(logic_or, choice((
        value(tag(">>>"), Operator::URBit),
        value(tag(">>"), Operator::RBit),
        value(tag("<<"), Operator::LBit),
    ))), makechain2)(i)
}

fn logic_or(i: &str) -> ParseResult<Expression> {
    map(infix(logic_and,
        value(tag("&&"), Operator::Or)
    ), makechain2)(i)
}

fn logic_and(i: &str) -> ParseResult<Expression> {
    map(infix(coalesce,
        value(tag("||"), Operator::And)
    ), makechain2)(i)
}

fn coalesce(i: &str) -> ParseResult<Expression> {
    map(infix(bitwise_or,
        value(tag("??"), Operator::Coalesce)
    ), makechain2)(i)
}

fn bitwise_or(i: &str) -> ParseResult<Expression> {
    map(infix(bitwise_xor,
        value(tag("|"), Operator::BitOr)
    ), makechain2)(i)
}

fn bitwise_xor(i: &str) -> ParseResult<Expression> {
    map(infix(bitwise_and,
        value(tag("^"), Operator::BitXor)
    ), makechain2)(i)
}

fn bitwise_and(i: &str) -> ParseResult<Expression> {
    map(infix(addition,
        value(tag("&"), Operator::BitAnd)
    ), makechain2)(i)
}

fn addition(i: &str) -> ParseResult<Expression> {
    map(infix(multiplication, choice((
        value(tag("+"), Operator::Add),
        value(tag("-"), Operator::Sub),
    ))), makechain2)(i)
}

fn multiplication(i: &str) -> ParseResult<Expression> {
    map(infix(power, choice((
        value(tag("*"), Operator::Mul),
        value(tag("/"), Operator::Div),
        value(tag("%"), Operator::Mod),
    ))), makechain2)(i)
}

fn power(i: &str) -> ParseResult<Expression> {
    map(infix(negation, value(tag("**"), Operator::Pow)), makechain2)(i)
}

fn negation(i: &str) -> ParseResult<Expression> {
    map(prefix(value(tag("!"), Operator::Not), prefixes), makechain)(i)
}

fn prefixes(i: &str) -> ParseResult<Expression> {
    map(prefix(choice((
        value(tag("++"), Operator::Incr),
        value(tag("--"), Operator::Dec),
        value(tag("+"), Operator::Add),
        value(tag("-"), Operator::Sub),
        value(tag("typeof"), Operator::TypeOf),
        value(tag("void"), Operator::Void),
        value(tag("delete"), Operator::Delete),
        value(tag("await "), Operator::Await ),
    )), postfix), makechain)(i)
}

fn postfix(i: &str) -> ParseResult<Expression> {
    map(pair(creation, many(ws(choice((
        value(tag("++"), Operator::Incr),
        value(tag("--"), Operator::Dec),
    ))))), makechainb)(i)
}

fn creation(i: &str) -> ParseResult<Expression> {
    map(prefix(value(tag("new"), Operator::New), action), makechain)(i)
}

fn action(i: &str) -> ParseResult<Expression> {
    map(pair(primitive, many(ws(choice((
        pair(value(tag("["), Operator::Array), left(expression, ws(tag("]")))),
        pair(value(tag("?."), Operator::Optional), map(ident, Expression::Ident)),
        pair(value(tag("."), Operator::Dot), map(ident, Expression::Ident)),
        pair(value(tag("("), Operator::Call), left(map(arguments, Expression::Args), ws(tag(")"))))
    ))))), makechain2)(i)
}

fn primitive(i: &str) -> ParseResult<Expression> {
    ws(choice((
        map(string('"'), Expression::Str),
        map(string('\''), Expression::Str),
        map(right(tag("0o"), number(8)), Expression::Octal),
        map(right(tag("0x"), number(16)), Expression::Hexadecimal),
        map(right(tag("0b"), number(2)), Expression::BinaryNum),
        map(double, Expression::Double),
        map(generator, Expression::Generator),
        map(function, Expression::Function),
        map(ident, Expression::Ident),
        map(object, Expression::Object),
        map(closure, Expression::Closure),
        map(paren, Expression::Paren),
        map(list, Expression::List),
    )))(i)
}

fn ident(i: &str) -> ParseResult<String> {
    ws(map(check(take_while(|c| c.is_alphanumeric()), |s| !RESERVED.contains(s)), String::from))(i)
}

fn object(i: &str) -> ParseResult<Vec<Expression>> {
    middle(tag("{"), chain(ws(tag(",")), key_value), ws(tag("}")))(i)
}

fn key_value(i: &str) -> ParseResult<Expression> {
    choice((
        map(outer(ws(boxed(choice((
            map(string('"'), Expression::Str),
            map(string('\''), Expression::Str),
            map(ident, Expression::Str),
            middle(tag("["), expression, tag("]")),
        )))), ws(tag(":")), boxed(expression)), Expression::KeyValue),
        map(ident, Expression::Ident),
        splat,
    ))(i)
}

fn paren(i: &str) -> ParseResult<Box<Expression>> {
    ws(middle(tag("("), boxed(expression), ws(tag(")"))))(i)
}

fn list(i: &str) -> ParseResult<Vec<Expression>> {
    ws(middle(tag("["), arguments, ws(tag("]"))))(i)
}

fn closure(i: &str) -> ParseResult<(Vec<Expression>, Box<Expression>)> {
    ws(outer(parameters, ws(tag("=>")), boxed(expression)))(i)
}

fn function(i: &str) -> ParseResult<(Option<String>, Vec<Expression>, Vec<Statement>)> {
    let inner = trio(ws(opt(ident)), parameters, codeblock);
    ws(right(tag("function"), inner))(i)
}

fn generator(i: &str) -> ParseResult<(Option<String>, Vec<Expression>, Vec<Statement>)> {
    let inner = trio(ws(opt(ident)), parameters, codeblock);
    ws(right(tag("function*"), inner))(i)
}

fn codeblock(i: &str) -> ParseResult<Vec<Statement>> {
    ws(middle(tag("{"), block, ws(tag("}"))))(i)
}

fn parameters(i: &str) -> ParseResult<Vec<Expression>> {
    let param = map(pair(ident, opt(right(ws(tag("=")),
        boxed(ws(expression))))), Expression::Parameter);
    let inner = chain(ws(tag(",")), choice((splat, param)));
    ws(middle(tag("("), inner, ws(tag(")"))))(i)
}

fn arguments(i: &str) -> ParseResult<Vec<Expression>> {
    ws(chain(tag(","), choice((splat, expression))))(i)
}

fn splat(i: &str) -> ParseResult<Expression> {
    ws(map(boxed(right(tag("..."), expression)), Expression::Splat))(i)
}

fn ws<'a, T>(item: impl Fn(&'a str) -> ParseResult<T>) -> impl Fn(&'a str) -> ParseResult<T> {
    right(whitespace, right(comments, item))
}

fn comments(i: &str) -> ParseResult<Vec<&str>> {
    many(right(tag("/"), choice((
        right(tag("/"), left(take_until("\n"), tag("\n"))),
        right(tag("*"), left(take_until("*/"), tag("*/"))),
    ))))(i)
}

fn maketernary(e: (Expression, Vec<(Expression, Expression)>)) -> Expression {
    e.1.iter().fold(e.0, |a, (b,c)| Expression::Ternary(Box::new(a),
        Box::new(b.clone()), Box::new(c.clone())))
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
