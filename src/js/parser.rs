use crate::js::combinators::*;

const RESERVED: &[&str] = &["const", "for"];

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Expression(Expression<'a>),
    Block(Vec<Statement<'a>>),
    If(
        (
            Box<Expression<'a>>,
            Box<Statement<'a>>,
            Option<Box<Statement<'a>>>,
        ),
    ),
    While((Box<Expression<'a>>, Box<Statement<'a>>)),
    For(
        (
            (
                Option<Box<Statement<'a>>>,
                Option<Expression<'a>>,
                Option<Expression<'a>>,
            ),
            Box<Statement<'a>>,
        ),
    ),
    Declaration((&'a str, Vec<Expression<'a>>)),
    Return(Option<Expression<'a>>),
    Throw(Option<Expression<'a>>),
    Continue,
    Break,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression<'a> {
    Str(String),
    Ident(String),
    Double(f64),
    Octal(u64),
    Hexadecimal(u64),
    BinaryNum(u64),
    List(Vec<Expression<'a>>),
    Object(Vec<Expression<'a>>),
    Paren(Box<Expression<'a>>),
    Closure((Vec<Expression<'a>>, Box<Expression<'a>>)),
    Function((Option<String>, Vec<Expression<'a>>, Vec<Statement<'a>>)),
    Generator((Option<String>, Vec<Expression<'a>>, Vec<Statement<'a>>)),
    Unary(&'a str, Box<Expression<'a>>),
    Binary(&'a str, Box<Expression<'a>>, Box<Expression<'a>>),
    Ternary(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),

    Args(Vec<Expression<'a>>),
    Splat(Box<Expression<'a>>),
    KeyValue((Box<Expression<'a>>, Box<Expression<'a>>)),
    Parameter((String, Option<Box<Expression<'a>>>)),
}

pub fn block<'a>(i: &'a str) -> ParseResult<'a, Vec<Statement<'a>>> {
    many(statement)(i)
}

fn statement<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    ws(choice((
        map(codeblock, Statement::Block::<'a>),
        map(if_block, Statement::If::<'a>),
        map(while_block, Statement::While::<'a>),
        map(for_block, Statement::For::<'a>),
        single_line_statements,
    )))(i)
}

fn single_line_statements<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    let end = choice((
        ws(eoi),
        ws(tag(";")),
        take_while(|c| c == '\n' || c == '\r'),
        value(ws(peek(tag("}"))), ""),
    ));
    left(
        choice((
            map(tag("continue"), |_| Statement::Continue::<'a>),
            map(tag("break"), |_| Statement::Break::<'a>),
            map(
                right(tag("return"), opt(expression)),
                Statement::Return::<'a>,
            ),
            map(right(tag("throw"), opt(expression)), Statement::Throw::<'a>),
            assignment,
        )),
        end,
    )(i)
}

fn assignment<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    choice((
        map(declaration, Statement::Declaration::<'a>),
        map(expression, Statement::Expression::<'a>),
    ))(i)
}

fn declaration<'a>(i: &'a str) -> ParseResult<(&str, Vec<Expression<'a>>)> {
    ws(pair(
        choice((tag("var"), tag("let"), tag("const"))),
        chain(ws(tag(",")), mutation),
    ))(i)
}

fn if_block<'a>(
    i: &'a str,
) -> ParseResult<(
    Box<Expression<'a>>,
    Box<Statement<'a>>,
    Option<Box<Statement<'a>>>,
)> {
    let else_block = ws(right(tag("else"), boxed(statement)));
    let inner = trio(paren, boxed(statement), opt(else_block));
    ws(right(tag("if"), inner))(i)
}

fn while_block<'a>(i: &'a str) -> ParseResult<(Box<Expression<'a>>, Box<Statement<'a>>)> {
    let inner = pair(paren, boxed(statement));
    ws(right(tag("while"), inner))(i)
}

fn for_block<'a>(
    i: &'a str,
) -> ParseResult<(
    (
        Option<Box<Statement<'a>>>,
        Option<Expression<'a>>,
        Option<Expression<'a>>,
    ),
    Box<Statement<'a>>,
)> {
    let trio = trio(
        opt(boxed(assignment)),
        right(ws(tag(";")), opt(expression)),
        right(ws(tag(";")), opt(expression)),
    );
    let inner = pair(middle(ws(tag("(")), trio, ws(tag(")"))), boxed(statement));
    ws(right(tag("for"), inner))(i)
}

pub fn expression<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        prefix(choice((tag("yield*"), tag("yield"))), mutation),
        makechain,
    )(i)
}

fn mutation<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        infix(
            ternary,
            choice((
                tag("="),
                tag("+="),
                tag("-="),
                tag("**="),
                tag("*="),
                tag("/="),
                tag("%="),
                tag("<<="),
                tag(">>>="),
                tag(">>="),
                tag("&="),
                tag("^="),
                tag("|="),
            )),
        ),
        makechain2,
    )(i)
}

fn ternary<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let conds = right(ws(tag("?")), outer(equality, ws(tag(":")), equality));
    ws(map(pair(equality, many(conds)), maketernary))(i)
}

fn equality<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        infix(
            comparison,
            choice((tag("==="), tag("=="), tag("!=="), tag("!="))),
        ),
        makechain2,
    )(i)
}

fn comparison<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        infix(
            bitwise,
            choice((
                tag(">="),
                tag("<="),
                tag(">"),
                tag("<"),
                tag("instanceof"),
                tag("in"),
            )),
        ),
        makechain2,
    )(i)
}

fn bitwise<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        infix(logic_or, choice((tag(">>>"), tag(">>"), tag("<<")))),
        makechain2,
    )(i)
}

fn logic_or<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(logic_and, tag("&&")), makechain2)(i)
}

fn logic_and<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(coalesce, tag("||")), makechain2)(i)
}

fn coalesce<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(bitwise_or, tag("??")), makechain2)(i)
}

fn bitwise_or<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(bitwise_xor, tag("|")), makechain2)(i)
}

fn bitwise_xor<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(bitwise_and, tag("^")), makechain2)(i)
}

fn bitwise_and<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(addition, tag("&")), makechain2)(i)
}

fn addition<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        infix(multiplication, choice((tag("+"), tag("-")))),
        makechain2,
    )(i)
}

fn multiplication<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        infix(power, choice((tag("*"), tag("/"), tag("%")))),
        makechain2,
    )(i)
}

fn power<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(negation, tag("**")), makechain2)(i)
}

fn negation<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(prefix(tag("!"), prefixes), makechain)(i)
}

fn prefixes<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        prefix(
            choice((
                tag("++"),
                tag("--"),
                tag("+"),
                tag("-"),
                tag("typeof"),
                tag("void"),
                tag("delete"),
                tag("await"),
            )),
            postfix,
        ),
        makechain,
    )(i)
}

fn postfix<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        pair(creation, many(ws(choice((tag("++"), tag("--")))))),
        makechainb,
    )(i)
}

fn creation<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(prefix(tag("new"), action), makechain)(i)
}

fn action<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(
        pair(
            primitive,
            many(ws(choice((
                pair(tag("["), left(expression, ws(tag("]")))),
                pair(tag("?."), map(ident, Expression::Ident::<'a>)),
                pair(tag("."), map(ident, Expression::Ident::<'a>)),
                pair(
                    tag("("),
                    left(map(arguments, Expression::Args::<'a>), ws(tag(")"))),
                ),
            )))),
        ),
        makechain2,
    )(i)
}

fn primitive<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    ws(choice((
        map(string('"'), Expression::Str::<'a>),
        map(string('\''), Expression::Str::<'a>),
        map(right(tag("0o"), number(8)), Expression::Octal::<'a>),
        map(right(tag("0x"), number(16)), Expression::Hexadecimal::<'a>),
        map(right(tag("0b"), number(2)), Expression::BinaryNum::<'a>),
        map(double, Expression::Double::<'a>),
        map(generator, Expression::Generator::<'a>),
        map(function, Expression::Function::<'a>),
        map(ident, Expression::Ident::<'a>),
        map(object, Expression::Object::<'a>),
        map(closure, Expression::Closure::<'a>),
        map(paren, Expression::Paren::<'a>),
        map(list, Expression::List::<'a>),
    )))(i)
}

fn ident(i: &str) -> ParseResult<String> {
    ws(map(
        check(take_while(|c| c.is_alphanumeric()), |s| {
            !RESERVED.contains(s)
        }),
        String::from,
    ))(i)
}

fn object<'a>(i: &'a str) -> ParseResult<Vec<Expression<'a>>> {
    middle(tag("{"), chain(ws(tag(",")), key_value), ws(tag("}")))(i)
}

fn key_value<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    choice((
        map(
            outer(
                ws(boxed(choice((
                    map(string('"'), Expression::Str::<'a>),
                    map(string('\''), Expression::Str::<'a>),
                    map(ident, Expression::Str::<'a>),
                    middle(tag("["), expression, tag("]")),
                )))),
                ws(tag(":")),
                boxed(expression),
            ),
            Expression::KeyValue::<'a>,
        ),
        map(ident, Expression::Ident::<'a>),
        splat,
    ))(i)
}

fn paren<'a>(i: &'a str) -> ParseResult<Box<Expression<'a>>> {
    ws(middle(tag("("), boxed(expression), ws(tag(")"))))(i)
}

fn list<'a>(i: &'a str) -> ParseResult<Vec<Expression<'a>>> {
    ws(middle(tag("["), arguments, ws(tag("]"))))(i)
}

fn closure<'a>(i: &'a str) -> ParseResult<(Vec<Expression<'a>>, Box<Expression<'a>>)> {
    ws(outer(parameters, ws(tag("=>")), boxed(expression)))(i)
}

fn function<'a>(
    i: &'a str,
) -> ParseResult<(Option<String>, Vec<Expression<'a>>, Vec<Statement<'a>>)> {
    let inner = trio(ws(opt(ident)), parameters, codeblock);
    ws(right(tag("function"), inner))(i)
}

fn generator<'a>(
    i: &'a str,
) -> ParseResult<(Option<String>, Vec<Expression<'a>>, Vec<Statement<'a>>)> {
    let inner = trio(ws(opt(ident)), parameters, codeblock);
    ws(right(tag("function*"), inner))(i)
}

fn codeblock<'a>(i: &'a str) -> ParseResult<Vec<Statement<'a>>> {
    ws(middle(tag("{"), block, ws(tag("}"))))(i)
}

fn parameters<'a>(i: &'a str) -> ParseResult<Vec<Expression<'a>>> {
    let param = map(
        pair(ident, opt(right(ws(tag("=")), boxed(ws(expression))))),
        Expression::Parameter::<'a>,
    );
    let inner = chain(ws(tag(",")), choice((splat, param)));
    ws(middle(tag("("), inner, ws(tag(")"))))(i)
}

fn arguments<'a>(i: &'a str) -> ParseResult<Vec<Expression<'a>>> {
    ws(chain(tag(","), choice((splat, expression))))(i)
}

fn splat<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    ws(map(
        boxed(right(tag("..."), expression)),
        Expression::Splat::<'a>,
    ))(i)
}

fn ws<'a, T>(item: impl Fn(&'a str) -> ParseResult<T>) -> impl Fn(&'a str) -> ParseResult<T> {
    right(whitespace, right(comments, item))
}

fn comments<'a>(i: &'a str) -> ParseResult<Vec<&'a str>> {
    many(right(
        tag("/"),
        choice((
            right(tag("/"), left(take_until("\n"), tag("\n"))),
            right(tag("*"), left(take_until("*/"), tag("*/"))),
        )),
    ))(i)
}

fn maketernary<'a>(e: (Expression<'a>, Vec<(Expression<'a>, Expression<'a>)>)) -> Expression<'a> {
    e.1.iter().fold(e.0, |a, (b, c)| {
        Expression::Ternary::<'a>(Box::new(a), Box::new(b.clone()), Box::new(c.clone()))
    })
}

fn makechain<'a>(e: (Vec<&'a str>, Expression<'a>)) -> Expression<'a> {
    e.0.iter()
        .fold(e.1, |acc, op| Expression::Unary::<'a>(*op, Box::new(acc)))
}

fn makechainb<'a>(e: (Expression<'a>, Vec<&'a str>)) -> Expression<'a> {
    e.1.iter()
        .fold(e.0, |acc, op| Expression::Unary::<'a>(*op, Box::new(acc)))
}

fn makechain2<'a>(e: (Expression<'a>, Vec<(&'a str, Expression<'a>)>)) -> Expression<'a> {
    e.1.iter().fold(e.0, |a, (op, b)| {
        Expression::Binary::<'a>(*op, Box::new(a), Box::new(b.clone()))
    })
}
