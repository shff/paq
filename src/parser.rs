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
    Function(
        (
            Option<Box<Expression<'a>>>,
            Vec<Expression<'a>>,
            Box<Statement<'a>>,
        ),
    ),
    Generator(
        (
            Option<Box<Expression<'a>>>,
            Vec<Expression<'a>>,
            Box<Statement<'a>>,
        ),
    ),
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
    Param((Box<Expression<'a>>, Option<Box<Expression<'a>>>)),
}

pub fn block<'a>(i: &'a str) -> ParseResult<'a, Vec<Statement<'a>>> {
    many(statement)(i)
}

fn statement<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    ws(choice((braces, condition, while_loop, for_loop, gotos)))(i)
}

fn gotos<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
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
    choice((declaration, standalone))(i)
}

fn declaration<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    let ops = &["var", "let", "const"];
    let declaration = ws(pair(one_of(ops), chain(ws(tag(",")), mutation)));
    map(declaration, Statement::Declaration::<'a>)(i)
}

fn condition<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    let else_block = ws(right(tag("else"), boxed(statement)));
    let inner = trio(paren, boxed(statement), opt(else_block));
    map(ws(right(tag("if"), inner)), Statement::If)(i)
}

fn while_loop<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    let inner = pair(paren, boxed(statement));
    map(ws(right(tag("while"), inner)), Statement::While)(i)
}

fn for_loop<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    let assign = opt(boxed(assignment));
    let expr1 = right(ws(tag(";")), opt(expression));
    let expr2 = right(ws(tag(";")), opt(expression));
    let trio = trio(assign, expr1, expr2);
    let inner = pair(middle(ws(tag("(")), trio, ws(tag(")"))), boxed(statement));
    map(ws(right(tag("for"), inner)), Statement::For)(i)
}

pub fn standalone<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    ws(map(expression, Statement::Expression::<'a>))(i)
}

pub fn expression<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(prefix(one_of(&["yield*", "yield"]), mutation), makechain)(i)
}

fn mutation<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let ops = &[
        "=", "+=", "-=", "**=", "*=", "/=", "%=", "<<=", ">>>=", ">>=", "&=", "^=", "|=",
    ];
    map(infix(ternary, one_of(ops)), makechain2)(i)
}

fn ternary<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let conds = right(ws(tag("?")), outer(equality, ws(tag(":")), equality));
    ws(map(pair(equality, many(conds)), maketernary))(i)
}

fn equality<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let ops = &["===", "==", "!==", "!="];
    map(infix(comparison, one_of(ops)), makechain2)(i)
}

fn comparison<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let ops = &[">=", "<=", ">", "<", "instanceof", "in"];
    map(infix(bitwise, one_of(ops)), makechain2)(i)
}

fn bitwise<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let ops = &[">>>", ">>", "<<"];
    map(infix(logic_or, one_of(ops)), makechain2)(i)
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
    map(infix(multiplication, one_of(&["+", "-"])), makechain2)(i)
}

fn multiplication<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(power, one_of(&["*", "/", "%"])), makechain2)(i)
}

fn power<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(infix(negation, tag("**")), makechain2)(i)
}

fn negation<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(prefix(tag("!"), prefixes), makechain)(i)
}

fn prefixes<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let ops = &["++", "--", "+", "-", "typeof", "void", "delete", "await"];
    map(prefix(one_of(ops), postfix), makechain)(i)
}

fn postfix<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let ops = &["++", "--"];
    map(pair(creation, many(ws(one_of(ops)))), makechainb)(i)
}

fn creation<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    map(prefix(tag("new"), action), makechain)(i)
}

fn action<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let array = pair(tag("["), left(expression, ws(tag("]"))));
    let elvis = pair(tag("?."), ident);
    let dot = pair(tag("."), ident);
    let args = map(args, Expression::Args::<'a>);
    let call = pair(tag("("), left(args, ws(tag(")"))));
    let action = pair(primitive, many(ws(choice((array, elvis, dot, call)))));
    map(action, makechain2)(i)
}

fn primitive<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    ws(choice((
        map(string('"'), Expression::Str::<'a>),
        map(string('\''), Expression::Str::<'a>),
        map(right(tag("0o"), number(8)), Expression::Octal::<'a>),
        map(right(tag("0x"), number(16)), Expression::Hexadecimal::<'a>),
        map(right(tag("0b"), number(2)), Expression::BinaryNum::<'a>),
        map(double, Expression::Double::<'a>),
        generator,
        function,
        ident,
        object,
        map(closure, Expression::Closure::<'a>),
        map(paren, Expression::Paren::<'a>),
        map(list, Expression::List::<'a>),
    )))(i)
}

fn ident<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let words = take_while(|c| c.is_alphanumeric());
    let ident = check(words, |s| !RESERVED.contains(s));
    ws(map(map(ident, String::from), Expression::Ident::<'a>))(i)
}

fn object<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let object = middle(tag("{"), chain(ws(tag(",")), key_value), ws(tag("}")));
    ws(map(object, Expression::Object::<'a>))(i)
}

fn paren<'a>(i: &'a str) -> ParseResult<Box<Expression<'a>>> {
    ws(middle(tag("("), boxed(expression), ws(tag(")"))))(i)
}

fn list<'a>(i: &'a str) -> ParseResult<Vec<Expression<'a>>> {
    ws(middle(tag("["), args, ws(tag("]"))))(i)
}

fn closure<'a>(i: &'a str) -> ParseResult<(Vec<Expression<'a>>, Box<Expression<'a>>)> {
    ws(outer(params, ws(tag("=>")), boxed(expression)))(i)
}

fn function<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let inner = trio(ws(opt(boxed(ident))), params, boxed(braces));
    let func = ws(right(tag("function"), inner));
    map(func, Expression::Function::<'a>)(i)
}

fn generator<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let inner = trio(ws(opt(boxed(ident))), params, boxed(braces));
    let func = ws(right(tag("function*"), inner));
    map(func, Expression::Generator::<'a>)(i)
}

fn braces<'a>(i: &'a str) -> ParseResult<Statement<'a>> {
    map(ws(middle(tag("{"), block, ws(tag("}")))), Statement::Block)(i)
}

fn params<'a>(i: &'a str) -> ParseResult<Vec<Expression<'a>>> {
    let value = opt(right(ws(tag("=")), boxed(ws(expression))));
    let param = pair(boxed(ident), value);
    let exp = map(param, Expression::Param::<'a>);
    let inner = chain(ws(tag(",")), choice((splat, exp)));
    ws(middle(tag("("), inner, ws(tag(")"))))(i)
}

fn args<'a>(i: &'a str) -> ParseResult<Vec<Expression<'a>>> {
    ws(chain(tag(","), choice((splat, expression))))(i)
}

fn splat<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    let exp = boxed(right(tag("..."), expression));
    ws(map(exp, Expression::Splat::<'a>))(i)
}

fn comments<'a>(i: &'a str) -> ParseResult<Vec<&'a str>> {
    let single = right(tag("/"), left(take_until("\n"), tag("\n")));
    let multi = right(tag("*"), left(take_until("*/"), tag("*/")));
    many(right(tag("/"), choice((single, multi))))(i)
}

fn ws<'a, T>(item: impl Fn(&'a str) -> ParseResult<T>) -> impl Fn(&'a str) -> ParseResult<T> {
    right(whitespace, right(comments, item))
}

fn key_value<'a>(i: &'a str) -> ParseResult<Expression<'a>> {
    choice((
        map(
            outer(
                ws(boxed(choice((
                    map(string('"'), Expression::Str::<'a>),
                    map(string('\''), Expression::Str::<'a>),
                    ident,
                    middle(tag("["), expression, tag("]")),
                )))),
                ws(tag(":")),
                boxed(expression),
            ),
            Expression::KeyValue::<'a>,
        ),
        ident,
        splat,
    ))(i)
}

// Utilities

pub type ParseResult<'a, T> = Result<(&'a str, T), (&'a str, ParserError)>;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Check,
    Choice,
    Eof,
    Tag,
    TakeWhile,
    MapRes,
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

pub fn tag(tag: &'static str) -> impl Fn(&str) -> ParseResult<&str> {
    move |i| match i.starts_with(tag) {
        true => Ok((&i[tag.len()..], &i[..tag.len()])),
        false => Err((i, ParserError::Tag)),
    }
}

pub fn chr(c: char) -> impl Fn(&str) -> ParseResult<&str> {
    move |i| match i.starts_with(c) {
        true => Ok((&i[1..], &i[..1])),
        false => Err((i, ParserError::Tag)),
    }
}

pub fn value<'a, P, R, V>(p: P, v: V) -> impl Fn(&'a str) -> ParseResult<V>
where
    P: Fn(&'a str) -> ParseResult<R>,
    V: Copy,
{
    move |i| p(i).map(|(i, _)| (i, v))
}

pub fn map<'a, P, F, A, B>(p: P, f: F) -> impl Fn(&'a str) -> ParseResult<B>
where
    P: Fn(&'a str) -> ParseResult<A>,
    F: Fn(A) -> B,
{
    move |i| p(i).map(|(i, r)| (i, f(r)))
}

pub fn mapr<'a, P, F, A, B, E>(p: P, f: F) -> impl Fn(&'a str) -> ParseResult<B>
where
    P: Fn(&'a str) -> ParseResult<A>,
    F: Fn(A) -> Result<B, E>,
{
    move |i| p(i).and_then(|(i, r)| f(r).map(|r| (i, r)).or(Err((i, ParserError::MapRes))))
}

pub fn opt<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<Option<R>>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| p(i).map(|(i, r)| (i, Some(r))).or(Ok((i, None)))
}

pub fn pair<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<(X, Y)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, r1)| b(i).map(|(i, r2)| (i, (r1, r2))))
}

pub fn trio<'a, A, B, C, X, Y, Z>(a: A, b: B, c: C) -> impl Fn(&'a str) -> ParseResult<(X, Y, Z)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
    C: Fn(&'a str) -> ParseResult<Z>,
{
    move |i| a(i).and_then(|(i, x)| b(i).and_then(|(i, y)| c(i).map(|(i, z)| (i, (x, y, z)))))
}

pub fn right<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<Y>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, _)| b(i).map(|(i, r2)| (i, r2)))
}

pub fn left<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<X>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, r1)| b(i).map(|(i, _)| (i, r1)))
}

pub fn middle<'a, A, B, C, X, Y, Z>(a: A, b: B, c: C) -> impl Fn(&'a str) -> ParseResult<Y>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
    C: Fn(&'a str) -> ParseResult<Z>,
{
    move |i| a(i).and_then(|(i, _)| b(i).and_then(|(i, r2)| c(i).map(|(i, _)| (i, r2))))
}

pub fn outer<'a, A, B, C, X, Y, Z>(a: A, b: B, c: C) -> impl Fn(&'a str) -> ParseResult<(X, Z)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
    C: Fn(&'a str) -> ParseResult<Z>,
{
    move |i| a(i).and_then(|(i, x)| b(i).and_then(|(i, _)| c(i).map(|(i, z)| (i, (x, z)))))
}

pub fn one_of<'a>(opts: &'a [&str]) -> impl Fn(&'a str) -> ParseResult<&str> {
    move |i| {
        for opt in opts {
            if i.starts_with(opt) {
                return Ok((&i[opt.len()..], &i[..opt.len()]));
            }
        }
        Err((i, ParserError::Choice))
    }
}

pub fn choice<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<R>
where
    P: Choice<'a, R>,
{
    move |i| p.choice(i)
}

pub fn take_while<'a, P>(p: P) -> impl Fn(&'a str) -> ParseResult<&str>
where
    P: Copy + Fn(char) -> bool,
{
    move |i| match i.find(|c| !p(c)) {
        Some(x) if x > 0 => Ok((&i[x..], &i[..x])),
        None if !i.is_empty() => Ok((&i[i.len()..], i)),
        _ => Err((i, ParserError::TakeWhile)),
    }
}

pub fn take_until(p: &'static str) -> impl Fn(&str) -> ParseResult<&str> {
    move |i| i.find(p).map_or(Ok((i, "")), |x| Ok((&i[x..], &i[..x])))
}

pub fn peek<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<R>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| p(i).map(|(_, o)| (i, o))
}

pub fn capture<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<&'a str>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| p(i).map(|(i2, _)| (i2, &i[..(i2.as_ptr() as usize - i.as_ptr() as usize)]))
}

pub fn check<'a, P, R, F>(p: P, f: F) -> impl Fn(&'a str) -> ParseResult<R>
where
    P: Fn(&'a str) -> ParseResult<R>,
    F: Fn(&R) -> bool,
{
    move |i| match p(i) {
        Ok((i, r)) if f(&r) => Ok((i, r)),
        _ => Err((i, ParserError::Check)),
    }
}

pub fn many<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<Vec<R>>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |mut i| {
        let mut r = Vec::new();
        while let Ok((next_input, next_item)) = p(i) {
            i = next_input;
            r.push(next_item);
        }
        Ok((i, r))
    }
}

pub fn chain<'a, S, P, R1, R2>(sep: S, p: P) -> impl Fn(&'a str) -> ParseResult<Vec<R2>>
where
    S: Fn(&'a str) -> ParseResult<R1>,
    P: Fn(&'a str) -> ParseResult<R2>,
    R1: Clone,
    R2: Clone,
{
    move |i| {
        p(i).map(|(i, a)| {
            let mut res = vec![a];
            let mut i = &(*i);
            while let Ok((next_input, next_item)) = right(&sep, &p)(i) {
                i = next_input;
                res.push(next_item);
            }
            if let Ok((new_i, _)) = opt(&sep)(i) {
                i = new_i;
            }
            (i, res)
        })
        .or_else(|_| Ok((i, vec![])))
    }
}

pub fn infix<'a, P, O, R, S>(p: P, o: O) -> impl Fn(&'a str) -> ParseResult<(R, Vec<(S, R)>)>
where
    P: Fn(&'a str) -> ParseResult<R>,
    O: Fn(&'a str) -> ParseResult<S>,
{
    move |i| pair(&p, many(pair(w(&o), &p)))(i)
}

pub fn prefix<'a, P, Q, X, Y>(p: P, q: Q) -> impl Fn(&'a str) -> ParseResult<(Vec<X>, Y)>
where
    P: Fn(&'a str) -> ParseResult<X>,
    Q: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| pair(many(w(&p)), &q)(i)
}

pub fn boxed<'a, P, R>(i: P) -> impl Fn(&'a str) -> ParseResult<Box<R>>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    map(i, Box::new)
}

pub fn string<'a>(q: char) -> impl Fn(&'a str) -> ParseResult<String> {
    move |i| {
        let escaped = right(
            tag("\\"),
            choice((
                value(tag("n"), "\n"),
                value(tag("r"), "\r"),
                value(tag("t"), "\t"),
                value(tag("v"), "\u{0B}"),
                value(tag("b"), "\u{08}"),
                value(tag("f"), "\u{0C}"),
                value(tag("\\"), "\\"),
                value(tag("/"), "/"),
                value(tag("\""), "\""),
                value(tag("\'"), "\'"),
                value(whitespace, ""),
            )),
        );
        let chars = take_while(|c| c != q && c != '\\');
        let inner = map(many(choice((chars, escaped))), |s| s.join(""));
        middle(chr(q), inner, chr(q))(i)
    }
}

pub fn number<'a>(b: u32) -> impl Fn(&'a str) -> ParseResult<u64> {
    move |i| mapr(take_while(|c| c.is_digit(b)), |s| u64::from_str_radix(s, b))(i)
}

pub fn double(i: &str) -> ParseResult<f64> {
    let digit = |i| take_while(|c| c.is_numeric())(i);
    let sign = |i| opt(one_of(&["+", "-"]))(i);
    let num = value(pair(digit, opt(pair(tag("."), opt(digit)))), 0);
    let frac = value(pair(tag("."), digit), 0);
    let exp = opt(trio(choice((tag("e"), tag("E"))), sign, digit));
    mapr(capture(trio(sign, choice((num, frac)), exp)), |s| s.parse())(i)
}

pub fn eoi(i: &str) -> ParseResult<&str> {
    match i.is_empty() {
        true => Ok((i, "")),
        false => Err((i, ParserError::Eof)),
    }
}

pub fn whitespace(i: &str) -> ParseResult<&str> {
    match i.find(|c: char| !c.is_whitespace()) {
        Some(x) => Ok((&i[x..], &i[..x])),
        _ => Ok(("", i)),
    }
}

pub fn w<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<R>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    right(whitespace, p)
}

pub trait Choice<'a, O> {
    fn choice(&self, i: &'a str) -> ParseResult<'a, O>;
}
macro_rules! choice(
    ($($id:ident)+ , $($num:tt)+) => (
        impl<'a, O, $($id: Fn(&'a str) -> ParseResult<'a, O>),+>
            Choice<'a, O> for ( $($id),+ ) {
            fn choice(&self, i: &'a str) -> ParseResult<'a, O> {
                Err(("", ""))$(.or_else(|_| self.$num(i)))*
            }
        }
    );
);
choice!(A B, 0 1);
choice!(A B C, 0 1 2);
choice!(A B C D, 0 1 2 3);
choice!(A B C D E, 0 1 2 3 4);
choice!(A B C D E F, 0 1 2 3 4 5);
choice!(A B C D E F G, 0 1 2 3 4 5 6);
choice!(A B C D E F G H, 0 1 2 3 4 5 6 7);
choice!(A B C D E F G H I, 0 1 2 3 4 5 6 7 8);
choice!(A B C D E F G H I J, 0 1 2 3 4 5 6 7 8 9);
choice!(A B C D E F G H I J K, 0 1 2 3 4 5 6 7 8 9 10);
choice!(A B C D E F G H I J K L, 0 1 2 3 4 5 6 7 8 9 10 11);
choice!(A B C D E F G H I J K L M, 0 1 2 3 4 5 6 7 8 9 10 11 12);
