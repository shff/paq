const RESERVED: &[&str] = &["const", "for"];

#[derive(Clone, Debug, PartialEq)]
pub enum Node<'a> {
    Block(Vec<Node<'a>>),
    If((Box<Node<'a>>, Box<Node<'a>>, Option<Box<Node<'a>>>)),
    While((Box<Node<'a>>, Box<Node<'a>>)),
    For((Box<Node<'a>>, Box<Node<'a>>)),
    Declaration((&'a str, Vec<Node<'a>>)),
    Return(Option<Box<Node<'a>>>),
    Throw(Box<Node<'a>>),
    Continue,
    Break,
    Blank,

    Str(String),
    Interpolation(String),
    Ident(String),
    Double(f64),
    Octal(u64),
    Hexadecimal(u64),
    BinaryNum(u64),
    Regex((&'a str, Option<&'a str>)),
    List(Vec<Option<Node<'a>>>),
    Object(Vec<Node<'a>>),
    Paren(Box<Node<'a>>),
    Closure((Box<Node<'a>>, Box<Node<'a>>)),
    Function((Option<Box<Node<'a>>>, Box<Node<'a>>, Box<Node<'a>>)),
    Shorthand((Box<Node<'a>>, Box<Node<'a>>, Box<Node<'a>>)),
    Generator((Option<Box<Node<'a>>>, Box<Node<'a>>, Box<Node<'a>>)),
    Unary(&'a str, Box<Node<'a>>),
    Binary(&'a str, Box<Node<'a>>, Box<Node<'a>>),
    Ternary(Box<Node<'a>>, Box<Node<'a>>, Box<Node<'a>>),
    Import((Option<Box<Node<'a>>>, Box<Node<'a>>)),

    Args(Vec<Node<'a>>),
    Splat(Box<Node<'a>>),
    KeyValue((Box<Node<'a>>, Box<Node<'a>>)),
    Params(Vec<Node<'a>>),
    Param((Box<Node<'a>>, Option<Box<Node<'a>>>)),
    ForTrio(Vec<Option<Node<'a>>>),
    ForOf((Box<Node<'a>>, Box<Node<'a>>)),
    ForIn((Box<Node<'a>>, Box<Node<'a>>)),
    Variable((&'a str, Box<Node<'a>>)),
}

pub fn block(i: &str) -> ParseResult<Node> {
    ws(map(many(statement), Node::Block))(i)
}

fn statement<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    ws(choice((
        braces, condition, while_loop, for_loop, gotos, function,
    )))(i)
}

fn gotos<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let eol = take_while(|c| c == '\n' || c == '\r');
    let brace = value(ws(peek(tag("}"))), "");
    let end = choice((ws(eoi), ws(tag(";")), eol, brace));

    let empty = map(peek(tag(";")), |_| Node::Blank);
    let cont = value(tag("continue"), Node::Continue);
    let brk = value(tag("break"), Node::Break);
    let ret = map(right(tag("return"), opt(boxed(expression))), Node::Return);
    let thrw = map(right(tag("throw"), boxed(expression)), Node::Throw);
    left(
        choice((
            imports,
            cont,
            brk,
            ret,
            thrw,
            declaration,
            expression,
            empty,
        )),
        end,
    )(i)
}

fn imports<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let from = opt(left(boxed(ws(ident)), ws(tag("from"))));
    map(
        right(ws(tag("import")), pair(from, boxed(ws(quote)))),
        Node::Import,
    )(i)
}

fn declaration<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &["var", "let", "const"];
    let declaration = ws(pair(one_of(ops), chain(ws(tag(",")), mutation)));
    map(declaration, Node::Declaration)(i)
}

fn condition<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let else_block = ws(right(tag("else"), boxed(statement)));
    let inner = trio(boxed(paren), boxed(statement), opt(else_block));
    map(ws(right(tag("if"), inner)), Node::If)(i)
}

fn while_loop<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let inner = pair(boxed(paren), boxed(statement));
    map(ws(right(tag("while"), inner)), Node::While)(i)
}

fn for_loop<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let iter = boxed(choice((for_of, for_in, for_trio)));
    let inner = pair(middle(ws(tag("(")), iter, ws(tag(")"))), boxed(statement));
    map(ws(right(tag("for"), inner)), Node::For)(i)
}

fn for_trio<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let expr1 = opt(choice((declaration, expression)));
    let expr2 = right(ws(tag(";")), opt(expression));
    let expr3 = right(ws(tag(";")), opt(expression));
    map(trio(expr1, expr2, expr3), |(a, b, c)| {
        Node::ForTrio(vec![a, b, c])
    })(i)
}

fn for_of<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let expr1 = boxed(choice((variable, ident)));
    let expr2 = boxed(expression);
    map(outer(expr1, ws(tag("of")), expr2), Node::ForOf)(i)
}

fn for_in<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let expr1 = boxed(choice((variable, ident)));
    let expr2 = boxed(expression);
    map(outer(expr1, ws(tag("in")), expr2), Node::ForIn)(i)
}

fn variable<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &["var", "let", "const"];
    map(ws(pair(one_of(ops), boxed(ident))), Node::Variable)(i)
}

pub fn expression<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(yield1, tag(",")), makechain2)(i)
}

fn yield1<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let yield_star = value(pair(tag("yield"), ws(tag("*"))), "yield*");
    map(prefix(yield_star, yield2), makechain)(i)
}

fn yield2<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(prefix(tag("yield"), mutation), makechain)(i)
}

fn mutation<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &[
        "=", "+=", "-=", "**=", "*=", "/=", "%=", "<<=", ">>>=", ">>=", "&=", "^=", "|=",
    ];
    map(infix(ternary, one_of(ops)), makechain2)(i)
}

fn ternary<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let conds = right(ws(tag("?")), outer(equality, ws(tag(":")), equality));
    ws(map(pair(equality, many(conds)), maketernary))(i)
}

fn equality<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &["===", "==", "!==", "!="];
    map(infix(comparison, one_of(ops)), makechain2)(i)
}

fn comparison<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &[">=", "<=", ">", "<", "instanceof", "in"];
    map(infix(bitwise, one_of(ops)), makechain2)(i)
}

fn bitwise<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &[">>>", ">>", "<<"];
    map(infix(logic_or, one_of(ops)), makechain2)(i)
}

fn logic_or<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(logic_and, tag("&&")), makechain2)(i)
}

fn logic_and<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(coalesce, tag("||")), makechain2)(i)
}

fn coalesce<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(bitwise_or, tag("??")), makechain2)(i)
}

fn bitwise_or<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(bitwise_xor, tag("|")), makechain2)(i)
}

fn bitwise_xor<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(bitwise_and, tag("^")), makechain2)(i)
}

fn bitwise_and<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(addition, tag("&")), makechain2)(i)
}

fn addition<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(multiplication, one_of(&["+", "-"])), makechain2)(i)
}

fn multiplication<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(power, one_of(&["*", "/", "%"])), makechain2)(i)
}

fn power<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(infix(negation, tag("**")), makechain2)(i)
}

fn negation<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &["!", "~"];
    map(prefix(one_of(ops), prefixes), makechain)(i)
}

fn prefixes<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &["++", "--", "+", "-", "typeof", "void", "delete", "await"];
    map(prefix(one_of(ops), postfix), makechain)(i)
}

fn postfix<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let ops = &["++", "--"];
    map(pair(creation, many(ws(one_of(ops)))), makechainb)(i)
}

fn creation<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    map(prefix(tag("new"), action), makechain)(i)
}

fn action<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let array = pair(tag("["), left(expression, ws(tag("]"))));
    let opt = pair(tag("?."), ident);
    let dot = pair(tag("."), ident);
    let call = pair(tag("("), left(args, ws(tag(")"))));
    let ea = pair(tag("?.["), left(expression, ws(tag("]"))));
    let ec = pair(tag("?.("), left(args, ws(tag(")"))));
    let action = pair(primitive, many(ws(choice((array, opt, dot, call, ea, ec)))));
    map(action, makechain2)(i)
}

fn primitive<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let octal = map(
        right(choice((tag("0o"), tag("0O"))), number(8)),
        Node::Octal,
    );
    let hexa = map(
        right(choice((tag("0x"), tag("0X"))), number(16)),
        Node::Hexadecimal,
    );
    let binary = map(
        right(choice((tag("0b"), tag("0B"))), number(2)),
        Node::BinaryNum,
    );
    let double = map(double, Node::Double);
    let intepolate = map(string('`'), Node::Interpolation);
    ws(choice((
        quote, octal, hexa, binary, double, generator, function, object, closure, paren, list,
        regex, ident, intepolate,
    )))(i)
}

fn regex<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let inner = pair(capture(string('/')), opt(take_while(|c| c.is_alphabetic())));
    ws(map(inner, Node::Regex))(i)
}

fn quote<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let single_quote = map(string('"'), Node::Str);
    let double_quote = map(string('\''), Node::Str);
    ws(choice((single_quote, double_quote)))(i)
}

fn ident<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let words = take_while(|c| c.is_alphanumeric());
    let ident = reserved(words, RESERVED);
    ws(map(map(ident, String::from), Node::Ident))(i)
}

fn object<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let item = choice((shorthand, key_value, ident, splat));
    let object = middle(tag("{"), chain(ws(tag(",")), item), ws(tag("}")));
    ws(map(object, Node::Object))(i)
}

fn shorthand<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let computed = middle(tag("["), expression, tag("]"));
    let title = choice((quote, ident, computed));
    let inner = trio(ws(boxed(title)), boxed(params), boxed(braces));
    map(inner, Node::Shorthand)(i)
}

fn paren<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let paren = middle(tag("("), boxed(expression), ws(tag(")")));
    ws(map(paren, Node::Paren))(i)
}

fn list<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let items = chain(tag(","), ws(opt(choice((splat, yield1)))));
    let list = middle(tag("["), items, ws(tag("]")));
    ws(map(list, Node::List))(i)
}

fn closure<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let params = ws(choice((ident, params)));
    let closure = outer(boxed(params), ws(tag("=>")), boxed(expression));
    ws(map(closure, Node::Closure))(i)
}

fn function<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let inner = trio(ws(opt(boxed(ident))), boxed(params), boxed(braces));
    let func = ws(right(tag("function"), inner));
    map(func, Node::Function)(i)
}

fn generator<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let inner = trio(ws(opt(boxed(ident))), boxed(params), boxed(braces));
    let func = ws(right(pair(tag("function"), ws(tag("*"))), inner));
    map(func, Node::Generator)(i)
}

fn braces<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    ws(middle(tag("{"), block, ws(tag("}"))))(i)
}

fn params<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let value = opt(right(ws(tag("=")), boxed(ws(expression))));
    let param = pair(boxed(ident), value);
    let exp = map(param, Node::Param);
    let inner = chain(ws(tag(",")), choice((splat, exp)));
    let params = middle(tag("("), inner, ws(tag(")")));
    ws(map(params, Node::Params))(i)
}

fn args<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let args = chain(tag(","), choice((splat, expression)));
    ws(map(args, Node::Args))(i)
}

fn splat<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let exp = boxed(right(tag("..."), yield1));
    ws(map(exp, Node::Splat))(i)
}

fn comments<'a>(i: &'a str) -> ParseResult<Vec<&'a str>> {
    let single = right(tag("/"), left(take_until("\n"), tag("\n")));
    let multi = right(tag("*"), left(take_until("*/"), tag("*/")));
    many(right(tag("/"), choice((single, multi))))(i)
}

fn key_value<'a>(i: &'a str) -> ParseResult<Node<'a>> {
    let double_quote = map(string('"'), Node::Str);
    let single_quote = map(string('\''), Node::Str);
    let computed = middle(tag("["), expression, tag("]"));
    let key = ws(boxed(choice((double_quote, single_quote, ident, computed))));
    let value = boxed(yield1);
    map(outer(key, ws(tag(":")), value), Node::KeyValue)(i)
}

// Tree walking

pub fn transform(root: Node) -> Node {
    walk(root, |child| {
        if let Node::Import((None, path)) = child {
            if let Node::Str(path) = *path {
                return Some(Node::Binary(
                    "(",
                    Box::new(Node::Ident(String::from("require"))),
                    Box::new(Node::Args(vec![Node::Str(path)])),
                ));
            }
        }
        None
    })
}

pub fn get_deps(root: Node) -> Vec<String> {
    let deps = std::cell::RefCell::new(Vec::new());
    walk(root, |child| {
        if let Node::Binary("(", call, args) = child {
            if let Node::Ident(func_name) = *call.clone() {
                if func_name.as_str() == "require" {
                    if let Node::Args(s) = *args {
                        if let Some(Node::Str(dep)) = s.first() {
                            deps.borrow_mut().push(dep.clone())
                        }
                    }
                }
            }
        };
        None
    });
    deps.replace(vec![])
}

fn walk<V>(node: Node, mut visit: V) -> Node
where
    V: Copy + FnMut(Node) -> Option<Node>,
{
    if let Some(ret) = visit(node.clone()) {
        return ret;
    }
    match node.clone() {
        Node::Continue => Node::Continue,
        Node::Break => Node::Break,
        Node::Return(None) => Node::Return(None),
        Node::Str(a) => Node::Str(a),
        Node::Interpolation(a) => Node::Interpolation(a),
        Node::Regex(a) => Node::Regex(a),
        Node::Ident(a) => Node::Ident(a),
        Node::Double(a) => Node::Double(a),
        Node::Octal(a) => Node::Octal(a),
        Node::Hexadecimal(a) => Node::Hexadecimal(a),
        Node::BinaryNum(a) => Node::BinaryNum(a),
        Node::Import(a) => Node::Import(a),
        Node::Return(Some(a)) => Node::Return(Some(Box::new(walk(*a.clone(), visit)))),
        Node::Throw(a) => Node::Throw(Box::new(walk(*a.clone(), visit))),
        Node::Paren(a) => Node::Paren(Box::new(walk(*a.clone(), visit))),
        Node::Splat(a) => Node::Splat(Box::new(walk(*a.clone(), visit))),
        Node::Unary(a, b) => Node::Unary(a, Box::new(walk(*b.clone(), visit))),
        Node::Param((a, None)) => Node::Param((Box::new(walk(*a.clone(), visit)), None)),
        Node::Variable((a, b)) => Node::Variable((a, Box::new(walk(*b.clone(), visit)))),
        Node::Block(a) => Node::Block(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::List(a) => Node::List(
            a.iter()
                .map(|n| n.as_ref().map(|m| walk(m.clone(), visit)))
                .collect(),
        ),
        Node::Object(a) => Node::Object(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::Args(a) => Node::Args(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::Params(a) => Node::Params(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::Declaration((a, b)) => {
            Node::Declaration((a, b.iter().map(|n| walk(n.clone(), visit)).collect()))
        }
        Node::While((a, b)) => Node::While((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::For((a, b)) => Node::For((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::ForOf((a, b)) => Node::ForOf((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::ForIn((a, b)) => Node::ForIn((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Closure((a, b)) => Node::Closure((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::KeyValue((a, b)) => Node::KeyValue((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Param((a, Some(b))) => Node::Param((
            Box::new(walk(*a.clone(), visit)),
            Some(Box::new(walk(*b.clone(), visit))),
        )),
        Node::Function((None, a, b)) => Node::Function((
            None,
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Generator((None, a, b)) => Node::Generator((
            None,
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::If((a, b, None)) => Node::If((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
            None,
        )),
        Node::Binary(a, b, c) => Node::Binary(
            a,
            Box::new(walk(*b.clone(), visit)),
            Box::new(walk(*c.clone(), visit)),
        ),
        Node::Ternary(a, b, c) => Node::Ternary(
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
            Box::new(walk(*c.clone(), visit)),
        ),
        Node::Shorthand((a, b, c)) => Node::Shorthand((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
            Box::new(walk(*c.clone(), visit)),
        )),
        Node::If((a, b, Some(c))) => Node::If((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
            Some(Box::new(walk(*c.clone(), visit))),
        )),
        Node::Function((Some(a), b, c)) => Node::Function((
            Some(Box::new(walk(*a.clone(), visit))),
            Box::new(walk(*b.clone(), visit)),
            Box::new(walk(*c.clone(), visit)),
        )),
        Node::Generator((Some(a), b, c)) => Node::Generator((
            Some(Box::new(walk(*a.clone(), visit))),
            Box::new(walk(*b.clone(), visit)),
            Box::new(walk(*c.clone(), visit)),
        )),
        Node::ForTrio(_) => node,
        Node::Blank => node,
    }
}

// Utilities

pub type ParseResult<'a, T> = Result<(&'a str, T), (&'a str, ParserError)>;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Reserved(String),
    Choice,
    Eof,
    Tag(String),
    TakeWhile,
    MapRes,
}

fn maketernary<'a>(e: (Node<'a>, Vec<(Node<'a>, Node<'a>)>)) -> Node<'a> {
    e.1.iter().fold(e.0, |a, (b, c)| {
        Node::Ternary::<'a>(Box::new(a), Box::new(b.clone()), Box::new(c.clone()))
    })
}

fn makechain<'a>(e: (Vec<&'a str>, Node<'a>)) -> Node<'a> {
    e.0.iter()
        .fold(e.1, |acc, op| Node::Unary::<'a>(*op, Box::new(acc)))
}

fn makechainb<'a>(e: (Node<'a>, Vec<&'a str>)) -> Node<'a> {
    e.1.iter()
        .fold(e.0, |acc, op| Node::Unary::<'a>(*op, Box::new(acc)))
}

fn makechain2<'a>(e: (Node<'a>, Vec<(&'a str, Node<'a>)>)) -> Node<'a> {
    e.1.iter().fold(e.0, |a, (op, b)| {
        Node::Binary::<'a>(*op, Box::new(a), Box::new(b.clone()))
    })
}

pub fn tag(tag: &'static str) -> impl Fn(&str) -> ParseResult<&str> {
    move |i| {
        if i.starts_with(tag) {
            Ok((&i[tag.len()..], &i[..tag.len()]))
        } else {
            Err((i, ParserError::Tag(tag.to_string())))
        }
    }
}

pub fn chr(c: char) -> impl Fn(&str) -> ParseResult<&str> {
    move |i| {
        if i.starts_with(c) {
            Ok((&i[1..], &i[..1]))
        } else {
            Err((i, ParserError::Tag(c.to_string())))
        }
    }
}

pub fn value<'a, P, R, V>(p: P, v: V) -> impl Fn(&'a str) -> ParseResult<V>
where
    P: Fn(&'a str) -> ParseResult<R>,
    V: Clone,
{
    move |i| p(i).map(|(i, _)| (i, v.clone()))
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

pub fn reserved<'a, P>(p: P, words: &'a [&'a str]) -> impl Fn(&'a str) -> ParseResult<&'a str>
where
    P: Fn(&'a str) -> ParseResult<&'a str>,
{
    move |i| match p(i) {
        Ok((i, r)) if !words.contains(&r) => Ok((i, r)),
        Ok((_, r)) => Err((i, ParserError::Reserved(r.to_string()))),
        Err((i, r)) => Err((i, r)),
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
    move |i| pair(&p, many(pair(ws(&o), &p)))(i)
}

pub fn prefix<'a, P, Q, X, Y>(p: P, q: Q) -> impl Fn(&'a str) -> ParseResult<(Vec<X>, Y)>
where
    P: Fn(&'a str) -> ParseResult<X>,
    Q: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| pair(many(ws(&p)), &q)(i)
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

pub const fn eoi(i: &str) -> ParseResult<&str> {
    if i.is_empty() {
        Ok((i, ""))
    } else {
        Err((i, ParserError::Eof))
    }
}

pub fn whitespace(i: &str) -> ParseResult<&str> {
    match i.find(|c: char| !c.is_whitespace()) {
        Some(x) => Ok((&i[x..], &i[..x])),
        _ => Ok(("", i)),
    }
}

fn ws<'a, T>(item: impl Fn(&'a str) -> ParseResult<T>) -> impl Fn(&'a str) -> ParseResult<T> {
    right(whitespace, right(comments, item))
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
choice!(A B C D E F G H I J K L M N, 0 1 2 3 4 5 6 7 8 9 10 11 12 13);
