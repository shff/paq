const RESERVED: &[&str] = &["const", "for", "extends"];

#[derive(Clone, Debug, PartialEq)]
pub enum Node<'a> {
    Block(Vec<Node<'a>>),
    If((Box<Node<'a>>, Box<Node<'a>>, Option<Box<Node<'a>>>)),
    While((Box<Node<'a>>, Box<Node<'a>>)),
    Do((Box<Node<'a>>, Box<Node<'a>>)),
    Switch((Box<Node<'a>>, Box<Node<'a>>)),
    For((Box<Node<'a>>, Box<Node<'a>>)),
    With((Box<Node<'a>>, Box<Node<'a>>)),
    Declaration((&'a str, Vec<Node<'a>>)),
    Return(Option<Box<Node<'a>>>),
    Throw(Box<Node<'a>>),
    Continue(Option<Box<Node<'a>>>),
    Break(Option<Box<Node<'a>>>),
    Blank,

    Str(String),
    Interpolation(String),
    Ident(&'a str),
    Double(f64),
    Octal(u64),
    Hexadecimal(u64),
    BinaryNum(u64),
    Idents(Vec<&'a str>),
    Regex((&'a str, Option<&'a str>)),
    List(Vec<Option<Node<'a>>>),
    Object(Vec<Node<'a>>),
    Paren(Box<Node<'a>>),
    Closure((Vec<Node<'a>>, Box<Node<'a>>)),
    Function((Option<&'a str>, Vec<Node<'a>>, Box<Node<'a>>)),
    Shorthand((Box<Node<'a>>, Vec<Node<'a>>, Box<Node<'a>>)),
    Setter(Box<Node<'a>>),
    Getter(Box<Node<'a>>),
    Static(Box<Node<'a>>),
    Generator((Option<&'a str>, Vec<Node<'a>>, Box<Node<'a>>)),
    Class((Option<&'a str>, Option<Box<Node<'a>>>, Vec<Node<'a>>)),
    Field((Box<Node<'a>>, Box<Node<'a>>)),
    Unary(&'a str, Box<Node<'a>>),
    Binary(&'a str, Box<Node<'a>>, Box<Node<'a>>),
    Ternary(Box<Node<'a>>, Box<Node<'a>>, Box<Node<'a>>),
    Import((Option<Box<Node<'a>>>, Box<Node<'a>>)),
    Export(Box<Node<'a>>),
    Default(Box<Node<'a>>),
    Try((Box<Node<'a>>, Option<Box<Node<'a>>>, Option<Box<Node<'a>>>)),
    Catch((Option<Box<Node<'a>>>, Box<Node<'a>>)),
    Label((Box<Node<'a>>, Box<Node<'a>>)),
    Case((Box<Node<'a>>, Box<Node<'a>>)),

    Args(Vec<Node<'a>>),
    ListPattern(Vec<Option<Node<'a>>>),
    ObjPattern(Vec<Node<'a>>),
    Splat(Box<Node<'a>>),
    KeyValue((Box<Node<'a>>, Box<Node<'a>>)),
    Param((Box<Node<'a>>, Option<Box<Node<'a>>>)),
    ForTrio(Vec<Option<Node<'a>>>),
    ForOf((Box<Node<'a>>, Box<Node<'a>>)),
    ForIn((Box<Node<'a>>, Box<Node<'a>>)),
    Variable((Option<&'a str>, Box<Node<'a>>)),
}

pub fn block(i: &str) -> ParseResult<'_, Node<'_>> {
    ws(map(many(case), Node::Block))(i)
}

fn case<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let label = || middle(ws(tag("case")), ws(expression), ws(tag(":")));
    let empty = map(
        right(
            label(),
            map(
                peek(ws(choice((tag("}"), tag("case"), tag("default"))))),
                |_| Node::Blank,
            ),
        ),
        |n| Node::Case((Box::new(n), Box::new(Node::Blank))),
    );
    let inner = map(pair(boxed(label()), boxed(case)), Node::Case);
    let line_comment = map(right(whitespace, single_comment), |_| Node::Blank);
    choice((ws(choice((empty, inner, default))), html_comment, line_comment))(i)
}

fn default<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let label = || pair(ws(tag("default")), ws(tag(":")));
    let empty = map(
        right(
            label(),
            map(
                peek(ws(choice((tag("}"), tag("case"), tag("default"))))),
                |_| Node::Blank,
            ),
        ),
        |n| Node::Default(Box::new(n)),
    );
    let inner = map(right(label(), boxed(default)), Node::Default);
    ws(choice((empty, inner, labeled)))(i)
}

fn labeled<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let label = left(ws(ident), ws(tag(":")));
    let inner = map(pair(boxed(label), boxed(labeled)), Node::Label);
    ws(choice((inner, statement)))(i)
}

fn statement<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    choice((
        braces, condition, while_loop, do_loop, for_loop, with, gotos, function,
    ))(i)
}

fn gotos<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let empty = map(peek(tag(";")), |_| Node::Blank);
    left(
        choice((
            imports,
            exports,
            try_catch,
            switch,
            cont,
            brk,
            ret,
            thrw,
            declaration,
            expression,
            html_comment,
            empty,
        )),
        end,
    )(i)
}

fn import_specifiers<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let mk_alias = || right(ws(tag("as")), ws(ident));
    let named_item = left(
        pair(ws(choice((quote, ident))), opt(mk_alias())),
        ws(opt(tag(","))),
    );
    let named = map(middle(ws(tag("{")), many(named_item), ws(tag("}"))), |_| {
        Node::Blank
    });
    let star = map(left(ws(tag("*")), opt(mk_alias())), |_| Node::Blank);
    choice((star, named))(i)
}

fn imports<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let default_bind = left(boxed(ws(ident)), ws(opt(tag(","))));
    let specifiers = map(import_specifiers, |_| None);
    let default_then_spec = map(
        pair(default_bind, opt(map(import_specifiers, |_| Node::Blank))),
        |(b, _)| Some(b),
    );
    let binding = choice((default_then_spec, specifiers));
    let with_from = pair(binding, right(ws(tag("from")), boxed(ws(quote))));
    let bare = map(ws(quote), |q| (None, Box::new(q)));
    let inner = right(ws(tag("import")), choice((with_from, bare)));
    map(inner, Node::Import)(i)
}

fn export_specifiers<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let alias = right(ws(tag("as")), ws(choice((quote, ident))));
    let item = map(pair(ws(choice((quote, ident))), opt(alias)), |_| {
        Node::Blank
    });
    let list = middle(ws(tag("{")), chain(ws(tag(",")), item), ws(tag("}")));
    let from = opt(right(ws(tag("from")), ws(quote)));
    map(left(list, from), |_| Node::Object(vec![]))(i)
}

fn exports<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let default = map(right(ws(tag("default")), boxed(expression)), Node::Default);
    let star = map(
        left(ws(tag("*")), opt(right(ws(tag("from")), ws(quote)))),
        |_| Node::Object(vec![]),
    );
    let from = opt(right(ws(tag("from")), ws(quote)));
    let named = map(left(object, from), |o| o);
    let opts = choice((declaration, function, class, export_specifiers, star, named));
    let inner = right(ws(tag("export")), boxed(choice((opts, default))));
    map(inner, Node::Export)(i)
}

fn try_catch<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let tryb = right(ws(tag("try")), boxed(braces));
    let finally = right(ws(tag("finally")), boxed(braces));
    map(trio(tryb, opt(boxed(catch)), opt(finally)), Node::Try)(i)
}

fn catch<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let exception = middle(ws(tag("(")), boxed(pattern), ws(tag(")")));
    let inner = right(tag("catch"), pair(opt(exception), boxed(braces)));
    ws(map(inner, Node::Catch))(i)
}

fn switch<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let expr = middle(ws(tag("(")), expression, ws(tag(")")));
    let inner = right(ws(tag("switch")), pair(boxed(expr), boxed(braces)));
    map(inner, Node::Switch)(i)
}

fn cont<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(tag("continue"), opt(boxed(ident)));
    ws(map(inner, Node::Continue))(i)
}

fn brk<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(tag("break"), opt(boxed(ident)));
    ws(map(inner, Node::Break))(i)
}

fn ret<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(tag("return"), opt(boxed(expression)));
    ws(map(inner, Node::Return))(i)
}

fn thrw<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(tag("throw"), boxed(expression));
    ws(map(inner, Node::Throw))(i)
}

fn declaration<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &["var", "let", "const"];
    let declaration = ws(pair(one_of(ops), chain(ws(tag(",")), mutation)));
    map(declaration, Node::Declaration)(i)
}

fn condition<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let else_block = ws(right(tag("else"), boxed(labeled)));
    let inner = trio(boxed(paren), boxed(labeled), opt(else_block));
    map(ws(right(tag("if"), inner)), Node::If)(i)
}

fn while_loop<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = pair(boxed(paren), boxed(labeled));
    map(ws(right(tag("while"), inner)), Node::While)(i)
}

fn do_loop<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(tag("do"), boxed(labeled));
    map(ws(outer(inner, ws(tag("while")), boxed(paren))), Node::Do)(i)
}

fn for_loop<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let iter = boxed(choice((for_of, for_in, for_trio)));
    let inner = pair(middle(ws(tag("(")), iter, ws(tag(")"))), boxed(labeled));
    map(ws(right(tag("for"), inner)), Node::For)(i)
}

fn for_trio<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let a = opt(choice((declaration, expression)));
    let b = right(ws(tag(";")), opt(expression));
    let c = right(ws(tag(";")), opt(expression));
    map(trio(a, b, c), |(a, b, c)| Node::ForTrio(vec![a, b, c]))(i)
}

fn for_lvalue<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &["var", "let", "const"];
    let not_in_of = |i: &'a str| {
        let (rest, node) = pattern(i)?;
        // reject if the bound name is literally "in" or "of"
        if let Node::Param((ref n, None)) = node {
            if let Node::Ident(name) = n.as_ref() {
                if *name == "in" || *name == "of" {
                    return Err((i, ParserError::Reserved(name.to_string())));
                }
            }
        }
        Ok((rest, node))
    };
    let with_keyword = ws(pair(opt(one_of(ops)), boxed(not_in_of)));
    choice((map(with_keyword, Node::Variable), postfix))(i)
}

fn for_of<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(
        outer(boxed(for_lvalue), ws(tag("of")), boxed(expression)),
        Node::ForOf,
    )(i)
}

fn for_in<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(
        outer(boxed(for_lvalue), ws(tag("in")), boxed(expression)),
        Node::ForIn,
    )(i)
}

fn with<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = pair(boxed(paren), boxed(labeled));
    map(ws(right(tag("with"), inner)), Node::With)(i)
}

#[allow(dead_code)]
fn variable<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &["var", "let", "const"];
    map(ws(pair(opt(one_of(ops)), boxed(pattern))), Node::Variable)(i)
}

pub fn expression<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(yield1, tag(",")), makechain2)(i)
}

fn yield1<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let yield_star = value(pair(tag("yield"), ws(tag("*"))), "yield*");
    map(prefix(yield_star, yield2), makechain)(i)
}

fn yield2<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let bare_yield = map(ws(tag("yield")), Node::Ident);
    choice((map(prefix(tag("yield"), mutation), makechain), bare_yield))(i)
}

fn mutation<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &[
        "=", "+=", "-=", "**=", "*=", "/=", "%=", "<<=", ">>>=", ">>=", "&=", "^=", "|=",
    ];
    map(infix(ternary, one_of(ops)), makechain2)(i)
}

fn ternary<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let conds = right(ws(tag("?")), outer(equality, ws(tag(":")), equality));
    ws(map(pair(equality, many(conds)), maketernary))(i)
}

fn equality<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &["===", "==", "!==", "!="];
    map(infix(comparison, one_of(ops)), makechain2)(i)
}

fn comparison<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &[">=", "<=", ">", "<", "instanceof", "in"];
    map(infix(bitwise, one_of(ops)), makechain2)(i)
}

fn bitwise<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &[">>>", ">>", "<<"];
    map(infix(logic_or, one_of(ops)), makechain2)(i)
}

fn logic_or<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(logic_and, tag("&&")), makechain2)(i)
}

fn logic_and<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(coalesce, tag("||")), makechain2)(i)
}

fn coalesce<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(bitwise_or, tag("??")), makechain2)(i)
}

fn bitwise_or<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(bitwise_xor, tag("|")), makechain2)(i)
}

fn bitwise_xor<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(bitwise_and, tag("^")), makechain2)(i)
}

fn bitwise_and<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(addition, tag("&")), makechain2)(i)
}

fn addition<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(multiplication, one_of(&["+", "-"])), makechain2)(i)
}

fn multiplication<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(power, one_of(&["*", "/", "%"])), makechain2)(i)
}

fn power<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(infix(negation, tag("**")), makechain2)(i)
}

fn negation<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &["!", "~"];
    map(prefix(one_of(ops), prefixes), makechain)(i)
}

fn prefixes<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &["++", "--", "+", "-", "typeof", "void", "delete", "await"];
    map(prefix(one_of(ops), postfix), makechain)(i)
}

fn postfix<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let ops = &["++", "--"];
    map(pair(creation, many(ws(one_of(ops)))), makechainb)(i)
}

fn creation<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let new_expr = map(right(ws(tag("new")), boxed(creation)), |e| Node::Unary("new", e));
    choice((new_expr, action))(i)
}

fn action<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let interp = pair(peek(tag("`")), map(string("`"), Node::Interpolation));
    let array = pair(tag("["), left(expression, ws(tag("]"))));
    let opt = pair(tag("?."), ident);
    let dot = pair(tag("."), ident);
    let call = pair(tag("("), left(map(args, Node::Args), ws(tag(")"))));
    let ea = pair(tag("?.["), left(expression, ws(tag("]"))));
    let ec = pair(tag("?.("), left(map(args, Node::Args), ws(tag(")"))));
    let action = pair(
        primitive,
        many(ws(choice((array, opt, dot, call, ea, ec, interp)))),
    );
    map(action, makechain2)(i)
}

fn new_target<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(
        capture(trio(tag("new"), tag("."), tag("target"))),
        Node::Ident,
    )(i)
}

fn primitive<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let double = map(double, Node::Double);
    let numeric = choice((octal, hexa, binary, double));
    ws(choice((
        quote,
        numeric,
        generator,
        function,
        object,
        closure,
        paren,
        list,
        regex,
        class,
        new_target,
        ident,
        interpolate,
    )))(i)
}

fn octal<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(choice((tag("0o"), tag("0O"))), number(8));
    ws(map(inner, Node::Octal))(i)
}

fn hexa<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(choice((tag("0x"), tag("0X"))), number(16));
    ws(map(inner, Node::Hexadecimal))(i)
}

fn binary<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = right(choice((tag("0b"), tag("0B"))), number(2));
    ws(map(inner, Node::BinaryNum))(i)
}

fn regex<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = pair(capture(string("/")), opt(take_while(|c| c.is_alphabetic())));
    ws(map(inner, Node::Regex))(i)
}

fn interpolate<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(string("`"), Node::Interpolation)(i)
}

fn quote<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let single_quote = map(string("\""), Node::Str);
    let double_quote = map(string("'"), Node::Str);
    ws(choice((single_quote, double_quote)))(i)
}

fn ident<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    map(identifier, Node::Ident)(i)
}

fn idents<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = middle(tag("("), chain(ws(tag(",")), identifier), ws(tag(")")));
    ws(map(inner, Node::Idents))(i)
}

fn unicode_escape(i: &str) -> ParseResult<'_, &str> {
    let hex_braced = middle(
        tag("{"),
        take_while(|c: char| c.is_ascii_hexdigit()),
        tag("}"),
    );
    let hex4 = take_while(|c: char| c.is_ascii_hexdigit());
    capture(right(tag("\\u"), choice((hex_braced, hex4))))(i)
}

fn identifier<'a>(i: &'a str) -> ParseResult<'a, &'a str> {
    let mk_ident_char = || {
        take_while(|c: char| {
            c.is_alphanumeric()
                || c == '_'
                || c == '$'
                || c == '\u{200C}'
                || c == '\u{200D}'
                || c == '\u{2028}'
                || c == '\u{2029}'
                || (!c.is_ascii() && (c.is_alphabetic() || c == '‿'))
        })
    };
    ws(reserved(
        capture(pair(
            choice((unicode_escape, mk_ident_char())),
            many(choice((unicode_escape, mk_ident_char()))),
        )),
        RESERVED,
    ))(i)
}

fn object<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let shorthand_default = map(
        pair(boxed(ident), right(ws(tag("=")), boxed(expression))),
        |(k, v)| Node::Param((k, Some(v))),
    );
    let item = choice((methods, key_value, shorthand_default, ident, splat));
    let object = middle(tag("{"), chain(ws(tag(",")), item), ws(tag("}")));
    ws(map(object, Node::Object))(i)
}

fn field<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = outer(boxed(ident), ws(tag("=")), boxed(expression));
    ws(map(inner, Node::Field))(i)
}

fn methods<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    ws(choice((statik, getter, setter, shorthand)))(i)
}

fn statik<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = boxed(choice((getter, setter, shorthand)));
    ws(map(right(tag("static"), inner), Node::Static))(i)
}

fn getter<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    ws(map(right(tag("get"), boxed(shorthand)), Node::Getter))(i)
}

fn setter<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    ws(map(right(tag("set"), boxed(shorthand)), Node::Setter))(i)
}

fn shorthand<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let mk_title = || choice((quote, ident, middle(tag("["), expression, tag("]"))));
    let gen_title = right(ws(tag("*")), mk_title());
    let inner = trio(
        ws(boxed(choice((gen_title, mk_title())))),
        params,
        boxed(braces),
    );
    map(inner, Node::Shorthand)(i)
}

fn paren<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let paren = middle(tag("("), boxed(expression), ws(tag(")")));
    ws(map(paren, Node::Paren))(i)
}

fn list<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let items = chain(tag(","), ws(opt(choice((splat, yield1)))));
    let list = middle(tag("["), items, ws(tag("]")));
    ws(map(list, Node::List))(i)
}

fn closure<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let params = ws(choice((map(ident, |a| vec![a]), params)));
    let body = choice((braces, expression));
    let closure = outer(params, ws(tag("=>")), boxed(body));
    ws(map(closure, Node::Closure))(i)
}

fn function<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = trio(ws(opt(identifier)), params, boxed(braces));
    let func = ws(right(tag("function"), inner));
    map(func, Node::Function)(i)
}

fn generator<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let inner = trio(ws(opt(identifier)), params, boxed(braces));
    let func = ws(right(pair(tag("function"), ws(tag("*"))), inner));
    map(func, Node::Generator)(i)
}

fn class<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let empty = map(ws(tag(";")), |_| Node::Blank);
    let inner = many(left(choice((field, methods, empty)), opt(end)));
    let inner = ws(middle(tag("{"), inner, ws(tag("}"))));
    let extend = ws(opt(right(tag("extends"), boxed(choice((ident, idents))))));
    let title = ws(right(tag("class"), opt(identifier)));
    map(trio(title, extend, inner), Node::Class)(i)
}

fn braces<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    ws(middle(tag("{"), block, ws(tag("}"))))(i)
}

fn params<'a>(i: &'a str) -> ParseResult<'a, Vec<Node<'a>>> {
    let inner = chain(ws(tag(",")), choice((splat, pattern)));
    ws(middle(tag("("), inner, ws(tag(")"))))(i)
}

fn args<'a>(i: &'a str) -> ParseResult<'a, Vec<Node<'a>>> {
    ws(chain(tag(","), choice((splat, expression))))(i)
}

fn splat<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let exp = boxed(right(tag("..."), yield1));
    ws(map(exp, Node::Splat))(i)
}

fn pattern<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let param = choice((list_pattern, object_pattern, ident));
    let default = right(ws(tag("=")), ws(expression));
    let inner = pair(boxed(param), opt(boxed(default)));
    ws(map(inner, Node::Param))(i)
}

fn list_pattern<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let items = chain(tag(","), ws(opt(choice((splat, pattern)))));
    let inner = middle(tag("["), items, ws(tag("]")));
    ws(map(inner, Node::ListPattern))(i)
}

fn key_value_pattern<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let key = ws(boxed(choice((quote, ident))));
    let value = boxed(pattern);
    map(outer(key, ws(tag(":")), value), Node::KeyValue)(i)
}

fn object_pattern<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let items = chain(tag(","), ws(choice((splat, key_value_pattern, pattern))));
    let inner = middle(tag("{"), items, ws(tag("}")));
    ws(map(inner, Node::ObjPattern))(i)
}

fn comments<'a>(i: &'a str) -> ParseResult<'a, &'a str> {
    let space = opt(take_while(|c| c == ' ' || c == '\t'));
    let multi = capture(trio(tag("/*"), take_until("*/"), tag("*/")));
    capture(many(right(choice((single_comment, multi)), space)))(i)
}

fn single_comment<'a>(i: &'a str) -> ParseResult<'a, &'a str> {
    let comments = choice((tag("//"), tag("<!--")));
    capture(trio(comments, take_until("\n"), tag("\n")))(i)
}

fn multi_comment<'a>(i: &'a str) -> ParseResult<'a, &'a str> {
    capture(trio(
        tag("/*"),
        right(take_until("\n"), tag("\n")),
        right(take_until("*/"), tag("*/")),
    ))(i)
}

fn html_comment<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let leading = take_while(|c: char| c.is_whitespace());
    let close = capture(trio(tag("-->"), take_until("\n"), tag("\n")));
    let open = capture(trio(tag("<!--"), take_until("\n"), tag("\n")));
    map(right(opt(leading), choice((close, open))), |_| Node::Blank)(i)
}

fn key_value<'a>(i: &'a str) -> ParseResult<'a, Node<'a>> {
    let double_quote = map(string("\""), Node::Str);
    let single_quote = map(string("\'"), Node::Str);
    let computed = middle(tag("["), expression, tag("]"));
    let key = ws(boxed(choice((double_quote, single_quote, ident, computed))));
    let value = boxed(yield1);
    map(outer(key, ws(tag(":")), value), Node::KeyValue)(i)
}

fn end<'a>(i: &'a str) -> ParseResult<'a, &'a str> {
    let mk_eol = || take_while(|c| c == '\n' || c == '\r');
    let eol_then_html = capture(pair(
        mk_eol(),
        capture(trio(ws(tag("-->")), take_until("\n"), tag("\n"))),
    ));
    let brace = value(peek(tag("}")), "");
    let ws_eol = capture(trio(whitespace, comments, mk_eol()));
    choice((
        ws(eoi),
        ws(tag(";")),
        eol_then_html,
        ws_eol,
        mk_eol(),
        single_comment,
        multi_comment,
        ws(brace),
    ))(i)
}

// Tree walking

pub fn transform(root: Node) -> Node {
    walk(root, |child| {
        if let Node::Import((None, path)) = child {
            if let Node::Str(path) = *path {
                return Some(Node::Binary(
                    "(",
                    Box::new(Node::Ident("require")),
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
                if func_name == "require" {
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

/// Returns each top-level statement with its (start, end) byte offsets into `source`.
pub fn stmts_with_spans(source: &str) -> Vec<(Node<'_>, usize, usize)> {
    let mut offset = 0usize; // current position in source (bytes consumed so far)
    let mut result = Vec::new();
    loop {
        let i = &source[offset..];
        if i.is_empty() {
            break;
        }
        let prev_remaining = i.len();
        match case(i) {
            Ok((after, node)) => {
                let consumed = prev_remaining - after.len();
                let start = offset;
                let end = offset + consumed;
                if !matches!(node, Node::Blank) || end > start {
                    result.push((node, start, end));
                }
                if consumed == 0 {
                    break; // no progress
                }
                offset = end;
            }
            Err(_) => break,
        }
    }
    result
}

/// Extracts (exported_name, local_name) pairs from an import specifier list like `{ foo, bar as baz }`.
/// Returns empty vec if the source doesn't contain a `{` specifier block.
pub fn parse_import_names(stmt_source: &str) -> Vec<(String, String)> {
    let mk_alias = || right(ws(tag("as")), ws(ident));
    let named_item = left(
        pair(ws(choice((quote, ident))), opt(mk_alias())),
        ws(opt(tag(","))),
    );
    let named = middle(ws(tag("{")), many(named_item), ws(tag("}")));
    // find the opening brace in the statement
    let start = match stmt_source.find('{') {
        Some(pos) => &stmt_source[pos..],
        None => return vec![],
    };
    match named(start) {
        Ok((_, items)) => items
            .into_iter()
            .map(|(name_node, alias)| {
                let exported = match &name_node {
                    Node::Ident(s) => s.to_string(),
                    Node::Str(s) => s.clone(),
                    _ => String::new(),
                };
                let local = alias.map(|a| match a {
                    Node::Ident(s) => s.to_string(),
                    _ => String::new(),
                }).unwrap_or_else(|| exported.clone());
                (exported, local)
            })
            .filter(|(e, _)| !e.is_empty())
            .collect(),
        Err(_) => vec![],
    }
}

/// Collects all Ident nodes in `root` with their (name, start, end) byte positions
/// relative to `base_ptr` (the address of the first byte of the source slice `root` was parsed from).
pub fn collect_ident_positions<'a>(root: &Node<'a>, base_ptr: usize, out: &mut Vec<(&'a str, usize, usize)>) {
    match root {
        Node::Ident(s) => {
            let start = s.as_ptr() as usize - base_ptr;
            out.push((s, start, start + s.len()));
        }
        Node::Block(children) | Node::Object(children) | Node::Args(children)
        | Node::ObjPattern(children) => {
            for c in children { collect_ident_positions(c, base_ptr, out); }
        }
        Node::List(items) | Node::ListPattern(items) => {
            for n in items.iter().flatten() { collect_ident_positions(n, base_ptr, out); }
        }
        Node::Declaration((_, children)) => {
            for c in children { collect_ident_positions(c, base_ptr, out); }
        }
        Node::Variable((_, inner)) => collect_ident_positions(inner, base_ptr, out),
        Node::Param((a, b)) => {
            collect_ident_positions(a, base_ptr, out);
            if let Some(b) = b { collect_ident_positions(b, base_ptr, out); }
        }
        Node::Function((_, params, body)) | Node::Generator((_, params, body)) => {
            for p in params { collect_ident_positions(p, base_ptr, out); }
            collect_ident_positions(body, base_ptr, out);
        }
        Node::Closure((params, body)) => {
            for p in params { collect_ident_positions(p, base_ptr, out); }
            collect_ident_positions(body, base_ptr, out);
        }
        Node::Shorthand((key, params, body)) => {
            collect_ident_positions(key, base_ptr, out);
            for p in params { collect_ident_positions(p, base_ptr, out); }
            collect_ident_positions(body, base_ptr, out);
        }
        Node::Class((_, super_, body)) => {
            if let Some(s) = super_ { collect_ident_positions(s, base_ptr, out); }
            for m in body { collect_ident_positions(m, base_ptr, out); }
        }
        Node::Field((k, v)) | Node::KeyValue((k, v)) | Node::Binary(_, k, v) => {
            collect_ident_positions(k, base_ptr, out);
            collect_ident_positions(v, base_ptr, out);
        }
        Node::Unary(_, a) | Node::Getter(a) | Node::Setter(a) | Node::Static(a)
        | Node::Export(a) | Node::Default(a) | Node::Return(Some(a))
        | Node::Continue(Some(a)) | Node::Break(Some(a)) | Node::Throw(a)
        | Node::Paren(a) | Node::Splat(a) => collect_ident_positions(a, base_ptr, out),
        Node::If((cond, then, else_)) => {
            collect_ident_positions(cond, base_ptr, out);
            collect_ident_positions(then, base_ptr, out);
            if let Some(e) = else_ { collect_ident_positions(e, base_ptr, out); }
        }
        Node::Ternary(a, b, c) => {
            collect_ident_positions(a, base_ptr, out);
            collect_ident_positions(b, base_ptr, out);
            collect_ident_positions(c, base_ptr, out);
        }
        Node::While((a, b)) | Node::Do((a, b)) | Node::Switch((a, b))
        | Node::For((a, b)) | Node::ForOf((a, b)) | Node::ForIn((a, b))
        | Node::With((a, b)) | Node::Label((a, b)) | Node::Case((a, b)) => {
            collect_ident_positions(a, base_ptr, out);
            collect_ident_positions(b, base_ptr, out);
        }
        Node::Try((body, catch, finally)) => {
            collect_ident_positions(body, base_ptr, out);
            if let Some(c) = catch { collect_ident_positions(c, base_ptr, out); }
            if let Some(f) = finally { collect_ident_positions(f, base_ptr, out); }
        }
        Node::Catch((binding, body)) => {
            if let Some(b) = binding { collect_ident_positions(b, base_ptr, out); }
            collect_ident_positions(body, base_ptr, out);
        }
        Node::Import((binding, path)) => {
            if let Some(b) = binding { collect_ident_positions(b, base_ptr, out); }
            collect_ident_positions(path, base_ptr, out);
        }
        Node::ForTrio(items) => {
            for n in items.iter().flatten() { collect_ident_positions(n, base_ptr, out); }
        }
        _ => {}
    }
}

fn walk<V>(node: Node, mut visit: V) -> Node
where
    V: Copy + FnMut(Node) -> Option<Node>,
{
    if let Some(ret) = visit(node.clone()) {
        return ret;
    }
    match node.clone() {
        Node::Str(a) => Node::Str(a),
        Node::Interpolation(a) => Node::Interpolation(a),
        Node::Regex(a) => Node::Regex(a),
        Node::Ident(a) => Node::Ident(a),
        Node::Idents(a) => Node::Idents(a),
        Node::Double(a) => Node::Double(a),
        Node::Octal(a) => Node::Octal(a),
        Node::Hexadecimal(a) => Node::Hexadecimal(a),
        Node::BinaryNum(a) => Node::BinaryNum(a),
        Node::Getter(a) => Node::Getter(Box::new(walk(*a.clone(), visit))),
        Node::Setter(a) => Node::Setter(Box::new(walk(*a.clone(), visit))),
        Node::Static(a) => Node::Static(Box::new(walk(*a.clone(), visit))),
        Node::Export(a) => Node::Export(Box::new(walk(*a.clone(), visit))),
        Node::Default(a) => Node::Default(Box::new(walk(*a.clone(), visit))),
        Node::Return(a) => Node::Return(a.map(|a| Box::new(walk(*a.clone(), visit)))),
        Node::Continue(a) => Node::Continue(a.map(|a| Box::new(walk(*a.clone(), visit)))),
        Node::Break(a) => Node::Break(a.map(|a| Box::new(walk(*a.clone(), visit)))),
        Node::Throw(a) => Node::Throw(Box::new(walk(*a.clone(), visit))),
        Node::Paren(a) => Node::Paren(Box::new(walk(*a.clone(), visit))),
        Node::Splat(a) => Node::Splat(Box::new(walk(*a.clone(), visit))),
        Node::Unary(a, b) => Node::Unary(a, Box::new(walk(*b.clone(), visit))),
        Node::Variable((a, b)) => Node::Variable((a, Box::new(walk(*b.clone(), visit)))),
        Node::Block(a) => Node::Block(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::List(a) => Node::List(
            a.iter()
                .map(|n| n.as_ref().map(|m| walk(m.clone(), visit)))
                .collect(),
        ),
        Node::ListPattern(a) => Node::ListPattern(
            a.iter()
                .map(|n| n.as_ref().map(|m| walk(m.clone(), visit)))
                .collect(),
        ),
        Node::Object(a) => Node::Object(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::ObjPattern(a) => Node::ObjPattern(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::Args(a) => Node::Args(a.iter().map(|n| walk(n.clone(), visit)).collect()),
        Node::Declaration((a, b)) => {
            Node::Declaration((a, b.iter().map(|n| walk(n.clone(), visit)).collect()))
        }
        Node::While((a, b)) => Node::While((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Do((a, b)) => Node::Do((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Switch((a, b)) => Node::Switch((
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
        Node::With((a, b)) => Node::With((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Closure((a, b)) => Node::Closure((
            a.iter().map(|n| walk(n.clone(), visit)).collect(),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Import((a, b)) => Node::Import((
            a.map(|b| Box::new(walk(*b.clone(), visit))),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Class((a, b, c)) => Node::Class((
            a,
            b.map(|b| Box::new(walk(*b.clone(), visit))),
            c.iter().map(|n| walk(n.clone(), visit)).collect(),
        )),
        Node::Field((a, b)) => Node::Field((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::KeyValue((a, b)) => Node::KeyValue((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Label((a, b)) => Node::Label((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Case((a, b)) => Node::Case((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Param((a, b)) => Node::Param((
            Box::new(walk(*a.clone(), visit)),
            b.map(|b| Box::new(walk(*b.clone(), visit))),
        )),
        Node::Try((a, b, c)) => Node::Try((
            Box::new(walk(*a.clone(), visit)),
            b.map(|b| Box::new(walk(*b.clone(), visit))),
            c.map(|c| Box::new(walk(*c.clone(), visit))),
        )),
        Node::Catch((a, b)) => Node::Catch((
            a.map(|a| Box::new(walk(*a.clone(), visit))),
            Box::new(walk(*b.clone(), visit)),
        )),
        Node::Function((a, b, c)) => Node::Function((
            a,
            b.iter().map(|n| walk(n.clone(), visit)).collect(),
            Box::new(walk(*c.clone(), visit)),
        )),
        Node::Generator((a, b, c)) => Node::Generator((
            a,
            b.iter().map(|n| walk(n.clone(), visit)).collect(),
            Box::new(walk(*c.clone(), visit)),
        )),
        Node::If((a, b, c)) => Node::If((
            Box::new(walk(*a.clone(), visit)),
            Box::new(walk(*b.clone(), visit)),
            c.map(|c| Box::new(walk(*c.clone(), visit))),
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
            b.iter().map(|n| walk(n.clone(), visit)).collect(),
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
        .fold(e.1, |acc, op| Node::Unary::<'a>(op, Box::new(acc)))
}

fn makechainb<'a>(e: (Node<'a>, Vec<&'a str>)) -> Node<'a> {
    e.1.iter()
        .fold(e.0, |acc, op| Node::Unary::<'a>(op, Box::new(acc)))
}

fn makechain2<'a>(e: (Node<'a>, Vec<(&'a str, Node<'a>)>)) -> Node<'a> {
    e.1.iter().fold(e.0, |a, (op, b)| {
        Node::Binary::<'a>(op, Box::new(a), Box::new(b.clone()))
    })
}

pub fn tag(tag: &'static str) -> impl Fn(&str) -> ParseResult<&str> {
    move |i| {
        if let Some(prefix) = i.strip_prefix(tag) {
            Ok((prefix, &i[..tag.len()]))
        } else {
            Err((i, ParserError::Tag(tag.to_string())))
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
    move |i| a(i).and_then(|(i, _)| b(i))
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

pub fn one_of<'a>(opts: &'a [&str]) -> impl Fn(&'a str) -> ParseResult<'a, &'a str> {
    move |i| {
        for opt in opts {
            if let Some(prefix) = i.strip_prefix(opt) {
                return Ok((prefix, &i[..opt.len()]));
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

pub fn reserved<'a, P>(p: P, words: &'a [&'a str]) -> impl Fn(&'a str) -> ParseResult<'a, &'a str>
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
            let mut i = i;
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

type InfixResult<'a, R, S> = ParseResult<'a, (R, Vec<(S, R)>)>;

pub fn infix<'a, P, O, R, S>(p: P, o: O) -> impl Fn(&'a str) -> InfixResult<'a, R, S>
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

pub fn string<'a>(q: &'static str) -> impl Fn(&'a str) -> ParseResult<String> {
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
        let chars = take_while(|c| c != q.chars().next().unwrap() && c != '\\');
        let inner = map(many(choice((chars, escaped))), |s| s.join(""));
        middle(tag(q), inner, tag(q))(i)
    }
}

pub fn number<'a>(b: u32) -> impl Fn(&'a str) -> ParseResult<u64> {
    move |i| mapr(take_while(|c| c.is_digit(b)), |s| u64::from_str_radix(s, b))(i)
}

pub fn double(i: &str) -> ParseResult<'_, f64> {
    let digit = |i| take_while(|c| c.is_numeric())(i);
    let sign = |i| opt(one_of(&["+", "-"]))(i);
    fn not_ident_after_dot(i: &str) -> ParseResult<'_, &str> {
        if i.starts_with(|c: char| c.is_alphabetic() || c == '_' || c == '$') {
            Err((i, ParserError::Tag("not_ident".to_string())))
        } else {
            Ok((i, ""))
        }
    }
    let dot_digit = left(pair(tag("."), opt(digit)), not_ident_after_dot);
    let num = value(pair(digit, opt(dot_digit)), 0);
    let frac = value(pair(tag("."), digit), 0);
    let exp = opt(trio(choice((tag("e"), tag("E"))), sign, digit));
    mapr(capture(trio(sign, choice((num, frac)), exp)), |s| s.parse())(i)
}

pub const fn eoi(i: &str) -> ParseResult<'_, &str> {
    if i.is_empty() {
        Ok((i, ""))
    } else {
        Err((i, ParserError::Eof))
    }
}

pub fn whitespace(i: &str) -> ParseResult<'_, &str> {
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
        impl<'a, OUT, $($id: Fn(&'a str) -> ParseResult<'a, OUT>),+>
            Choice<'a, OUT> for ( $($id),+ ) {
            fn choice(&self, i: &'a str) -> ParseResult<'a, OUT> {
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
choice!(A B C D E F G H I J K L M N O, 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14);
