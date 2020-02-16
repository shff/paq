use paq::pcomb::*;

#[test]
fn test_tag() {
    let parser = tag("function");

    assert_eq!(parser("function hello"), Ok((" hello", "function")));
    assert_eq!(parser("Something else"), Err(("Something else", ParserError::Tag)));
    assert_eq!(parser(""), Err(("", ParserError::Tag)));
}

#[test]
fn test_value() {
    let parser = value(tag("Hello, world!"), "Hallo welt");

    assert_eq!(parser("Hello, world!"), Ok(("", "Hallo welt")));
    assert_eq!(parser("Bonjour le monde"), Err(("Bonjour le monde", ParserError::Tag)));
}

#[test]
fn test_map() {
    let parser = map(tag("1"), |s| s.parse::<i32>().unwrap());

    assert_eq!(parser("1"), Ok(("", 1)));
    assert_eq!(parser("2"), Err(("2", ParserError::Tag)));
}

#[test]
fn test_map_res() {
    let parser = map_res(take_while(|c| c.is_alphanumeric()), |s| s.parse::<i32>());

    assert_eq!(parser("123"), Ok(("", 123)));
    assert_eq!(parser("abc"), Err(("", ParserError::MapRes)));
}

#[test]
fn test_opt() {
    let parser = opt(tag("1"));

    assert_eq!(parser("1"), Ok(("", Some("1"))));
    assert_eq!(parser("2"), Ok(("2", None)));
}

#[test]
fn test_pair() {
    let parser = pair(tag("hello "), tag("world"));

    assert_eq!(parser("hello world"), Ok(("", ("hello ", "world"))));
    assert_eq!(parser("oh noes"), Err(("oh noes", ParserError::Tag)));
}

#[test]
fn test_trio() {
    let parser = trio(tag("ein "), tag("zwei "), tag("drei"));

    assert_eq!(parser("ein zwei drei"), Ok(("", ("ein ", "zwei ", "drei"))));
    assert_eq!(parser("one two three"), Err(("one two three", ParserError::Tag)));
}

#[test]
fn test_right() {
    let parser = right(tag("not me "), tag("me"));

    assert_eq!(parser("not me me"), Ok(("", "me")));
    assert_eq!(parser("not me you"), Err(("you", ParserError::Tag)));
}

#[test]
fn test_left() {
    let parser = left(tag("me"), tag("you"));

    assert_eq!(parser("meyou"), Ok(("", "me")));
    assert_eq!(parser("youme"), Err(("youme", ParserError::Tag)));
}

#[test]
fn test_middle() {
    let parser = middle(tag("("), tag("secret"), tag(")"));

    assert_eq!(parser("(secret)"), Ok(("", "secret")));
    assert_eq!(parser("secret"), Err(("secret", ParserError::Tag)));
}

#[test]
fn test_outer() {
    let parser = outer(tag("a"), tag(","), tag("b"));

    assert_eq!(parser("a,b"), Ok(("", ("a", "b"))));
    assert_eq!(parser("a+b"), Err(("+b", ParserError::Tag)));
}

#[test]
fn test_either() {
    let parser = either(tag("a"), tag("b"));

    assert_eq!(parser("a"), Ok(("", "a")));
    assert_eq!(parser("b"), Ok(("", "b")));
    assert_eq!(parser("c"), Err(("c", ParserError::Tag)));
}

#[test]
fn test_choice() {
    let parser = choice([ tag("1"), tag("2"), tag("3") ]);

    assert_eq!(parser("1"), Ok(("", "1")));
    assert_eq!(parser("2"), Ok(("", "2")));
    assert_eq!(parser("3"), Ok(("", "3")));
    assert_eq!(parser("4"), Err(("4", ParserError::Choice)));
}

#[test]
fn test_take_while() {
    let parser = take_while(|c| c.is_numeric());

    assert_eq!(parser("123"), Ok(("", "123")));
    assert_eq!(parser("456"), Ok(("", "456")));
    assert_eq!(parser("abc"), Err(("abc", ParserError::TakeWhile)));
}

#[test]
fn test_take_until() {
    let parser = take_until(" combo breaker");

    assert_eq!(parser("123 combo breaker"), Ok((" combo breaker", "123")));
    assert_eq!(parser("456"), Ok(("456", "")));
}

#[test]
fn test_peek() {
    let parser = peek(tag("The future"));

    assert_eq!(parser("The future"), Ok(("The future", "The future")));
    assert_eq!(parser("Not the future"), Err(("Not the future", ParserError::Tag)));
}

#[test]
fn test_recognize() {
    let parser = recognize(pair(tag("badger"), tag("badger")));

    assert_eq!(parser("badgerbadger"), Ok(("", "badgerbadger")));
    assert_eq!(parser("mushroom"), Err(("mushroom", ParserError::Tag)));
}

#[test]
fn test_check() {
    let parser = check(take_until("-"), |a| a.len() == 3);

    assert_eq!(parser("yes-"), Ok(("-", "yes")));
    assert_eq!(parser("no-"), Err(("no-", ParserError::Check)));
}

#[test]
fn test_many() {
    let parser = many(tag("badger"));

    assert_eq!(parser("badgerbadgerbadger"), Ok(("", vec!["badger", "badger", "badger"])));
    assert_eq!(parser("not badger"), Ok(("not badger", vec![])));
}

#[test]
fn test_chain() {
    let parser = chain(tag(","), tag("1"));

    assert_eq!(parser("1,1,1"), Ok(("", vec!["1", "1", "1"])));
    assert_eq!(parser("1,1,1,"), Ok(("", vec!["1", "1", "1"])));
    assert_eq!(parser("1"), Ok(("", vec!["1"])));
    assert_eq!(parser("2"), Ok(("2", vec![])));
}

#[test]
fn test_infix() {
    let parser = infix(double, tag("+"));

    assert_eq!(parser("1+1"), Ok(("", (1.0, vec![("+", 1.0)]))));
    assert_eq!(parser("1+1+2"), Ok(("", (1.0, vec![("+", 1.0), ("+", 2.0)]))));
}

#[test]
fn test_prefix() {
    let parser = prefix(tag("!"), double);

    assert_eq!(parser("!!!1"), Ok(("", (vec!["!", "!", "!"], 1.0))));
}

#[test]
fn test_boxed() {
    let parser = boxed(tag("thing"));

    assert_eq!(parser("thing"), Ok(("", Box::new("thing"))));
    assert_eq!(parser("not thing"), Err(("not thing", ParserError::Tag)));
}

#[test]
fn test_double() {
    assert_eq!(double("1.0"), Ok(("", 1.0)));
    assert_eq!(double("2"), Ok(("", 2.0)));
    assert_eq!(double("2e2"), Ok(("", 200.0)));
    assert_eq!(double("2e-2"), Ok(("", 0.02)));
    assert_eq!(double("-2"), Ok(("", -2.0)));
    assert_eq!(double("+2"), Ok(("", 2.0)));
    assert_eq!(double("+.2"), Ok(("", 0.2)));
    assert_eq!(double("-.2"), Ok(("", -0.2)));
}

#[test]
fn test_eoi() {
    assert_eq!(eoi(""), Ok(("", "")));
    assert_eq!(eoi("not the end"), Err(("not the end", ParserError::Eof)));
}

#[test]
fn test_whitespace() {
    let parser1 = right(whitespace, tag("hello"));
    let parser2 = left(tag("hello"), whitespace);

    assert_eq!(parser1("    hello"), Ok(("", "hello")));
    assert_eq!(parser2("hello    "), Ok(("", "hello")));
    assert_eq!(whitespace(" \t\r\n "), Ok(("", " \t\r\n ")));
}

#[test]
fn test_w() {
    let parser = w(tag("tag"));

    assert_eq!(parser("    tag"), Ok(("", "tag")));
}
