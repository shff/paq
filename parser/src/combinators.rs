pub type ParseResult<'a, T> = Result<(&'a str, T), (&'a str, ParserError)>;

/// Recognizes a fixed string pattern.
///
/// If the input data matches the first argument, it will return a successful
/// value containing the argument itself.
///
/// Otherwise it returns `Err((_, ParserError::Tag))`
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = tag("function");
///
/// assert_eq!(parser("function hello"), Ok((" hello", "function")));
/// assert_eq!(parser("Something else"), Err(("Something else", ParserError::Tag)));
/// assert_eq!(parser(""), Err(("", ParserError::Tag)));
/// ```
pub fn tag(tag: &'static str) -> impl Fn(&str) -> ParseResult<&str> {
    move |i| match i.starts_with(tag) {
        true => Ok((&i[tag.len()..], tag)),
        false => Err((i, ParserError::Tag)),
    }
}

/// Converts a matched parser to a fixed value.
///
/// It doesn't produce errors by itself, but errors by the inner parser are
/// forwarded to the output.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = value(tag("Hello, world!"), "Hallo welt");
///
/// assert_eq!(parser("Hello, world!"), Ok(("", "Hallo welt")));
/// assert_eq!(parser("Bonjour le monde"), Err(("Bonjour le monde", ParserError::Tag)));
/// ```
pub fn value<'a, P, R, V>(p: P, v: V) -> impl Fn(&'a str) -> ParseResult<V>
where
    P: Fn(&'a str) -> ParseResult<R>,
    V: Copy,
{
    move |i| p(i).map(|(i, _)| (i, v))
}

/// Transforms the (successful) output of a matched parser.
///
/// Since parsers dwell mostly in strings, you need something to turn them into
/// other kinds of values. As with the `value` combinator, errors by the inner
/// parser are forwarded to the output.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = map(tag("1"), |s| s.parse::<i32>().unwrap());
///
/// assert_eq!(parser("1"), Ok(("", 1)));
/// assert_eq!(parser("2"), Err(("2", ParserError::Tag)));
/// ```
pub fn map<'a, P, F, A, B>(p: P, f: F) -> impl Fn(&'a str) -> ParseResult<B>
where
    P: Fn(&'a str) -> ParseResult<A>,
    F: Fn(A) -> B,
{
    move |i| p(i).map(|(i, r)| (i, f(r)))
}

/// Makes the inner parser optional by swallowing errors and turning them into a
/// `None` value. Actual matched values are boxed by `Some`.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = opt(tag("1"));
///
/// assert_eq!(parser("1"), Ok(("", Some("1"))));
/// assert_eq!(parser("2"), Ok(("2", None)));
/// ```
pub fn opt<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<Option<R>>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| p(i).and_then(|(i, r)| Ok((i, Some(r)))).or(Ok((i, None)))
}

/// Matches a pair of tokens. Both have to matched, otherwise the wole thing
/// fails. The error returned by the first unmatched parser is returned.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = pair(tag("hello "), tag("world"));
///
/// assert_eq!(parser("hello world"), Ok(("", ("hello ", "world"))));
/// assert_eq!(parser("oh noes"), Err(("oh noes", ParserError::Tag)));
/// ```
pub fn pair<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<(X, Y)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, r1)| b(i).map(|(i, r2)| (i, (r1, r2))))
}

/// What's better than a pair? You got it: a trio.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = trio(tag("ein "), tag("zwei "), tag("drei"));
///
/// assert_eq!(parser("ein zwei drei"), Ok(("", ("ein ", "zwei ", "drei"))));
/// assert_eq!(parser("one two three"), Err(("one two three", ParserError::Tag)));
/// ```
pub fn trio<'a, A, B, C, X, Y, Z>(a: A, b: B, c: C) -> impl Fn(&'a str) -> ParseResult<(X, Y, Z)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
    C: Fn(&'a str) -> ParseResult<Z>,
{
    move |i| a(i).and_then(|(i, x)| b(i).and_then(|(i, y)| c(i).map(|(i, z)| (i, (x, y, z)))))
}

/// Just like the pair combinator, but it throws away the result of the parser
/// in the right and returns a single value.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = right(tag("not me "), tag("me"));
///
/// assert_eq!(parser("not me me"), Ok(("", "me")));
/// assert_eq!(parser("not me you"), Err(("you", ParserError::Tag)));
/// ```
pub fn right<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<Y>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, _)| b(i).map(|(i, r2)| (i, r2)))
}

/// We already have a right combinator. Guess what's next? The left. Balanced,
/// as all things should be
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = left(tag("me"), tag("you"));
///
/// assert_eq!(parser("meyou"), Ok(("", "me")));
/// assert_eq!(parser("youme"), Err(("youme", ParserError::Tag)));
/// ```
pub fn left<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<X>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, r1)| b(i).map(|(i, _)| (i, r1)))
}

/// Same as left and right, but now it rejects both tokens that bookend the one
/// in the middle.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = middle(tag("("), tag("secret"), tag(")"));
///
/// assert_eq!(parser("(secret)"), Ok(("", "secret")));
/// assert_eq!(parser("secret"), Err(("secret", ParserError::Tag)));
/// ```
pub fn middle<'a, A, B, C, X, Y, Z>(a: A, b: B, c: C) -> impl Fn(&'a str) -> ParseResult<Y>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
    C: Fn(&'a str) -> ParseResult<Z>,
{
    move |i| a(i).and_then(|(i, _)| b(i).and_then(|(i, r2)| c(i).map(|(i, _)| (i, r2))))
}

/// Takes the result of the outermost parsers and rejects the middle. Useful for
/// parsing separated pairs.
///
/// # Example
/// ```rust
/// use js_parser::combinators::*;
///
/// let parser = outer(tag("a"), tag(","), tag("b"));
///
/// assert_eq!(parser("a,b"), Ok(("", ("a", "b"))));
/// assert_eq!(parser("a+b"), Err(("+b", ParserError::Tag)));
/// ```
pub fn outer<'a, A, B, C, X, Y, Z>(a: A, b: B, c: C) -> impl Fn(&'a str) -> ParseResult<(X, Z)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
    C: Fn(&'a str) -> ParseResult<Z>,
{
    move |i| a(i).and_then(|(i, x)| b(i).and_then(|(i, _)| c(i).map(|(i, z)| (i, (x, z)))))
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Tag,
}
