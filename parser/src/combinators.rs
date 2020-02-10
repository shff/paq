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

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Tag,
}
