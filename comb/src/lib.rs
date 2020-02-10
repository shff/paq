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
/// use comb::*;
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

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Tag,
}
