pub type ParseResult<'a, T> = Result<(&'a str, T), (&'a str, ParserError)>;

/// Recognizes a fixed string pattern.
///
/// If the input data matches the first argument, it will return a successful
/// value containing the argument itself.
///
/// Otherwise it returns `Err((_, ParserError::Tag))`
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
pub fn map<'a, P, F, A, B>(p: P, f: F) -> impl Fn(&'a str) -> ParseResult<B>
where
    P: Fn(&'a str) -> ParseResult<A>,
    F: Fn(A) -> B,
{
    move |i| p(i).map(|(i, r)| (i, f(r)))
}

/// Same as the map combinator, but errors in the lambda function used to
/// transform cause the parser to reject the token.
pub fn map_res<'a, P, F, A, B, E>(p: P, f: F) -> impl Fn(&'a str) -> ParseResult<B>
where
    P: Fn(&'a str) -> ParseResult<A>,
    F: Fn(A) -> Result<B, E>,
{
    move |i| p(i).and_then(|(i, r)| f(r).map(|r| (i, r)).or(Err((i, ParserError::MapRes))))
}

/// Makes the inner parser optional by swallowing errors and turning them into a
/// `None` value. Actual matched values are boxed by `Some`.
pub fn opt<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<Option<R>>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| p(i).and_then(|(i, r)| Ok((i, Some(r)))).or(Ok((i, None)))
}

/// Matches a pair of tokens. Both have to matched, otherwise the wole thing
/// fails. The error returned by the first unmatched parser is returned.
pub fn pair<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<(X, Y)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, r1)| b(i).map(|(i, r2)| (i, (r1, r2))))
}

/// What's better than a pair? You got it: a trio.
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
pub fn right<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<Y>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, _)| b(i).map(|(i, r2)| (i, r2)))
}

/// We already have a right combinator. Guess what's next? The left. Balanced,
/// as all things should be
pub fn left<'a, A, B, X, Y>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<X>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| a(i).and_then(|(i, r1)| b(i).map(|(i, _)| (i, r1)))
}

/// Same as left and right, but now it rejects both tokens that bookend the one
/// in the middle.
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
pub fn outer<'a, A, B, C, X, Y, Z>(a: A, b: B, c: C) -> impl Fn(&'a str) -> ParseResult<(X, Z)>
where
    A: Fn(&'a str) -> ParseResult<X>,
    B: Fn(&'a str) -> ParseResult<Y>,
    C: Fn(&'a str) -> ParseResult<Z>,
{
    move |i| a(i).and_then(|(i, x)| b(i).and_then(|(i, _)| c(i).map(|(i, z)| (i, (x, z)))))
}

/// Tries to match either one of the parsers and returns the sucessful one.
pub fn either<'a, A, B, R>(a: A, b: B) -> impl Fn(&'a str) -> ParseResult<R>
where
    A: Fn(&'a str) -> ParseResult<R>,
    B: Fn(&'a str) -> ParseResult<R>,
{
    move |i| a(i).or_else(|_| b(i))
}

/// Exactly like either, but in this case you can have as many choices as you
/// need (as long as they have the same type).
pub fn choice<'a, S, P, R>(ps: S) -> impl Fn(&'a str) -> ParseResult<R>
where
    S: AsRef<[P]>,
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| {
        AsRef::as_ref(&ps)
            .iter()
            .find_map(|p| p(i).ok())
            .ok_or((i, ParserError::Choice))
    }
}

/// This one swallows characters as long as a condition is matched.
pub fn take_while<'a, P>(p: P) -> impl Fn(&'a str) -> ParseResult<&str>
where
    P: Copy + Fn(char) -> bool,
{
    move |i| match i.find(|c| !p(c)) {
        Some(0) => Err((i, ParserError::TakeWhile)),
        Some(x) => Ok((&i[x..], &i[..x])),
        None if i.len() > 0 => Ok((&i[i.len()..], i)),
        None => Err((i, ParserError::TakeWhile)),
    }
}

/// Matches and joins all matched characters until the combination of characters
/// in the first argument is reached. Don't worry, it can be just one character
/// if you want.
///
/// This is very forgiving about errors. If the end of the string comes before
/// the breaking combination, it will return everything it found.
pub fn take_until<'a>(p: &'a str) -> impl Fn(&'a str) -> ParseResult<&str> {
    move |i| i.find(p).map_or(Ok((i, "")), |x| Ok((&i[x..], &i[..x])))
}

/// A curious parser: it returns successful response if its inner parsers are
/// able to find something, but it doesn't eat the characters.
pub fn peek<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<R>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| p(i).map(|(_, o)| (i, o))
}

/// This one extract the raw characters that were matched by the inner parsers.
///
/// It is particularly useful when you want to meticulously parse some content,
/// but wants it's raw content instead.
pub fn recognize<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<&'a str>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    move |i| p(i).map(|(i2, _)| (i2, &i[..(i2.as_ptr() as usize - i.as_ptr() as usize)]))
}

/// Ensures that the return of an inner parser matches the stated conditions.
///
/// Example
/// ```rust
/// use paq::combinators::*;
///
/// let parser = check(take_until("-"), |a| a.len() == 3);
///
/// assert_eq!(parser("yes-"), Ok(("-", "yes")));
/// assert_eq!(parser("no-"), Err(("no-", ParserError::Check)));
/// ```
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

/// Probably the most useful combinator of all. It matches multiple instances
/// the same sequence. Very useful for everything, from numbers and strings, up
/// to complex sequences.
///
/// If it doesn't find any instance of, the sequence it just returns an empty
/// array.
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

/// Matches a chain of repetitions linked (got it?) by a certain token or
/// combination of tokens.
pub fn chain<'a, S, P, R1, R2>(sep: S, p: P) -> impl Fn(&'a str) -> ParseResult<Vec<R2>>
where
    S: Fn(&'a str) -> ParseResult<R1>,
    P: Fn(&'a str) -> ParseResult<R2>,
    R1: Clone,
    R2: Clone,
{
    let join = |(a, b)| [vec![a], b].concat();
    move |i| map(pair(&p, left(many(right(&sep, &p)), opt(&sep))), join)(i).or(Ok((i, vec![])))
}

/// Similar to the chain operator, but it stores the result of the infix
/// operators.
pub fn infix<'a, P, O, R, S>(p: P, o: O) -> impl Fn(&'a str) -> ParseResult<(R, Vec<(S, R)>)>
where
    P: Fn(&'a str) -> ParseResult<R>,
    O: Fn(&'a str) -> ParseResult<S>,
{
    move |i| pair(&p, many(pair(w(&o), &p)))(i)
}

/// Similar to the infix operator, but allows multiple prefixes that can later
/// be chained together.
pub fn prefix<'a, P, Q, X, Y>(p: P, q: Q) -> impl Fn(&'a str) -> ParseResult<(Vec<X>, Y)>
where
    P: Fn(&'a str) -> ParseResult<X>,
    Q: Fn(&'a str) -> ParseResult<Y>,
{
    move |i| pair(many(w(&p)), &q)(i)
}

/// Wraps the result of the inner parser in a plain-old Rust Box.
pub fn boxed<'a, P, R>(i: P) -> impl Fn(&'a str) -> ParseResult<Box<R>>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    map(i, Box::new)
}

/// Parses a 64-bit floating point number.
pub fn double<'a>(i: &'a str) -> ParseResult<f64> {
    let digit = |i| take_while(|c| c.is_numeric())(i);
    let sign = |i| opt(either(tag("+"), tag("-")))(i);
    let num = value(pair(digit, opt(pair(tag("."), opt(digit)))), 0);
    let frac = value(pair(tag("."), digit), 0);
    let exp = opt(trio(either(tag("e"), tag("E")), sign, digit));
    map_res(recognize(trio(sign, either(num, frac), exp)), |s| s.parse())(i)
}

/// This is not a combinator, but rather a parser itself that detects if
/// we have reached the end of the input.
pub fn eoi(i: &str) -> ParseResult<&str> {
    match i.is_empty() {
        true => Ok((i, "")),
        false => Err((i, ParserError::Eof)),
    }
}

/// A parser that only cares about whitespace! Spaces, tabs, line breaks.
/// All the invisible stuff will be eaten by this monster.
pub fn whitespace<'a>(i: &str) -> ParseResult<&str> {
    match i.find(|c: char| !c.is_whitespace()) {
        Some(x) => Ok((&i[x..], &i[..x])),
        _ => Ok(("", i)),
    }
}

/// A convenient one-letter helper to trim whitespace from the left of your
/// tokens.
pub fn w<'a, P, R>(p: P) -> impl Fn(&'a str) -> ParseResult<R>
where
    P: Fn(&'a str) -> ParseResult<R>,
{
    right(whitespace, p)
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    Check,
    Choice,
    Eof,
    Tag,
    TakeWhile,
    MapRes,
}
