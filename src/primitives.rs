use super::{ParseError, ParseSuccess, Parser};

/// Returns a parser which applies several parsers sequentially.
/// The returned parser will fail if any of the provided parsers fail
/// # Arguments
///
/// * `parsers`: A vector of parsers which are applied in the order provided
///
/// ```
/// use parser_combinators::combinators::text;
/// use parser_combinators::primitives::seq;
/// use parser_combinators::parse;
///
/// let parser = seq(vec![text("abc"), text("xyz")]);
/// let parts = parse(parser, &String::from("abcxyz"));
/// assert_eq!(parts[0], "abc");
/// assert_eq!(parts[1], "xyz");
/// ```
pub fn seq<A: 'static + Clone>(parsers: Vec<Parser<A>>) -> Parser<Vec<A>> {
    Box::new(move |input| {
        let start = ParseSuccess {
            value: vec![],
            next: input.to_string(),
        };

        parsers.iter().try_fold(start, |mut iter, p| {
            let res = p(&iter.next)?;
            iter.value.push(res.value);
            iter.next = res.next;
            Ok(iter)
        })
    })
}

/// Returns a parser which will apply a function to the output of a given parser
/// # Arguments
/// * `p`: A parser to apply to the input
/// * `func`: A function to apply to the output of the parser
///
/// ```
/// use parser_combinators::combinators::text;
/// use parser_combinators::primitives::map;
/// use parser_combinators::parse;
///
/// let parser = map(text("abc"), |t| t.to_uppercase());
/// let out = parse(parser, &String::from("abc"));
/// assert_eq!(out, "ABC");
/// ```
pub fn map<A: 'static + Clone, B: 'static + Clone>(p: Parser<A>, func: fn(A) -> B) -> Parser<B> {
    Box::new(move |input| {
        p(input).map(|res| ParseSuccess {
            value: func(res.value),
            next: res.next,
        })
    })
}

/// Returns a parser which will try to apply the first parser, and if that
/// fails, then the second
/// # Arguments
/// * `p`: Parser to apply first
/// * `q`: Parser to apply if first parser fails
///
/// ```
/// use parser_combinators::combinators::text;
/// use parser_combinators::primitives::or;
/// use parser_combinators::parse;
///
/// let parser = or(text("abc"), text("xyz"));
/// let out = parse(parser, &String::from("xyz"));
/// assert_eq!(out, "xyz");
/// ```
pub fn or<A: 'static>(p: Parser<A>, q: Parser<A>) -> Parser<A> {
    Box::new(move |input| {
        p(input).or_else(|e| {
            q(input).map_err(|e2| ParseError {
                expected: format!("{} or {}", e.expected, e2.expected),
                fatal: true,
                input: input.to_string(),
            })
        })
    })
}

/// Returns a parser which will attempt a apply a list of parsers in turn,
/// returning the first parse result that succeeds
/// # Arguments
/// * `parser`: A vector of parsers to apply in turn
///
/// ```
/// use parser_combinators::combinators::text;
/// use parser_combinators::primitives::one_of;
/// use parser_combinators::parse;
///
/// let parser = one_of(vec![text("abc"), text("def"), text("xyz")]);
/// let out = parse(parser, &String::from("def"));
/// assert_eq!(out, "def");
/// ```
pub fn one_of<A: 'static>(parsers: Vec<Parser<A>>) -> Parser<A> {
    Box::new(move |input| {
        let mut failures: Vec<ParseError> = vec![];
        for p in parsers.iter() {
            match p(input) {
                Ok(res) => return Ok(res),
                Err(e) => failures.push(e),
            }
        }

        Err(ParseError {
            input: input.to_string(),
            fatal: true,
            expected: format!(
                "one of {}",
                failures
                    .iter()
                    .map(|f| f.expected.clone())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        })
    })
}

pub fn sep_by<A: 'static, B: 'static>(p: Parser<A>, q: Parser<B>) -> Parser<Vec<A>> {
    Box::new(move |input| {
        let mut values: Vec<A> = vec![];
        let mut current = input.to_string();

        loop {
            let v = p(&current);
            match v {
                Ok(res) => match q(&res.next) {
                    Ok(res2) => {
                        current = res2.next;
                        values.push(res.value);
                    }
                    Err(_) => {
                        return Ok(ParseSuccess {
                            value: values,
                            next: current,
                        })
                    }
                },
                Err(_) => {
                    return Ok(ParseSuccess {
                        value: values,
                        next: current,
                    })
                }
            };
        }
    })
}

/// Returns a parser which tries apply the given parser 0 or more times and
/// returns a vector of the parse values
/// # Arguments
/// * `p`: A parser to apply repeatedly
///
/// ```
/// use parser_combinators::combinators::text;
/// use parser_combinators::primitives::many;
/// use parser_combinators::parse;
///
/// let parser = many(text("abc"));
/// let parts = parse(parser, &String::from("abcabc"));
/// assert_eq!(parts.len(), 2);
/// assert_eq!(parts[0], "abc");
/// assert_eq!(parts[1], "abc");
/// ```
pub fn many<A: 'static>(p: Parser<A>) -> Parser<Vec<A>> {
    Box::new(move |input| {
        let mut values: Vec<A> = vec![];
        let mut current = input.to_string();
        'parse: loop {
            match p(&current) {
                Ok(res) => {
                    values.push(res.value);
                    current = res.next;
                }
                _ => break 'parse,
            }
        }

        Ok(ParseSuccess {
            value: values,
            next: current,
        })
    })
}

/// Returns a parser which tries apply the given parser 1 or more times and
/// returns a vector of the parse values. If the parser does not match at least
/// once, the parser fails
/// # Arguments
/// * `p`: A parser to apply repeatedly
///
/// ```
/// use parser_combinators::combinators::text;
/// use parser_combinators::primitives::many;
/// use parser_combinators::parse;
///
/// let parser = many(text("abc"));
/// let parts = parse(parser, &String::from("abcabc"));
/// assert_eq!(parts.len(), 2);
/// assert_eq!(parts[0], "abc");
/// assert_eq!(parts[1], "abc");
/// ```
pub fn many1<A: 'static>(p: Parser<A>) -> Parser<Vec<A>> {
    Box::new(move |input| {
        let mut values: Vec<A> = vec![];
        let mut current = input.to_string();
        let mut first = true;
        'parse: loop {
            match p(&current) {
                Ok(res) => {
                    first = false;
                    values.push(res.value);
                    current = res.next;
                }
                Err(e) => {
                    if first {
                        return Err(e);
                    }
                    break 'parse;
                }
            }
        }

        Ok(ParseSuccess {
            value: values,
            next: current,
        })
    })
}

/// Returns a parser which first tries to apply `before`,
/// then `p` then `after`, and retuns the value parsed by
/// `p`
/// # Arguments
/// `p`: The parser whose successful parse value will be
/// returned
/// `before`: The parser which is applied first
/// `after`: The parser which is applied last
///
/// ```
/// use parser_combinators::primitives::between;
/// use parser_combinators::combinators::text;
/// use parser_combinators::parse;
///
/// let p = between(text("abc"), text("("), text(")"));
/// assert_eq!(parse(p, "(abc)"), "abc");
/// ```
pub fn between<A: 'static, B: 'static, C: 'static>(
    p: Parser<A>,
    before: Parser<B>,
    after: Parser<C>,
) -> Parser<A> {
    Box::new(move |inp| {
        let mut val: Option<A> = None;
        before(inp)
            .and_then(|first| p(&first.next))
            .and_then(|res| {
                val = Some(res.value);
                after(&res.next)
            })
            .map(|last| ParseSuccess {
                value: val.unwrap(),
                next: last.next,
            })
    })
}

// TODO: docs
pub fn not_followed_by<A: 'static + Clone, B: 'static>(p: Parser<A>, q: Parser<B>) -> Parser<A> {
    Box::new(move |input| {
        let v = p(input)?;
        let ret = v.clone();
        q(&v.next).map_or_else(
            |_| Ok(ret),
            |_| {
                Err(ParseError {
                    input: input.to_string(),
                    fatal: true,
                    expected: format!("something other than {}", v.next),
                })
            },
        )
    })
}

pub fn skip<A: 'static, B: 'static>(p: Parser<A>, q: Parser<B>) -> Parser<B> {
    Box::new(move |input| match p(input) {
        Ok(res) => q(&res.next),
        Err(_) => q(input),
    })
}

/// Returns a parser that always succeeds, either with a
/// Some() value or None. If the parser does not succeed,
/// no input is consumed
///
/// # Arguments
/// * `p`: A parser to try and apply
///
/// ```
/// use parser_combinators::primitives::maybe;
/// use parser_combinators::combinators::text;
/// use parser_combinators::parse;
///
/// let p = maybe(text("abc"));
/// assert_eq!(parse(p, "def"), None);
/// ```
pub fn maybe<A: 'static + Clone>(p: Parser<A>) -> Parser<Option<A>> {
    let to_some = map(p, Some);
    Box::new(move |input| {
        let original = input.to_string();
        to_some(input).or(Ok(ParseSuccess {
            value: None,
            next: original,
        }))
    })
}

#[cfg(test)]
mod tests {
    use super::super::combinators::*;
    use super::super::*;
    use super::*;

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum Output {
        Int(u64),
        Str(String),
        EOF,
    }

    #[test]
    fn seq_test() {
        let int_parser = map(integer(), |i| Output::Int(i));
        let string_parser = map(text("abc"), |t| Output::Str(t));

        let parts = parse(
            seq(vec![int_parser, string_parser]),
            &String::from("123abc"),
        );
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], Output::Int(123));
        assert_eq!(parts[1], Output::Str(String::from("abc")));
    }

    #[test]
    fn multi_seq_test() {
        let int_parser = map(integer(), |i| Output::Int(i));
        let string_parser1 = map(text("abc"), |s| Output::Str(s));
        let string_parser2 = map(text("xyz"), |s| Output::Str(s));
        let end = map(eof(), |_| Output::EOF);
        let p = seq(vec![int_parser, string_parser1, string_parser2, end]);

        let parts = parse(p, &String::from("123abcxyz"));
        assert_eq!(parts.len(), 4);
        assert_eq!(parts[0], Output::Int(123));
        assert_eq!(parts[1], Output::Str(String::from("abc")));
        assert_eq!(parts[2], Output::Str(String::from("xyz")));
        assert_eq!(parts[3], Output::EOF);
    }

    #[test]
    #[should_panic]
    fn seq_failure_1() {
        let p = seq(vec![text("abc"), text("xyz")]);
        parse(p, &String::from("abc"));
    }

    #[test]
    #[should_panic]
    fn seq_failure_2() {
        let p = seq(vec![text("abc"), text("xyz")]);
        parse(p, &String::from("xyz"));
    }

    #[test]
    fn or_test() {
        let ps = or(text("abc"), text("xyz"));
        assert_eq!(parse(ps, &String::from("abc")), "abc");
        //TODO: Avoid the move above
        let ps2 = or(text("abc"), text("xyz"));
        assert_eq!(parse(ps2, &String::from("xyz")), "xyz");
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected string abc or string xyz")]
    fn or_test_failure() {
        let ps = or(text("abc"), text("xyz"));
        parse(ps, &String::from("test"));
    }

    #[test]
    fn one_of_test() {
        let ps1 = one_of(vec![text("abc"), text("def"), text("xyz")]);
        assert_eq!(parse(ps1, &String::from("abc")), "abc");
        let ps2 = one_of(vec![text("abc"), text("def"), text("xyz")]);
        assert_eq!(parse(ps2, &String::from("def")), "def");
        let ps3 = one_of(vec![text("abc"), text("def"), text("xyz")]);
        assert_eq!(parse(ps3, &String::from("xyz")), "xyz");
        let ps4 = one_of(vec![text("abc"), text("def"), text("xyz")]);
        assert_eq!(parse(ps4, &String::from("xyzabc")), "xyz");
    }

    #[test]
    #[should_panic(
        expected = "Failed to parse input, expected one of string abc, string def, string xyz"
    )]
    fn one_of_test_failure() {
        let ps = one_of(vec![text("abc"), text("def"), text("xyz")]);
        parse(ps, &String::from("asd"));
    }

    #[test]
    fn many_test() {
        let p = many(text("abc,"));
        let parts = parse(p, &String::from("abc,abc,def"));
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], "abc,");
        assert_eq!(parts[1], "abc,");
    }

    #[test]
    fn many_no_entries_test() {
        let p = many(text("abc"));
        assert_eq!(parse(p, &String::from("xyz")).len(), 0);
    }

    #[test]
    fn many_seq_test() {
        let parsers = seq(vec![many(text("abc")), map(text("xyz"), |t| vec![t])]);
        let parts = parse(parsers, &String::from("abcabcxyz"));
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0].len(), 2);
        assert_eq!(parts[1].len(), 1);
    }

    #[test]
    fn many1_test() {
        let parts = parse(many1(text("abc")), &String::from("abcabc"));
        assert_eq!(parts.len(), 2);
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected string abc")]
    fn many1_test_failure() {
        parse(many1(text("abc")), &String::from("xyz"));
    }

    #[test]
    fn between_test() {
        assert_eq!(
            parse(
                between(text("abc"), text("("), text(")")),
                &String::from("(abc)")
            ),
            "abc"
        );
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected string (")]
    fn between_test_failure_1() {
        parse(
            between(text("abc"), text("("), text(")")),
            &String::from("abc)"),
        );
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected string abc")]
    fn between_test_failure_2() {
        parse(
            between(text("abc"), text("("), text(")")),
            &String::from("(xyz)"),
        );
    }
    #[test]
    #[should_panic(expected = "Failed to parse input, expected string )")]
    fn between_test_failure_3() {
        parse(
            between(text("abc"), text("("), text(")")),
            &String::from("(abc"),
        );
    }

    #[test]
    fn backtrack_or_test() {
        let p1 = seq(vec![text("abc"), text("xyz")]);
        let p2 = seq(vec![text("abc"), text("def")]);
        let parser = or(p1, p2);

        let parts = parse(parser, "abcdef");
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], "abc");
        assert_eq!(parts[1], "def");
    }

    #[test]
    fn backtrack_one_of_test() {
        let p1 = seq(vec![text("abc"), text("xyz")]);
        let p2 = seq(vec![text("abc"), text("def")]);
        let parser = one_of(vec![p1, p2]);

        let parts = parse(parser, "abcdef");
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], "abc");
        assert_eq!(parts[1], "def");
    }

    #[test]
    fn maybe_test() {
        // TODO
    }

    #[test]
    fn skip_test() {
        let p = skip(many(char(' ')), text("abc"));
        assert_eq!(parse(p, "    abc"), "abc");

        assert_eq!(parse(skip(many(char(' ')), text("abc")), "abc"), "abc");
    }

    #[test]
    fn not_followed_by_test() {
        assert_eq!(parse(not_followed_by(text("abc"), text(",")), "abc"), "abc");
    }

    #[test]
    #[should_panic]
    fn not_followed_by_test_failure() {
        parse(not_followed_by(text("abc"), text(",")), "abc,");
    }

    #[test]
    fn sep_by_test() {
        let p = sep_by(integer(), char(';'));
        let values = parse(p, "1;23;5;");
        assert_eq!(values.len(), 3);
        assert_eq!(values[0], 1);
        assert_eq!(values[1], 23);
        assert_eq!(values[2], 5);
    }
}
