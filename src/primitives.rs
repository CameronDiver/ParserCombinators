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
pub fn seq<A: 'static>(parsers: Vec<Parser<A>>) -> Parser<Vec<A>> {
    Box::new(move |input| {
        let start = ParseSuccess {
            value: vec![],
            next: input.to_string(),
        };

        parsers.iter().try_fold(start, |mut iter, p| {
            p(&iter.next).and_then(|v| {
                iter.value.push(v.value);
                Ok(ParseSuccess {
                    value: iter.value,
                    next: v.next,
                })
            })
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
pub fn map<A: 'static, B: 'static>(p: Parser<A>, func: fn(A) -> B) -> Parser<B> {
    Box::new(move |input| match p(input) {
        Ok(res) => Ok(ParseSuccess {
            value: func(res.value),
            next: res.next,
        }),
        Err(e) => Err(e),
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
    Box::new(move |input| match p(input) {
        Ok(res) => Ok(res),
        Err(e) => match q(input) {
            Ok(res) => Ok(res),
            Err(e2) => Err(ParseError {
                expected: format!("{} or {}", e.expected, e2.expected),
                fatal: true,
                input: input.to_string(),
            }),
        },
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

        return Err(ParseError {
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
        });
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
        let mut current = input.clone();
        'parse: loop {
            match p(&current) {
                Ok(res) => {
                    values.push(res.value);
                    current = res.next.clone();
                }
                _ => break 'parse,
            }
        }

        return Ok(ParseSuccess {
            value: values,
            next: current.to_string(),
        });
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
        let mut current = input.clone();
        let mut first = true;
        'parse: loop {
            match p(&current) {
                Ok(res) => {
                    first = false;
                    values.push(res.value);
                    current = res.next.clone();
                }
                Err(e) => {
                    if first {
                        return Err(e);
                    }
                    break 'parse;
                }
            }
        }

        return Ok(ParseSuccess {
            value: values,
            next: current.to_string(),
        });
    })
}

// TODO: Docs
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
            .and_then(|last| {
                Ok(ParseSuccess {
                    value: val.unwrap(),
                    next: last.next.to_string(),
                })
            })
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
}
