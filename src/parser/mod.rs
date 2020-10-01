#![feature(drain_filter)]
pub type Input = String;

#[derive(Debug)]
pub struct ParseError {
    input: Input,
    expected: String,
    fatal: bool,
}

#[derive(Debug)]
pub struct ParseSuccess<A> {
    value: A,
    next: Input,
}

pub type ParseResult<A> = Result<ParseSuccess<A>, ParseError>;

pub type Parser<A> = Box<dyn Fn(&mut Input) -> ParseResult<A>>;

pub fn integer() -> Parser<u64> {
    Box::new(|input: &mut String| {
        let mut cs = input.chars();
        let num_str: String = cs.by_ref().take_while(|c| c.is_digit(10)).collect();

        if num_str.len() == 0 {
            return Err(ParseError {
                input: input.to_string(),
                expected: String::from("integer"),
                fatal: true,
            });
        }

        let num = num_str.parse::<u64>().unwrap();

        Ok(ParseSuccess {
            value: num,
            // TODO: Better way to skip
            next: input.chars().skip(num_str.len()).collect(),
        })
    })
}

pub fn text(text: &'static str) -> Parser<String> {
    Box::new(move |input| {
        if let Some(val) = input.get(..text.len()) {
            if val == text {
                Ok(ParseSuccess {
                    value: String::from(text),
                    next: input.chars().skip(text.len()).collect(),
                })
            } else {
                Err(ParseError {
                    input: input.to_string(),
                    expected: String::from(format!("string {}", text)),
                    fatal: true,
                })
            }
        } else {
            Err(ParseError {
                input: input.to_string(),
                expected: String::from(format!("string {}", text)),
                fatal: true,
            })
        }
    })
}

pub fn eof() -> Parser<()> {
    Box::new(move |input| {
        if input.len() == 0 {
            Ok(ParseSuccess {
                value: (),
                next: String::from(""),
            })
        } else {
            Err(ParseError {
                input: input.to_string(),
                expected: String::from("end of input"),
                fatal: true,
            })
        }
    })
}

pub fn seq<A: 'static + Clone>(p: Parser<A>, q: Parser<A>) -> Parser<Vec<A>> {
    Box::new(move |input| match p(input) {
        Ok(mut res) => match q(&mut res.next) {
            Ok(res2) => {
                return Ok(ParseSuccess {
                    value: vec![res.value.clone(), res2.value.clone()],
                    next: res2.next,
                });
            }
            Err(e) => Err(e),
        },
        Err(e) => Err(e),
    })
}

fn apply<A: 'static, B: 'static>(p: Parser<A>, func: fn(A) -> B) -> Parser<B> {
    Box::new(move |input| match p(input) {
        Ok(res) => Ok(ParseSuccess {
            value: func(res.value),
            next: res.next,
        }),
        Err(e) => Err(e),
    })
}

pub fn parse<A>(p: Parser<A>, input: &mut Input) -> A {
    match p(input) {
        Ok(success) => success.value,
        Err(e) => panic!(format!("Failed to parse input, expected {}", e.expected)),
    }
}

pub fn parse_with_next<A>(p: Parser<A>, input: &mut Input) -> ParseResult<A> {
    let res = p(input);

    if let Err(e) = res {
        panic!(format!("Failed to parse input, expected {}", e.expected))
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer() {
        assert_eq!(parse(integer(), &mut String::from("123")), 123);
        assert_eq!(
            parse_with_next(integer(), &mut String::from("123abc"))
                .unwrap()
                .next,
            String::from("abc")
        );
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected integer")]
    fn test_integer_failure() {
        parse(integer(), &mut String::from("abc"));
    }

    #[test]
    fn test_text() {
        assert_eq!(parse(text("abc"), &mut String::from("abc")), "abc");
        assert_eq!(
            parse_with_next(text("abc"), &mut String::from("abc123"))
                .unwrap()
                .next,
            String::from("123")
        );
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected string xyz")]
    fn test_text_failure() {
        println!("{}", parse(text("xyz"), &mut String::from("abc")));
    }

    #[test]
    fn eof_test() {
        assert_eq!(parse(eof(), &mut String::from("")), ());
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected end of input")]
    fn eof_test_failure() {
        parse(eof(), &mut String::from("abc"));
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum Output {
        Int(u64),
        Str(String),
    }

    #[test]
    fn seq_test() {
        let int_parser = apply(integer(), |i| Output::Int(i));
        let string_parser = apply(text("abc"), |t| Output::Str(t));

        let parts = parse(seq(int_parser, string_parser), &mut String::from("123abc"));
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], Output::Int(123));
        assert_eq!(parts[1], Output::Str(String::from("abc")));
    }

    #[test]
    fn multi_seq_test() {
        let int_parser = apply(integer(), |i| Output::Int(i));
        let string_parser1 = text("abc");
        let string_parser2 = text("xyz");
        let two_strings = apply(seq(string_parser1, string_parser2), |strs: Vec<String>| {
            Output::Str(strs.join(""))
        });
        let parts = parse(seq(int_parser, two_strings), &mut String::from("123abcxyz"));
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], Output::Int(123));
        assert_eq!(parts[1], Output::Str(String::from("abcxyz")));
    }
}
