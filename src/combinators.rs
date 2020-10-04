use super::primitives::{many, one_of};
use super::*;

pub fn integer() -> Parser<u64> {
    Box::new(|input| {
        let mut cs = input.chars();
        let num_str: String = cs.by_ref().take_while(|c| c.is_digit(10)).collect();

        if num_str.is_empty() {
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
            next: String::from(input.get(num_str.len()..).unwrap()),
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
                    expected: format!("string {}", text),
                    fatal: true,
                })
            }
        } else {
            Err(ParseError {
                input: input.to_string(),
                expected: format!("string {}", text),
                fatal: true,
            })
        }
    })
}

pub fn eof() -> Parser<()> {
    Box::new(move |input| {
        if input.is_empty() {
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

pub fn digit() -> Parser<u32> {
    Box::new(|input| {
        let mut cs = input.chars();
        cs.next()
            .map(|c| {
                if c.is_digit(10) {
                    Ok(ParseSuccess {
                        next: cs.collect(),
                        value: c.to_digit(10).unwrap(),
                    })
                } else {
                    Err(ParseError {
                        fatal: true,
                        expected: String::from("digit"),
                        input: input.to_string(),
                    })
                }
            })
            .or_else(|| {
                Some(Err(ParseError {
                    fatal: true,
                    expected: String::from("digit"),
                    input: input.to_string(),
                }))
            })
            .unwrap()
    })
}

pub fn char(c: char) -> Parser<char> {
    Box::new(move |input| {
        let mut cs = input.chars();
        cs.next()
            .map(|v| {
                if v == c {
                    Ok(ParseSuccess {
                        value: v,
                        next: cs.collect(),
                    })
                } else {
                    Err(ParseError {
                        fatal: true,
                        input: input.to_string(),
                        expected: format!("char {}", c),
                    })
                }
            })
            .or_else(|| {
                Some(Err(ParseError {
                    fatal: true,
                    input: input.to_string(),
                    expected: format!("char {}", c),
                }))
            })
            .unwrap()
    })
}

pub fn ws() -> Parser<char> {
    one_of(vec![char(' '), char('\t'), char('\n'), char('\r')])
}

#[cfg(test)]
mod tests {
    use super::primitives::*;
    use super::*;

    #[test]
    fn test_integer() {
        assert_eq!(parse(integer(), &String::from("123")), 123);
        assert_eq!(
            parse_with_next(integer(), &String::from("123abc"))
                .unwrap()
                .next,
            String::from("abc")
        );
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected integer")]
    fn test_integer_failure() {
        parse(integer(), &String::from("abc"));
    }

    #[test]
    fn test_text() {
        assert_eq!(parse(text("abc"), &String::from("abc")), "abc");
        assert_eq!(
            parse_with_next(text("abc"), &String::from("abc123"))
                .unwrap()
                .next,
            String::from("123")
        );
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected string xyz")]
    fn test_text_failure() {
        println!("{}", parse(text("xyz"), &String::from("abc")));
    }

    #[test]
    fn eof_test() {
        assert_eq!(parse(eof(), &String::from("")), ());
    }

    #[test]
    #[should_panic(expected = "Failed to parse input, expected end of input")]
    fn eof_test_failure() {
        parse(eof(), &String::from("abc"));
    }

    #[test]
    fn digit_test() {
        assert_eq!(parse(digit(), "123"), 1);
        let many_digits = parse(many(digit()), "123");
        assert_eq!(many_digits.len(), 3);
        assert_eq!(many_digits[0], 1);
        assert_eq!(many_digits[1], 2);
        assert_eq!(many_digits[2], 3);
    }

    #[test]
    fn char_test() {
        assert_eq!(parse(char('a'), "a"), 'a');
        assert_eq!(parse(char('a'), "ab"), 'a');
    }

    #[test]
    fn char_unicode_test() {
        assert_eq!(parse(char('💩'), "💩"), '💩');
    }
}
