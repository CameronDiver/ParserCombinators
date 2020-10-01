pub mod combinators;
pub mod parser;
pub mod primitives;

#[cfg(test)]
mod tests {
    use super::combinators::*;
    use super::parser::*;
    use super::primitives::*;

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

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum Output {
        Int(u64),
        Str(String),
        EOF,
    }

    #[test]
    fn seq_test() {
        let int_parser = apply(integer(), |i| Output::Int(i));
        let string_parser = apply(text("abc"), |t| Output::Str(t));

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
        let int_parser = apply(integer(), |i| Output::Int(i));
        let string_parser1 = apply(text("abc"), |s| Output::Str(s));
        let string_parser2 = apply(text("xyz"), |s| Output::Str(s));
        let end = apply(eof(), |_| Output::EOF);
        let p = seq(vec![int_parser, string_parser1, string_parser2, end]);

        let parts = parse(p, &String::from("123abcxyz"));
        assert_eq!(parts.len(), 4);
        assert_eq!(parts[0], Output::Int(123));
        assert_eq!(parts[1], Output::Str(String::from("abc")));
        assert_eq!(parts[2], Output::Str(String::from("xyz")));
        assert_eq!(parts[3], Output::EOF);
    }
}
