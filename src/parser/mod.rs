pub mod combinators;
pub mod parser;
pub mod primitives;

#[cfg(test)]
mod tests {
    use super::combinators::*;
    use super::parser::*;

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
}
