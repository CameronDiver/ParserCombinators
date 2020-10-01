pub type Input = String;

#[derive(Debug)]
pub struct ParseError {
    pub input: Input,
    pub expected: String,
    pub fatal: bool,
}

#[derive(Debug)]
pub struct ParseSuccess<A> {
    pub value: A,
    pub next: Input,
}

pub type ParseResult<A> = Result<ParseSuccess<A>, ParseError>;

pub type Parser<A> = Box<dyn Fn(&Input) -> ParseResult<A>>;

pub fn parse<A>(p: Parser<A>, input: &Input) -> A {
    match p(input) {
        Ok(success) => success.value,
        Err(e) => panic!(format!("Failed to parse input, expected {}", e.expected)),
    }
}

pub fn parse_with_next<A>(p: Parser<A>, input: &Input) -> ParseResult<A> {
    let res = p(input);

    if let Err(e) = res {
        panic!(format!("Failed to parse input, expected {}", e.expected))
    }

    res
}
