use super::parser::{ParseSuccess, Parser};

pub fn seq<A: 'static>(parsers: Vec<Parser<A>>) -> Parser<Vec<A>> {
    Box::new(move |input| {
        let start = ParseSuccess {
            value: vec![],
            next: input.to_string(),
        };
        parsers.iter().fold(Ok(start), |iter, p| match iter {
            Ok(mut success) => match p(&mut success.next) {
                Ok(s) => {
                    success.value.push(s.value);
                    Ok(ParseSuccess {
                        value: success.value,
                        next: s.next,
                    })
                }
                Err(e) => Err(e),
            },
            Err(e) => Err(e),
        })
    })
}

pub fn apply<A: 'static, B: 'static>(p: Parser<A>, func: fn(A) -> B) -> Parser<B> {
    Box::new(move |input| match p(input) {
        Ok(res) => Ok(ParseSuccess {
            value: func(res.value),
            next: res.next,
        }),
        Err(e) => Err(e),
    })
}
