use super::*;

pub fn integer() -> Parser<u64> {
    Box::new(|input| {
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
