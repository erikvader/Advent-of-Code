#[allow(unused_imports)]
use nom::{
    bytes::complete::tag,
    character::complete::{char, digit1, none_of, one_of},
    combinator::{all_consuming, cut, map_res, recognize},
    error::{Error, ErrorKind},
    multi::{many0, separated_list1},
    regexp::str::re_capture,
    Finish, IResult,
};
use regex::Regex;

// list of parseable //////////////////////////////////////////////////////////

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("couldn't parse file because '{kind:?}'")]
    Nom { kind: ErrorKind },
    #[error("couldn't parse line '{line}' because '{parse}'")]
    NomParse {
        line: String,
        kind: ErrorKind,
        parse: Box<dyn std::error::Error + Send + Sync + 'static>,
    },
    #[error(transparent)]
    Regex(#[from] regex::Error),
}

impl ParserError {
    fn get_line(_input: &str) -> String {
        // TODO: get the first line of input
        String::new()
    }
}

impl nom::error::ParseError<&str> for ParserError {
    fn from_error_kind(_input: &str, kind: ErrorKind) -> Self {
        ParserError::Nom { kind: kind }
    }

    fn append(_input: &str, _kind: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<E> nom::error::FromExternalError<&str, E> for ParserError
where
    E: std::error::Error + Send + Sync + 'static,
{
    fn from_external_error(input: &str, kind: ErrorKind, e: E) -> Self {
        ParserError::NomParse {
            line: ParserError::get_line(input),
            kind: kind,
            parse: Box::new(e),
        }
    }
}

pub fn list_of_parseable<T, E>(s: &str, sep: &str) -> Result<Vec<T>, ParserError>
where
    T: std::str::FromStr<Err = E>,
    E: std::error::Error + Send + Sync + 'static,
{
    let r: IResult<&str, Vec<T>, ParserError> = all_consuming(separated_list1(
        one_of(sep),
        cut(map_res(recognize(many0(none_of(sep))), |x: &str| {
            x.parse::<T>()
        })),
    ))(s);

    r.finish().map(|x| x.1)
}

pub fn list_of_parseable_lines<T, E>(s: &str) -> Result<Vec<T>, ParserError>
where
    T: std::str::FromStr<Err = E>,
    E: std::error::Error + Send + Sync + 'static,
{
    list_of_parseable(s, "\n")
}

// list of regex //////////////////////////////////////////////////////////////

pub fn list_of_regex_lines<'a>(s: &'a str, regex: &str) -> Result<Vec<Vec<&'a str>>, ParserError> {
    let reg = Regex::new(&format!("(?m){}", regex))?;

    let r: IResult<&'a str, Vec<Vec<&'a str>>, ParserError> =
        all_consuming(separated_list1(one_of("\n"), cut(re_capture(reg))))(s);

    r.finish().map(|x| {
        let mut lines = x.1;
        lines.iter_mut().for_each(|l| {
            l.remove(0);
            ()
        });
        lines
    })
}
