pub use crate::list_of_regex_lines_parsed;
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
use std::convert::TryFrom;
use std::str::FromStr;

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

#[macro_export]
macro_rules! list_of_regex_lines_parsed {
    ($inp:ident, $string:expr) => {{
        use std::convert::TryInto;
        $crate::parsers::list_of_regex_lines($inp, $string)?
            .into_iter()
            .map(|x| $crate::parsers::VecParse(x).try_into())
            .collect::<Result<Vec<_>, _>>()?
    }};
}

// vec to tuple ///////////////////////////////////////////////////////////////

#[derive(Debug, thiserror::Error)]
pub enum VecParseError {
    #[error("Vec is of wrong length")]
    WrongLen,
    #[error("str at position {ele} failed with '{error}'")]
    ParseError {
        ele: usize,
        error: Box<dyn std::error::Error + Send + Sync + 'static>,
    },
}

pub struct VecParse<'a>(pub Vec<&'a str>);

macro_rules! args_to_tuple {
    ($name:ident) => {
        ($name ,)
    };
    ($($name:ident)+) => {
        ($($name),+)
    };
}

macro_rules! vec_parse_impls {
    ($($name:ident $ename:ident)+) => {
        impl<'a, $($name, $ename),+> TryFrom<VecParse<'a>> for args_to_tuple!($($name)+)
        where $($name: FromStr<Err = $ename>),+ ,
              $($ename: std::error::Error + Send + Sync + 'static),+
        {
            type Error = VecParseError;
            #[allow(unused_assignments, non_snake_case)]
            fn try_from(value: VecParse<'a>) -> Result<Self, Self::Error> {
                let mut i = 0;
                $(
                    if i >= value.0.len() {
                        return Err(VecParseError::WrongLen);
                    }

                    let $name = match value.0[i].parse() {
                        Ok(x) => x,
                        Err(e) => {
                            return Err(VecParseError::ParseError {
                                ele: i,
                                error: Box::new(e),
                            })
                        }
                    };
                    i += 1;
                )+

                Ok(args_to_tuple!($($name)+))
            }
        }
    };
}

vec_parse_impls! {A E1}
vec_parse_impls! {A E1 B E2}
vec_parse_impls! {A E1 B E2 C E3}
vec_parse_impls! {A E1 B E2 C E3 D E4}
vec_parse_impls! {A E1 B E2 C E3 D E4 E E5}
vec_parse_impls! {A E1 B E2 C E3 D E4 E E5 F E6}
vec_parse_impls! {A E1 B E2 C E3 D E4 E E5 F E6 G E7}
vec_parse_impls! {A E1 B E2 C E3 D E4 E E5 F E6 G E7 H E8}
