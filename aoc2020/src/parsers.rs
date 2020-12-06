pub use crate::list_of_regex_lines_parsed;
use grid::Grid;
use regex::Regex;
use std::convert::TryFrom;
use std::str::FromStr;

// parser error ///////////////////////////////////////////////////////////////

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error(transparent)]
    Regex(#[from] regex::Error),
    #[error("the regex didn't match")]
    RegexNoMatch,
    #[error("a line failed to map because '{0}'")]
    MapError(Box<dyn std::error::Error + Send + Sync + 'static>),
}

// list of parseable //////////////////////////////////////////////////////////

#[allow(dead_code)]
pub fn map_sep<'a, F, T, E>(lines: &'a str, sep: &str, f: F) -> Result<Vec<T>, ParserError>
where
    F: Fn(&'a str) -> Result<T, E>,
    E: std::error::Error + Send + Sync + 'static,
{
    lines
        .trim_end()
        .split(sep)
        .map(f)
        .collect::<Result<_, _>>()
        .map_err(|e| ParserError::MapError(Box::new(e)))
}

#[allow(dead_code)]
pub fn map_lines<'a, F, T, E>(lines: &'a str, f: F) -> Result<Vec<T>, ParserError>
where
    F: Fn(&'a str) -> Result<T, E>,
    E: std::error::Error + Send + Sync + 'static,
{
    map_sep(lines, "\n", f)
}

#[allow(dead_code)]
pub fn map_paragraphs<'a, F, T, E>(lines: &'a str, f: F) -> Result<Vec<T>, ParserError>
where
    F: Fn(&'a str) -> Result<T, E>,
    E: std::error::Error + Send + Sync + 'static,
{
    map_sep(lines, "\n\n", f)
}

#[allow(dead_code)]
pub fn list_of_parseable<T, E>(s: &str, sep: &str) -> Result<Vec<T>, ParserError>
where
    T: std::str::FromStr<Err = E>,
    E: std::error::Error + Send + Sync + 'static,
{
    map_sep(s, sep, |x| x.parse())
}

#[allow(dead_code)]
pub fn list_of_parseable_lines<T, E>(s: &str) -> Result<Vec<T>, ParserError>
where
    T: std::str::FromStr<Err = E>,
    E: std::error::Error + Send + Sync + 'static,
{
    list_of_parseable(s, "\n")
}

// safe maps //////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub fn map_sep_safe<'a, F, T>(lines: &'a str, sep: &str, f: F) -> Result<Vec<T>, ParserError>
where
    F: Fn(&'a str) -> T,
{
    map_sep::<_, _, std::convert::Infallible>(lines, sep, |x| Ok(f(x)))
}

#[allow(dead_code)]
pub fn map_lines_safe<'a, F, T>(lines: &'a str, f: F) -> Result<Vec<T>, ParserError>
where
    F: Fn(&'a str) -> T,
{
    map_sep_safe(lines, "\n", f)
}

#[allow(dead_code)]
pub fn map_paragraphs_safe<'a, F, T>(lines: &'a str, f: F) -> Result<Vec<T>, ParserError>
where
    F: Fn(&'a str) -> T,
{
    map_sep_safe(lines, "\n\n", f)
}

// list of regex //////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub fn list_of_regex_lines<'a>(s: &'a str, regex: &str) -> Result<Vec<Vec<&'a str>>, ParserError> {
    let reg = Regex::new(&format!("(?m){}", regex))?;

    map_sep(s, "\n", |l| {
        reg.captures(l)
            .and_then(|caps| {
                caps.iter()
                    .skip(1)
                    .map(|g| g.map(|g| g.as_str()))
                    .collect::<Option<Vec<&'a str>>>()
            })
            .ok_or(ParserError::RegexNoMatch)
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

// grid ///////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub fn char_grid(s: &str) -> Result<Grid<char>, ParserError> {
    let mut flattened = Vec::new();
    let mut cols = None;
    for l in s.lines() {
        cols = Some(l.len()); // TODO: check whether line lenghts are different
        for c in l.chars() {
            flattened.push(c);
        }
    }

    if cols.is_none() {
        return Ok(grid::grid![]);
    }
    Ok(Grid::from_vec(flattened, cols.unwrap()))
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
