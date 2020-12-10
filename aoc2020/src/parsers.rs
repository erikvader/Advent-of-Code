pub use crate::parse_regex_lines;
use grid::Grid;
use regex::Regex;
use std::convert::TryFrom;
use std::iter::Iterator;
use std::str::FromStr;

pub fn paragraphs(s: &str) -> impl Iterator<Item = &str> {
    s.trim_end().split("\n\n")
}

pub fn regex_capture<'a>(input: &'a str, regex: &Regex) -> Vec<Option<&'a str>> {
    regex.captures(input).map_or_else(Vec::new, |groupiter| {
        groupiter
            .iter()
            .skip(1)
            .map(|optmat| optmat.map(|mat| mat.as_str()))
            .collect::<Vec<Option<&'a str>>>()
    })
}

pub fn regex_capture_require<'a>(input: &'a str, regex: &Regex) -> Vec<&'a str> {
    regex_capture(input, regex)
        .into_iter()
        .map(|x| x.expect("capturing group is not allowed to be optional"))
        .collect()
}

#[macro_export]
macro_rules! parse_regex_lines {
    ($inp:ident, $string:expr) => {{
        use regex::Regex;
        use std::convert::TryInto;
        let reg = Regex::new($string)?;

        $inp.lines()
            .map(|l| $crate::parsers::regex_capture_require(l, &reg))
            .map(|x| $crate::parsers::VecParse(x).try_into())
            .collect::<Result<Vec<_>, _>>()?
    }};
}

// grid ///////////////////////////////////////////////////////////////////////

#[allow(dead_code)]
pub fn char_grid(s: &str) -> Option<Grid<char>> {
    let mut flattened = Vec::new();
    let mut cols = None;
    for l in s.lines() {
        cols = Some(l.len()); // TODO: check whether line lenghts are different
        for c in l.chars() {
            flattened.push(c);
        }
    }

    if cols.is_none() {
        return Some(grid::grid![]);
    }
    Some(Grid::from_vec(flattened, cols.unwrap()))
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

pub struct VecParse<T>(pub Vec<T>);

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
        impl<'a, $($name, $ename),+> TryFrom<VecParse<&'a str>> for args_to_tuple!($($name)+)
        where $($name: FromStr<Err = $ename>),+ ,
              $($ename: std::error::Error + Send + Sync + 'static),+
        {
            type Error = VecParseError;
            #[allow(unused_assignments, non_snake_case)]
            fn try_from(value: VecParse<&'a str>) -> Result<Self, Self::Error> {
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
