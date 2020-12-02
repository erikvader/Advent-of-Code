use anyhow::{self, Context};
use std::str::FromStr;

fn separated_values_parser<T, E>(sep: &str, s: &str) -> anyhow::Result<Vec<T>>
where
    T: FromStr<Err = E>,
    E: Send + Sync + 'static + std::error::Error,
{
    let contents: Result<Vec<T>, _> = s.split(sep).map(|e| e.trim()).map(|e| e.parse()).collect();
    Ok(contents?)
}

pub struct CSV<T>(pub Vec<T>);

impl<T, E> FromStr for CSV<T>
where
    T: FromStr<Err = E>,
    E: Send + Sync + 'static + std::error::Error,
{
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(CSV(separated_values_parser(",", s).context("couldn't parse CSV")?))
    }
}

pub struct LSV<T>(pub Vec<T>);

impl<T, E> FromStr for LSV<T>
where
    T: FromStr<Err = E>,
    E: Send + Sync + 'static + std::error::Error,
{
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(LSV(separated_values_parser("\n", s).context("couldn't parse LSV")?))
    }
}
