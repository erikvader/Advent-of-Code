use anyhow;
use std::str::FromStr;

pub struct CSV<T>(pub Vec<T>);

impl<T, E> FromStr for CSV<T>
where
    T: FromStr<Err = E>,
    E: Send + Sync + 'static + std::error::Error,
{
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let contents: Result<Vec<T>, _> =
            s.split(",").map(|e| e.trim()).map(|e| e.parse()).collect();

        Ok(CSV(contents?))
    }
}
