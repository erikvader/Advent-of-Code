use std::str::FromStr;

pub struct CSV<T>(pub Vec<T>);

#[derive(Debug)]
pub struct CSVError<E>(E);

impl<E: std::fmt::Display> std::fmt::Display for CSVError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "couldn't parse CSV because '{}'", self.0)
    }
}

impl<E: std::fmt::Display + std::fmt::Debug> std::error::Error for CSVError<E> {}

impl<T, E> FromStr for CSV<T>
where
    T: FromStr<Err = E>,
{
    type Err = CSVError<E>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let contents: Result<Vec<T>, _> =
            s.split(",").map(|e| e.trim()).map(|e| e.parse()).collect();

        contents.map_err(|e| CSVError(e)).map(|v| CSV(v))
    }
}
