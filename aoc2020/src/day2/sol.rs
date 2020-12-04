use crate::parsers as P;
use anyhow;

pub fn part1(input: &str) -> anyhow::Result<String> {
    let lines = P::list_of_regex_lines(input, r"^([0-9]+)-([0-9]+) ([a-zA-z]): ([a-zA-z]+)$")?;

    let count = lines
        .into_iter()
        .map(|x| {
            (
                x[0].parse::<usize>().unwrap(),
                x[1].parse::<usize>().unwrap(),
                x[2].parse::<char>().unwrap(),
                x[3],
            )
        })
        .filter(|(lb, up, c, s)| {
            let count = s.chars().filter(|x| x == c).count();
            count >= *lb && count <= *up
        })
        .count();

    Ok(count.to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let lines = P::list_of_regex_lines(input, r"^([0-9]+)-([0-9]+) ([a-zA-z]): ([a-zA-z]+)$")?;

    let count = lines
        .into_iter()
        .map(|x| {
            (
                x[0].parse::<usize>().unwrap(),
                x[1].parse::<usize>().unwrap(),
                x[2].parse::<char>().unwrap(),
                x[3],
            )
        })
        .filter(|(lb, up, c, s)| {
            (s.as_bytes()[*lb - 1] as char == *c) ^ (s.as_bytes()[*up - 1] as char == *c)
        })
        .count();

    Ok(count.to_string())
}
