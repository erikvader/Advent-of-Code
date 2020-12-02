use anyhow;

pub fn part1(input: &str) -> anyhow::Result<String> {
    let inp: crate::CSV<i32> = input.parse()?;
    let numbers = inp.0;
    Ok(numbers.into_iter().sum::<i32>().to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    Ok("hej".to_string())
}
