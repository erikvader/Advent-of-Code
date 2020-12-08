use anyhow;

pub fn part1(input: &str) -> anyhow::Result<String> {
    let numbers: Vec<i32> = input.lines().map(|x| x.parse()).collect::<Result<_, _>>()?;

    for i in 0..numbers.len() {
        for j in i..numbers.len() {
            if numbers[i] + numbers[j] == 2020 {
                return Ok((numbers[i] * numbers[j]).to_string());
            }
        }
    }
    anyhow::bail!("no numbers sum up to 2020");
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let numbers: Vec<i32> = input.lines().map(|x| x.parse()).collect::<Result<_, _>>()?;

    for i in 0..numbers.len() {
        for j in i..numbers.len() {
            for k in j..numbers.len() {
                if numbers[i] + numbers[j] + numbers[k] == 2020 {
                    return Ok((numbers[i] * numbers[j] * numbers[k]).to_string());
                }
            }
        }
    }
    anyhow::bail!("no numbers sum up to 2020");
}
