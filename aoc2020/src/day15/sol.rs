use std::collections::HashMap;

fn doit(input: &str, limit: u64) -> anyhow::Result<String> {
    let starting = input
        .trim_end()
        .split(',')
        .map(|n| n.parse::<u64>())
        .collect::<Result<Vec<_>, _>>()?;

    let mut lasts = HashMap::<u64, u64>::new();
    let (l, starting) = starting.split_last().unwrap();
    let mut say_next = *l;
    let mut i = 1;

    for sn in starting.iter() {
        lasts.insert(*sn, i);
        i += 1;
    }

    while i < limit {
        match lasts.get(&say_next) {
            None => {
                lasts.insert(say_next, i);
                say_next = 0;
            }
            Some(&j) => {
                let speak = i - j;
                lasts.insert(say_next, i);
                say_next = speak;
            }
        }
        i += 1;
    }

    Ok(say_next.to_string())
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    doit(input, 2020)
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    doit(input, 30_000_000)
}
