use std::collections::HashMap;

fn add_sums(xmas: &[u64], sums: &mut HashMap<u64, usize>, i: usize, j: usize) {
    for k in i..j {
        if xmas[j] == xmas[k] {
            continue;
        }
        let s = xmas[j] + xmas[k];
        sums.entry(s)
            .and_modify(|v| *v = std::cmp::max(*v, k))
            .or_insert(k);
    }
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let xmas = input
        .lines()
        .map(|x| x.parse())
        .collect::<Result<Vec<u64>, _>>()?;

    let preamble = 25;
    let mut sums = HashMap::<u64, usize>::new();

    for j in 1..preamble {
        add_sums(&xmas, &mut sums, 0, j);
    }

    let mut cur = preamble;
    let ans = loop {
        if sums
            .get(&xmas[cur])
            .map(|&i| i < cur - preamble)
            .unwrap_or(true)
        {
            break xmas[cur];
        }

        // NOTE: can also remove the old sums here
        add_sums(&xmas, &mut sums, cur - preamble + 1, cur);
        cur += 1;
    };

    Ok(ans.to_string())
}

fn cumsum(x: &[u64]) -> Vec<u64> {
    let mut cum = Vec::new();
    cum.push(x[0]);
    for i in x.iter().skip(1) {
        let last = *cum.last().unwrap();
        cum.push(last + i);
    }
    cum
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let xmas = input
        .lines()
        .map(|x| x.parse())
        .collect::<Result<Vec<u64>, _>>()?;

    let target = 507622668;
    let cum = cumsum(&xmas);
    for i in 0..cum.len() {
        let tofind = target + cum[i];
        if let Ok(j) = cum.binary_search(&tofind) {
            let weakness =
                xmas[i + 1..=j].iter().min().unwrap() + xmas[i + 1..=j].iter().max().unwrap();
            return Ok(weakness.to_string());
        }
    }

    anyhow::bail!("did not find an answer :(");
}
