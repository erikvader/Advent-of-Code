fn read_adapters(input: &str) -> anyhow::Result<Vec<u32>> {
    let mut adapters = input
        .lines()
        .map(|x| x.parse())
        .collect::<Result<Vec<u32>, _>>()?;

    adapters.sort_unstable();
    adapters.insert(0, 0);
    adapters.push(adapters.last().unwrap() + 3);

    Ok(adapters)
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let adapters = read_adapters(input)?;

    let mut ones = 0;
    let mut threes = 0;

    for lr in adapters.windows(2) {
        if let [l, r] = lr {
            if r - l == 1 {
                ones += 1;
            } else if r - l == 3 {
                threes += 1;
            }
        } else {
            unreachable!();
        }
    }

    let ans = ones * threes;

    Ok(ans.to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let adapters = read_adapters(input)?;

    let mut memo = vec![0u64; adapters.len()];
    *memo.last_mut().unwrap() = 1;

    for i in (0..memo.len() - 1).rev() {
        let mut j = 1;
        let mut combs = 0;
        while j <= 3 && i + j < memo.len() && adapters[i] + 3 >= adapters[i + j] {
            combs += memo[i + j];
            j += 1;
        }
        memo[i] = combs;
    }

    Ok(memo.first().unwrap().to_string())
}
