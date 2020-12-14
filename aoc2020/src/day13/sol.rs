fn parse_input(input: &str) -> (i32, Vec<Option<i32>>) {
    let mut ls = input.lines();
    let earliest_depart = ls.next().unwrap().parse().unwrap();
    let buses = ls
        .next()
        .unwrap()
        .split(',')
        .map(|b| {
            if b == "x" {
                None
            } else {
                Some(b.parse().unwrap())
            }
        })
        .collect();
    (earliest_depart, buses)
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let (ed, buses) = parse_input(input);

    let (timeleft, id) = buses
        .into_iter()
        .filter_map(|b| b)
        .map(|b| (b - (ed % b), b))
        .min()
        .unwrap();

    let ans = timeleft * id;

    Ok(ans.to_string())
}

// Solves a system of congruences
//   x ≡ a1 (mod n1)
//   x ≡ a2 (mod n2)
//   ...
//   x ≡ ak (mod nk)
// i.e. find an `x` that satisfies all of congruenses in `system`.
// system is a slice &[(n, a)] with all n > 0 and 0 <= ak < nk.
// All n have to be pairwise coprime with eachother.
// Solves faster with the larger n at the beginning.
// The returned value is 0 <= x < n1*n2...*nk and is the only answer in this range.
// Taken from https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving
fn chinese_remainder(system: &[(i64, i64)]) -> i64 {
    if system.is_empty() {
        return 0;
    }

    let (&(n1, a1), rest) = system.split_first().unwrap();
    let mut x = a1 % n1;
    let mut step = n1;

    for &(n, a) in rest {
        while x % n != a {
            x += step;
        }
        step *= n;
    }

    x
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let (_, buses) = parse_input(input);
    let mut offsets = buses
        .into_iter()
        .enumerate()
        .filter(|(_, b)| b.is_some())
        .map(|(i, b)| (i as i64, b.unwrap() as i64))
        .map(|(i, b)| (b, (((-i) % b) + b) % b))
        .collect::<Vec<(i64, i64)>>();

    offsets.sort_unstable();
    offsets.reverse();

    let ans = chinese_remainder(&offsets);
    Ok(ans.to_string())
}
