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

// parallel assignment
macro_rules! para_ass {
    ($a:ident, $b:ident = $aa:expr, $bb:expr) => {
        let (a, b) = ($aa, $bb);
        $a = a;
        $b = b;
    };
}

// extended_gcd(a, b) = (gcd(a, b), x, y)
// ax + by = gcd(a, b)
// not sure what happens if a and b are negative
fn extended_gcd(a: i64, b: i64) -> (i64, i64, i64) {
    #![allow(clippy::many_single_char_names)]
    let (mut old_r, mut r) = (a, b);
    let (mut old_s, mut s) = (1, 0);
    let (mut old_t, mut t) = (0, 1);

    while r != 0 {
        let q = old_r / r;
        para_ass! {old_r, r = r, old_r - q * r}
        para_ass! {old_s, s = s, old_s - q * s}
        para_ass! {old_t, t = t, old_t - q * t}
    }

    (old_r, old_s, old_t)
}

// (a * b) % m
// https://www.geeksforgeeks.org/multiply-large-integers-under-large-modulo/
fn mul_modulo(a: i64, b: i64, m: i64) -> i64 {
    assert!(m > 0, "modulo must be a positive number");
    let sign = a.is_negative() ^ b.is_negative();

    let mut apos = a.abs() % m;
    let mut bpos = b.abs();
    let mut res = 0;

    while bpos > 0 {
        if bpos & 1 > 0 {
            res = (res + apos) % m;
        }

        apos = (2 * apos) % m;

        bpos >>= 1;
    }

    res * if sign { -1 } else { 1 }
}

// Sames as `chinese_remainder` but doesn't require that all a are positive (nor smaller than n??).
// Also doesn't matter if system is sorted in any way or not.
// Inspired from the idea that the system of congruences can be rewritten as:
//   x = a1 + x1*n1
//   x = a2 + x2*n2
//   ...
//   x = ak + xk*nk
// A pair can be combined by putting them equal to each other and solving for x1, x2 and x (ac) with
// extended gcd. The combined pair is then x = ac + x3*n1*n2. This is done until there is only one
// equation left which will have the trivial solution ak, which is the final answer.
fn chinese_remainder2(system: &[(i64, i64)]) -> i64 {
    if system.is_empty() {
        return 0;
    }

    let (&(mut nfinal, mut afinal), rest) = system.split_first().unwrap();

    for &(n, a) in rest {
        let (gcd, alpha, _beta) = extended_gcd(nfinal, n);
        assert!(gcd == 1, "all n aren't coprime");
        let xfinal = (a - afinal) * alpha;
        let newnfinal = nfinal * n;
        afinal += mul_modulo(xfinal, nfinal, newnfinal);
        afinal %= newnfinal;
        nfinal = newnfinal;
    }

    if afinal < 0 {
        afinal + nfinal
    } else {
        afinal
    }
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
#[allow(dead_code)]
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
    let offsets = buses
        .into_iter()
        .enumerate()
        .filter(|(_, b)| b.is_some())
        .map(|(i, b)| (i as i64, b.unwrap() as i64))
        // .map(|(i, b)| (b, (((-i) % b) + b) % b)) // NOTE: required for cr1
        .map(|(i, b)| (b, -i))
        .collect::<Vec<(i64, i64)>>();

    // offsets.sort_unstable(); // NOTE: required for cr1
    // offsets.reverse();

    let ans = chinese_remainder2(&offsets);
    Ok(ans.to_string())
}
