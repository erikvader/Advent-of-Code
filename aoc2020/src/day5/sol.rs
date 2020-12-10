fn bs(lr: &[u8], upchar: u8, mut lb: u32, mut ub: u32) -> u32 {
    for &x in lr.iter().take(lr.len() - 1) {
        let mid = lb + (ub - lb) / 2;
        if x == upchar {
            lb = mid + 1;
        } else {
            ub = mid;
        }
    }

    if *lr.last().unwrap() == upchar {
        ub
    } else {
        lb
    }
}

fn seat_id(bsp: &str) -> u32 {
    let row = bs(&bsp.as_bytes()[..7], b'B', 0, 127);
    let col = bs(&bsp.as_bytes()[7..], b'R', 0, 7);
    row * 8 + col
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let m = input.lines().map(seat_id).max();
    Ok(m.unwrap().to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let mut seat_ids = input.lines().map(seat_id).collect::<Vec<_>>();
    seat_ids.sort_unstable();

    for i in 0..seat_ids.len() - 1 {
        if seat_ids[i] == seat_ids[i + 1] - 2 {
            return Ok((seat_ids[i] + 1).to_string());
        }
    }

    anyhow::bail!("no answer found");
}
