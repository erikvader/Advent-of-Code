use crate::parsers as P;
use anyhow;
use std::collections::HashMap;

fn hashmapify<'a>(record: &'a str) -> HashMap<&'a str, &'a str> {
    record
        .split_whitespace()
        .map(|kv| {
            kv.find(":")
                .map(|i| {
                    let (k, v) = kv.split_at(i);
                    (k, &v[1..])
                })
                .unwrap()
        })
        .collect()
}

fn remove_invalid(records: &mut Vec<HashMap<&str, &str>>) {
    records.retain(|l| {
        let wc = l.len();
        wc == 8 || wc == 7 && !l.contains_key("cid")
    })
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let mut records = P::map_paragraphs_safe(input, hashmapify)?;

    remove_invalid(&mut records);

    Ok(records.len().to_string())
}

fn test_num(n: &str, lb: i32, ub: i32) -> bool {
    match n.parse::<i32>() {
        Ok(n) => n >= lb && n <= ub,
        _ => false,
    }
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let mut records = P::map_paragraphs_safe(input, hashmapify)?;
    remove_invalid(&mut records);

    let count = records
        .into_iter()
        .filter(|r| {
            let hgt = match r.get("hgt") {
                Some(cm) if cm.ends_with("cm") => test_num(&cm[..cm.len() - 2], 150, 193),
                Some(inch) if inch.ends_with("in") => test_num(&inch[..inch.len() - 2], 59, 76),
                _ => false,
            };

            let hcl = {
                let h = r.get("hcl").unwrap();
                h.len() == 7
                    && h.starts_with("#")
                    && h.chars().skip(1).all(|c| c.is_ascii_alphanumeric())
            };

            let ecl =
                ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(r.get("ecl").unwrap());

            let pid = {
                let p = r.get("pid").unwrap();
                p.len() == 9 && p.chars().all(|c| c.is_ascii_hexdigit())
            };

            test_num(r.get("byr").unwrap(), 1920, 2002)
                && test_num(r.get("iyr").unwrap(), 2010, 2020)
                && test_num(r.get("eyr").unwrap(), 2020, 2030)
                && hgt
                && hcl
                && ecl
                && pid
        })
        .count();

    Ok(count.to_string())
}
