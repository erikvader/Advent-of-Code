use crate::parsers as P;
use anyhow;
use std::collections::{HashMap, HashSet};

pub fn part1(input: &str) -> anyhow::Result<String> {
    let summan: usize = P::paragraphs(input)
        .map(|p| {
            p.chars()
                .filter(|&c| c != '\n')
                .collect::<HashSet<char>>()
                .len()
        })
        .into_iter()
        .sum();

    Ok(summan.to_string())
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let summan: usize = P::paragraphs(input)
        .map(|p| {
            let lc = p.lines().count();
            let mut counts = HashMap::new();
            for c in p.chars().filter(|&c| c != '\n') {
                counts.entry(c).and_modify(|e| *e += 1).or_insert(1);
            }
            counts.into_iter().filter(|(_, v)| *v == lc).count()
        })
        .into_iter()
        .sum();

    Ok(summan.to_string())
}
