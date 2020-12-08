use anyhow;
use regex::Regex;
use std::collections::{HashMap, HashSet};

const TARGETBAG: &str = "shiny gold";

fn mark<'a>(
    cur: &'a str,
    marked: &mut HashSet<&'a str>,
    tree: &'a HashMap<String, Vec<(u32, String)>>,
) -> bool {
    if marked.contains(cur) {
        return true;
    }

    let mut mark_myself = false;

    for c in tree.get(cur).unwrap().iter() {
        if mark(&c.1, marked, tree) {
            mark_myself = true;
        }
    }

    if mark_myself {
        marked.insert(cur);
    }
    mark_myself
}

fn parse_tree(input: &str) -> anyhow::Result<HashMap<String, Vec<(u32, String)>>> {
    let mainreg = Regex::new(r"(?m)^(.+?) bags contain (.+?)\.$")?;
    let depsreg = Regex::new(r" *([0-9]+) (.+?) bags?")?;

    let res: Option<HashMap<String, Vec<(u32, String)>>> = input
        .lines()
        .map(|l| {
            mainreg.captures(l).and_then(|outerbag| {
                let bag = outerbag[1].to_string();
                let deps = if &outerbag[2] == "no other bags" {
                    Some(Vec::new())
                } else {
                    outerbag[2]
                        .split(",")
                        .map(|d| {
                            depsreg.captures(d).map(|deps| {
                                (
                                    deps[1].parse::<u32>().unwrap(),
                                    deps[2].parse::<String>().unwrap(),
                                )
                            })
                        })
                        .collect()
                };

                deps.map(|d| (bag, d))
            })
        })
        .collect();

    res.ok_or_else(|| anyhow::anyhow!("couldn't parse tree"))
}

pub fn part1(input: &str) -> anyhow::Result<String> {
    let tree = parse_tree(input)?;
    let mut marked = HashSet::new();
    marked.insert(TARGETBAG);

    for k in tree.keys() {
        if marked.contains(k.as_str()) {
            continue;
        }
        mark(k, &mut marked, &tree);
    }

    Ok((marked.len() - 1).to_string())
}

fn bagsinbags(cur: &str, tree: &HashMap<String, Vec<(u32, String)>>) -> u32 {
    1 + tree
        .get(cur)
        .unwrap()
        .iter()
        .fold(0, |b, bag| b + bag.0 * bagsinbags(&bag.1, tree))
}

pub fn part2(input: &str) -> anyhow::Result<String> {
    let tree = parse_tree(input)?;
    let bags = bagsinbags(TARGETBAG, &tree) - 1;
    Ok(bags.to_string())
}
