use crate::parsers as P;
use crate::parsers::VecParse;
use anyhow;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;

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
    let mainreg = Regex::new(r"(?m)^(.+?) bags contain (.+?)\.$").unwrap();
    let depsreg = Regex::new(r" *([0-9]+) (.+?) bags?").unwrap();

    let res = P::list_of_regex_sep::<_, _, P::ParserError>(input, "\n", &mainreg, |outerbag| {
        let bag = outerbag[0].to_string();
        let deps = if outerbag[1] == "no other bags" {
            Vec::new()
        } else {
            P::list_of_regex_sep(outerbag[1], ",", &depsreg, |deps| {
                let x: Result<(u32, String), _> = VecParse(deps).try_into();
                x
            })?
        };
        Ok((bag, deps))
    })?;

    Ok(res.into_iter().collect())
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
