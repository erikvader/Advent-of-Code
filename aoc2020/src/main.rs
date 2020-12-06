use anyhow::{self, Context};
use colored::*;
use std::env;
use std::path::Path;

#[macro_use]
mod framework;
mod parsers;

use framework::{
    execute_test_cases, find_src_dir,
    Part::{Part1, Part2},
};

aoc_import! {
    mod day1;
    mod day2;
    mod day3;
    mod day4;
    //<day-marker>
}

fn fake_main() -> anyhow::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        anyhow::bail!("I need a day and a part as arguments");
    }

    let dayname = String::from("day") + &args[1];
    let part = &args[2];
    let prob = fetch_problem(&dayname).with_context(|| {
        format!(
            "\"{}\" is not a valid day or it is not created yet",
            dayname
        )
    })?;

    let mut srcdir = find_src_dir(Path::new(&args[0])).context("can't find cargo src dir")?;
    srcdir.push(&dayname);

    if part == "1" {
        execute_test_cases(prob.0, Part1, &srcdir)?;
    } else if part == "2" {
        execute_test_cases(prob.1, Part2, &srcdir)?;
    } else {
        anyhow::bail!("Invalid part");
    }

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let exit_code = match fake_main() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}: {:?}", "Exception".red(), e);
            1
        }
    };
    std::process::exit(exit_code);
}
