use colored::*;
use std::env;
use std::path::Path;

#[macro_use]
mod framework;
mod parsers;

pub use framework::AnyError;
use framework::{
    execute_test_cases, find_src_dir,
    Part::{Part1, Part2},
};
pub use parsers::CSV;

aoc_import! {
    mod day1;
}

fn fake_main() -> Result<(), AnyError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        return Err("I need a day and a part as arguments".into());
    }

    let dayname = String::from("day") + &args[1];
    let part = &args[2];
    let prob = fetch_problem(&dayname).ok_or(format!(
        "\"{}\" is not a valid day or it is not created yet",
        dayname
    ))?;

    let mut srcdir = find_src_dir(Path::new(&args[0]))?;
    srcdir.push(&dayname);

    if part == "1" {
        execute_test_cases(prob.0, Part1, &srcdir)?;
    } else if part == "2" {
        execute_test_cases(prob.1, Part2, &srcdir)?;
    } else {
        return Err("Invalid part".into());
    }

    Ok(())
}

fn main() {
    let exit_code = match fake_main() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{}: {}", "Exception".red(), e);
            1
        }
    };
    std::process::exit(exit_code);
}
