use std::path::Path;
use std::env;
use colored::*;

#[macro_use]
mod framework;
mod parsers;

pub use framework::AnyError;
use framework::{find_src_dir, execute_test_cases};
pub use parsers::CSV;

aoc_import! {
    mod day1;
}

fn fake_main() -> Result<(), AnyError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err("I need a day as an argument".into());
    }

    let dayname = String::from("day") + &args[1];
    let prob = match fetch_problem(&dayname) {
        Some(x) => x,
        None => {
            return Err(format!(
                "\"{}\" is not a valid day or it is not created yet",
                dayname
            )
            .into());
        }
    };

    let mut srcdir = find_src_dir(Path::new(&args[0]))?;
    srcdir.push(&dayname);
    execute_test_cases(prob, &srcdir)?;

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
