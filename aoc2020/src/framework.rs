use anyhow::{self, Context};
use colored::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

pub type AOCPart = fn(input: &str) -> anyhow::Result<String>;
pub type AOCSolve = (AOCPart, AOCPart);

pub enum Part {
    Part1,
    Part2,
}

impl Part {
    fn prob_name(&self) -> &'static str {
        match self {
            Part::Part1 => "part1",
            Part::Part2 => "part2",
        }
    }

    fn is_test(&self, fname: &str) -> bool {
        fname.starts_with(self.prob_name()) && fname.find("test").is_some()
    }

    fn is_solution(&self, fname: &str) -> bool {
        fname.starts_with(self.prob_name()) && fname.find("solution").is_some()
    }
}

macro_rules! aoc_import {
    ($(mod $i:ident);+ ;) => {
        $(
            mod $i {pub mod sol;}
        )+

        fn fetch_problem(day: &str) -> Option<$crate::framework::AOCSolve> {
            match day {
                $(
                    stringify!($i) => Some(($i::sol::part1, $i::sol::part2))
                ),+ ,
                _ => None
            }
        }
    }
}

fn find_instance_files(
    part: Part,
    dir: &Path,
) -> anyhow::Result<(Vec<PathBuf>, Option<PathBuf>, Option<PathBuf>)> {
    let mut tests = Vec::new();
    let mut prob = None;
    let mut solution = None;

    for f in fs::read_dir(dir).with_context(|| format!("couldn't read folder: {:?}", dir))? {
        let ff = if let Ok(f) = f { f } else { continue };

        let fname = ff.file_name();
        if fname == "input" {
            prob = Some(ff.path());
        }

        if let Some(s) = fname.as_os_str().to_str() {
            if part.is_test(s) {
                tests.push(ff.path());
            }
        }

        if let Some(s) = fname.as_os_str().to_str() {
            if part.is_solution(s) {
                solution = Some(ff.path());
            }
        }
    }

    if tests.is_empty() && prob.is_none() {
        anyhow::bail!("found no instances to run");
    }

    tests.sort_unstable();

    Ok((tests, prob, solution))
}

pub fn execute_test_cases(solver: AOCPart, part: Part, dir: &Path) -> anyhow::Result<()> {
    let (tests, prob, solution) = find_instance_files(part, dir)?;

    for t in tests.into_iter() {
        let fname = t
            .file_name()
            .context("can't get file name")?
            .to_str()
            .context("filename not valid unicode")?;
        let cont =
            fs::read_to_string(&t).with_context(|| format!("coulnd't read contents of {:?}", t))?;

        let last_line_sep = cont.trim_end().rfind('\n');
        if last_line_sep.is_none() {
            anyhow::bail!(format!("{:?} doesn't seem to contain an answer row", fname));
        }

        let last_line = cont.as_str()[last_line_sep.unwrap() + 1..].trim_end();
        let first_lines = &cont.as_str()[..last_line_sep.unwrap()];

        let solved = solver(first_lines).context("solver error")?;
        if solved == last_line {
            println!("{}: {}", "Ok".green(), fname);
        } else {
            println!("{}: {}", "Error".red(), fname);
            println!("expected: {}", last_line);
            println!("got: {}", solved);
            return Ok(());
        }
    }

    if let Some(p) = prob {
        let cont = fs::read_to_string(&p).context("couldn't read problem file")?;
        let solver_input = cont.trim_end();

        let (resans, t) = time_it(|| solver(solver_input));
        let ans = resans.context("solver error")?;

        println!("{}: {}", "Answer".yellow(), ans);
        pretty_print_dur(t);

        if let Some(p) = solution {
            let sol = fs::read_to_string(&p).context("couldn't read solution file")?;
            println!(
                "The answer is {}",
                if sol.trim_end() == ans {
                    "correct".green()
                } else {
                    "wrong".red()
                }
            );
        }
    }

    Ok(())
}

pub fn find_src_dir(cwd: &Path) -> anyhow::Result<PathBuf> {
    let cargo_name = std::env!("CARGO_PKG_NAME");
    if cargo_name.is_empty() {
        anyhow::bail!("not running in cargo?");
    }
    let can = cwd.canonicalize()?; // TODO: maybe use something that doesn't resolve symlinks?

    let mut proj_root = can
        .ancestors()
        .find(|a| a.is_dir() && a.file_name().map_or(false, |f| f == cargo_name))
        .ok_or_else(|| anyhow::anyhow!("couldn't find project root"))?
        .to_path_buf();

    proj_root.push("src");

    Ok(proj_root)
}

pub fn time_it<F, T>(f: F) -> (T, Duration)
where
    F: FnOnce() -> T,
{
    let prev = Instant::now();
    let res = f();
    let dur = prev.elapsed();
    (res, dur)
}

fn pretty_print_dur(d: Duration) {
    print!("{}: ", "Execution time".blue());
    if d.as_millis() > 0 {
        println!("{} ms", d.as_millis());
    } else if d.as_micros() > 0 {
        println!("{} μs", d.as_micros());
    } else {
        println!("{} ns", d.as_nanos());
    }
}
