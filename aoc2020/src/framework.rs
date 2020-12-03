use anyhow::{self, Context};
use colored::*;
use std::fs;
use std::path::{Path, PathBuf};

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

pub fn execute_test_cases(solver: AOCPart, part: Part, dir: &Path) -> anyhow::Result<()> {
    let mut tests = Vec::new();
    let mut prob = None;
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
    }

    if tests.is_empty() && prob.is_none() {
        anyhow::bail!("found no instances to run");
    }

    tests.sort_unstable();

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
        println!("{}: {}", "Answer".yellow(), solver(cont.trim_end()).context("solver error")?);
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
        .ok_or(anyhow::anyhow!("couldn't find project root"))?
        .to_path_buf();

    proj_root.push("src");

    Ok(proj_root)
}
