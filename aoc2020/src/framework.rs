use colored::*;
use std::fs;
use std::path::{Path, PathBuf};

pub type AnyError = Box<dyn std::error::Error>;
pub type AOCSolve = fn(input: &str) -> Result<String, AnyError>;

macro_rules! aoc_import {
    ($(mod $i:ident);+ ;) => {
        $(
            mod $i {pub mod sol;}
        )+

        fn fetch_problem(day: &str) -> Option<$crate::framework::AOCSolve> {
            match day {
                $(
                    stringify!($i) => Some($i::sol::solve)
                ),+ ,
                _ => None
            }
        }
    }
}

pub fn execute_test_cases(solver: AOCSolve, dir: &Path) -> Result<(), AnyError> {
    let mut tests = Vec::new();
    let mut prob = None;
    for f in fs::read_dir(dir)? {
        let ff = if let Ok(f) = f { f } else { continue };

        let fname = ff.file_name();
        if fname == "prob" {
            prob = Some(ff.path());
        }

        if let Some(s) = fname.as_os_str().to_str() {
            if s.starts_with("test") {
                tests.push(ff.path());
            }
        }
    }

    for t in tests.into_iter() {
        let fname = t
            .file_name()
            .expect("can't get file name")
            .to_str()
            .expect("filename not valid unicode");
        let cont = fs::read_to_string(&t)?;

        let last_line_sep = cont.trim_end().rfind('\n');
        if last_line_sep.is_none() {
            return Err(format!("{:?} doesn't seem to contain an answer row", fname).into());
        }

        let last_line = cont.as_str()[last_line_sep.unwrap() + 1..].trim_end();
        let first_lines = &cont.as_str()[..last_line_sep.unwrap()];

        let solved = solver(first_lines)?;
        if solved == last_line {
            println!("{}: {}", "Ok".green(), fname);
        } else {
            println!("{}: {}", "Error".red(), fname);
            println!("expected: {}", last_line);
            println!("got: {}", solved);
            return Ok(())
        }
    }

    if let Some(p) = prob {
        let cont = fs::read_to_string(&p)?;
        println!("{}: {}", "Answer".yellow(), solver(cont.trim_end())?);
    }

    Ok(())
}

pub fn find_src_dir(cwd: &Path) -> Result<PathBuf, AnyError> {
    let cargo_name = std::env!("CARGO_PKG_NAME");
    if cargo_name.is_empty() {
        return Err("not running in cargo?".into());
    }
    let can = cwd.canonicalize()?; // TODO: maybe use something that doesn't resolve symlinks?

    let mut proj_root = can
        .ancestors()
        .find(|a| a.is_dir() && a.file_name().map_or(false, |f| f == cargo_name))
        .ok_or("couldn't find project root")?
        .to_path_buf();

    proj_root.push("src");

    Ok(proj_root)
}
