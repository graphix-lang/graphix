mod check;
mod select;
mod solve;
mod ty;

use check::{Checker, Unsupported};
use std::{
    env, fs,
    io::{self, BufRead},
    path::PathBuf,
};

/// One program a line on stdin, or the files named as arguments. Prints
/// `ok <type>`, `no <message>` or `skip <construct>`, then the steps the
/// solver took, tab separated, one line per program.
fn main() {
    let stdlib = env::var("GRAPHIX_STDLIB")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../stdlib"));
    let judge = |text: &str| {
        let mut c = Checker::new(stdlib.clone());
        let verdict = match c.program(text) {
            Ok(t) => format!("ok\t{t}"),
            Err(e) => match e.downcast_ref::<Unsupported>() {
                Some(u) => format!("skip\t{}", u.0),
                None => format!("no\t{}", format!("{e:#}").replace(['\n', '\t'], " ")),
            },
        };
        println!("{verdict}\t{}", c.s.steps)
    };
    let files: Vec<String> = env::args().skip(1).collect();
    if files.is_empty() {
        io::stdin().lock().lines().map_while(Result::ok).for_each(|l| judge(&l))
    } else {
        for f in files {
            match fs::read_to_string(&f) {
                Ok(text) => judge(&text),
                Err(e) => println!("skip\tunreadable: {e}\t0"),
            }
        }
    }
}
