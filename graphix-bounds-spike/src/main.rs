mod check;
mod mirror;
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
    let judge = |text: &str, dir: Option<PathBuf>, sig: Option<String>| {
        // a package's root module: .../graphix-package-<name>/src/graphix/mod.gx
        let root = dir.as_ref().filter(|_| sig.is_some()).and_then(|d| {
            let pkg = d
                .parent()?
                .parent()?
                .file_name()?
                .to_str()?
                .strip_prefix("graphix-package-")?;
            Some(pkg.replace('-', "_"))
        });
        let mut c = Checker::new(stdlib.clone(), dir, root);
        if env::var_os("SPIKE_PORT").is_some() {
            c.s.mirror = Some(Box::default())
        }
        let verdict = match c.program(text, sig.as_deref()) {
            Ok(t) => format!("ok\t{t}"),
            Err(e) => match e.downcast_ref::<Unsupported>() {
                Some(u) => format!("skip\t{}", u.0),
                None => {
                    // the refusal, then where its two ends are
                    let mut parts: Vec<String> =
                        e.chain().map(|c| c.to_string()).collect();
                    parts.reverse();
                    format!("no\t{}", parts.join("; ").replace(['\n', '\t'], " "))
                }
            },
        };
        let (module, line, column) = &c.last;
        // the ported solver's verdict on the same calls
        c.s.refused_outside = verdict.starts_with("no") && !c.s.refused_inside;
        let port = match c.s.mirror.take() {
            Some(mut m) => format!("\t{}", m.verdict(&c.s)),
            None => String::new(),
        };
        println!("{verdict}\t{}\t{module}:{line}:{column}{port}", c.s.steps)
    };
    let files: Vec<String> = env::args().skip(1).collect();
    if files.is_empty() {
        io::stdin()
            .lock()
            .lines()
            .map_while(Result::ok)
            .for_each(|l| judge(&l, None, None))
    } else {
        for f in files {
            match fs::read_to_string(&f) {
                Ok(text) => {
                    let path = PathBuf::from(&f);
                    let sig = fs::read_to_string(path.with_extension("gxi")).ok();
                    judge(&text, path.parent().map(PathBuf::from), sig)
                }
                Err(e) => println!("skip\tunreadable: {e}\t0"),
            }
        }
    }
}
