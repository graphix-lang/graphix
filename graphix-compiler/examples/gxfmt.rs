use graphix_compiler::expr::format::{
    DEFAULT_WIDTH, SourceKind, format_source, format_source_unchecked,
};
use std::{env, fs, path::Path};

fn main() {
    let mut bad = 0;
    let args: Vec<String> = env::args().skip(1).collect();
    let quiet = args.len() > 1;
    for f in &args {
        let text = match fs::read_to_string(f) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("{f}: {e}");
                continue;
            }
        };
        if env::var("GXFMT_UNCHECKED").is_ok() {
            let kind = SourceKind::of_path(Path::new(f));
            print!("{}", &*format_source_unchecked(kind, &text, DEFAULT_WIDTH).unwrap());
            continue;
        }
        match format_source(SourceKind::of_path(Path::new(f)), &text, DEFAULT_WIDTH) {
            Ok(s) if !quiet => print!("{}", &*s),
            Ok(s) => {
                match format_source(SourceKind::of_path(Path::new(f)), &s, DEFAULT_WIDTH)
                {
                    Ok(s2) if *s2 == *s => (),
                    Ok(_) => {
                        bad += 1;
                        eprintln!("{f}: NOT IDEMPOTENT")
                    }
                    Err(e) => {
                        bad += 1;
                        eprintln!("{f}: second pass: {e:#}")
                    }
                }
            }
            Err(e) => {
                if format!("{e:#}").contains("formatter bug") {
                    bad += 1;
                    eprintln!("{f}: {e:#}")
                }
            }
        }
    }
    if quiet {
        eprintln!("{bad} formatter bugs in {} files", args.len())
    }
}
