use graphix_compiler::expr::format::{
    FormatConfig, Refused, SourceKind, format_source, format_source_unchecked,
};
use std::{env, fs, path::Path, process::ExitCode};

/// Round trip + idempotence over every file given (one file: print it).
/// Exits non-zero when any file was unreadable or exposed a formatter
/// bug; a file the formatter merely declines is not a failure.
// XCR codex for eric: CR24 — done: exit status, read errors counted,
// `Refused` matched by type.
fn main() -> ExitCode {
    let cfg = FormatConfig::default();
    let mut bad = 0;
    let args: Vec<String> = env::args().skip(1).collect();
    let quiet = args.len() > 1;
    for f in &args {
        let text = match fs::read_to_string(f) {
            Ok(t) => t,
            Err(e) => {
                bad += 1;
                eprintln!("{f}: {e}");
                continue;
            }
        };
        if env::var("GXFMT_UNCHECKED").is_ok() {
            let kind = SourceKind::of_path(Path::new(f));
            print!("{}", &*format_source_unchecked(kind, &text, &cfg).unwrap());
            continue;
        }
        match format_source(SourceKind::of_path(Path::new(f)), &text, &cfg) {
            Ok(s) if !quiet => print!("{}", &*s),
            Ok(s) => match format_source(SourceKind::of_path(Path::new(f)), &s, &cfg) {
                Ok(s2) if *s2 == *s => (),
                Ok(_) => {
                    bad += 1;
                    eprintln!("{f}: NOT IDEMPOTENT")
                }
                Err(e) => {
                    bad += 1;
                    eprintln!("{f}: second pass: {e:#}")
                }
            },
            Err(e) => {
                if e.is::<Refused>() {
                    bad += 1;
                    eprintln!("{f}: {e:#}")
                }
            }
        }
    }
    if bad > 0 {
        eprintln!("{bad} bad");
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}
