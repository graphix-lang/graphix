use anyhow::{Context, Result};
use graphix_compiler::expr::format::{
    FormatConfig, Refused, SourceKind, format_source, format_source_unchecked,
};
use std::{env, fs, path::Path, process::ExitCode};

/// Round trip + idempotence over every file given (one file: print it),
/// each under the configuration `graphix fmt` would find for it.
/// `GXFMT_UNCHECKED=1` skips the reparse. Exits non-zero when any file was
/// unreadable or exposed a formatter bug; a file the formatter merely
/// declines is not a failure.
fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    let quiet = args.len() > 1;
    let unchecked = env::var("GXFMT_UNCHECKED").is_ok();
    let mut bad = 0;
    for f in &args {
        if let Err(e) = check(Path::new(f), quiet, unchecked) {
            bad += 1;
            eprintln!("{f}: {e:#}")
        }
    }
    if bad > 0 {
        eprintln!("{bad} bad");
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

fn check(path: &Path, quiet: bool, unchecked: bool) -> Result<()> {
    let text = fs::read_to_string(path)?;
    let kind = SourceKind::of_path(path);
    let dir = path.parent().filter(|d| !d.as_os_str().is_empty());
    let cfg = FormatConfig::discover(dir.unwrap_or(Path::new(".")))?;
    if unchecked {
        let s = format_source_unchecked(kind, &text, &cfg)?;
        if !quiet {
            print!("{}", &*s)
        }
        return Ok(());
    }
    let once = match format_source(kind, &text, &cfg) {
        Ok(s) => s,
        Err(e) if e.is::<Refused>() => return Err(e),
        Err(_) => return Ok(()),
    };
    if !quiet {
        print!("{}", &*once);
        return Ok(());
    }
    let twice = format_source(kind, &once, &cfg).context("second pass")?;
    anyhow::ensure!(*twice == *once, "NOT IDEMPOTENT");
    Ok(())
}
