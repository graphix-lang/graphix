use anyhow::{Context, Result};
use graphix_compiler::expr::format::{
    FormatConfig, Refused, SourceKind, format_source, format_source_unchecked,
};
use std::{env, fs, path::Path, process::ExitCode};

/// Round trip + idempotence over every file given (one file: print it),
/// each under the configuration `graphix fmt` would find for it.
/// `GXFMT_UNCHECKED=1` skips the reparse. Exits non-zero when any file was
/// unreadable or exposed a formatter bug, and, given one file, when it does
/// not parse. Many files: those that do not parse are skipped and listed.
fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    let quiet = args.len() > 1;
    let unchecked = env::var("GXFMT_UNCHECKED").is_ok();
    let (mut bad, mut skipped) = (0, Vec::new());
    for f in &args {
        match check(Path::new(f), quiet, unchecked) {
            Ok(Checked::Formatted) => (),
            Ok(Checked::Unparsed(e)) if quiet => skipped.push((f, e)),
            Ok(Checked::Unparsed(e)) | Err(e) => {
                bad += 1;
                eprintln!("{f}: {e:#}")
            }
        }
    }
    if !skipped.is_empty() {
        eprintln!("{} skipped: do not parse", skipped.len());
        for (f, e) in &skipped {
            eprintln!("  {f}: {}", format!("{e:#}").lines().next().unwrap_or(""));
        }
    }
    if bad > 0 {
        eprintln!("{bad} bad");
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

enum Checked {
    Formatted,
    /// The input does not parse, so there is nothing to round-trip.
    Unparsed(anyhow::Error),
}

fn check(path: &Path, quiet: bool, unchecked: bool) -> Result<Checked> {
    let text = fs::read_to_string(path)?;
    let kind = SourceKind::of_path(path);
    let cfg = FormatConfig::for_file(path)?;
    if unchecked {
        let s = format_source_unchecked(kind, &text, &cfg)?;
        if !quiet {
            print!("{}", &*s)
        }
        return Ok(Checked::Formatted);
    }
    let once = match format_source(kind, &text, &cfg) {
        Ok(s) => s,
        Err(e) if e.is::<Refused>() => return Err(e),
        Err(e) => return Ok(Checked::Unparsed(e)),
    };
    if !quiet {
        print!("{}", &*once);
        return Ok(Checked::Formatted);
    }
    let twice = format_source(kind, &once, &cfg).context("second pass")?;
    anyhow::ensure!(*twice == *once, "NOT IDEMPOTENT");
    Ok(Checked::Formatted)
}
