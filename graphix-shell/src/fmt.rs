//! `graphix fmt`: the source formatter's command line.

use anyhow::{Context, Result, bail};
use graphix_compiler::expr::format::{SourceKind, format_source};
use std::{
    fs,
    io::{self, Read, Write},
    path::PathBuf,
};

pub struct Args {
    pub files: Vec<PathBuf>,
    pub check: bool,
    pub stdout: bool,
    pub interface: bool,
    pub width: usize,
}

fn format_stdin(args: &Args) -> Result<()> {
    let kind =
        if args.interface { SourceKind::Interface } else { SourceKind::Program };
    let mut text = String::new();
    io::stdin().read_to_string(&mut text).context("reading stdin")?;
    let formatted = format_source(kind, &text, args.width)?;
    if args.check {
        if *formatted != text {
            bail!("stdin is not formatted")
        }
        return Ok(());
    }
    Ok(io::stdout().write_all(formatted.as_bytes())?)
}

/// Format stdin to stdout when no file is named, else each file in
/// place. `check` writes nothing and fails if anything would change.
pub fn run(args: Args) -> Result<()> {
    if args.files.is_empty() {
        return format_stdin(&args);
    }
    let mut failed = 0;
    let mut unformatted = 0;
    for path in &args.files {
        let res = (|| -> Result<bool> {
            let text = fs::read_to_string(path)?;
            let formatted =
                format_source(SourceKind::of_path(path), &text, args.width)?;
            if args.stdout {
                io::stdout().write_all(formatted.as_bytes())?;
                return Ok(false);
            }
            let changed = *formatted != text;
            if changed && !args.check {
                fs::write(path, formatted.as_bytes())?
            }
            Ok(changed)
        })();
        match res {
            Ok(false) => (),
            Ok(true) => {
                unformatted += 1;
                if args.check {
                    println!("{}", path.display())
                }
            }
            Err(e) => {
                failed += 1;
                eprintln!("{}: {e:#}", path.display())
            }
        }
    }
    if failed > 0 {
        bail!("{failed} file(s) could not be formatted")
    }
    if args.check && unformatted > 0 {
        bail!("{unformatted} file(s) are not formatted")
    }
    Ok(())
}
