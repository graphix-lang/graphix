//! `graphix fmt`: the source formatter's command line.

use anyhow::{Context, Result, bail};
use graphix_compiler::expr::format::{FormatConfig, SourceKind, format_source};
use std::{
    fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

pub struct Args {
    pub files: Vec<PathBuf>,
    pub check: bool,
    pub stdout: bool,
    pub interface: bool,
    /// override the configured line width
    pub width: Option<usize>,
    /// override the configured indent
    pub indent: Option<usize>,
}

impl Args {
    /// The configuration for a source file in `dir`, under the command
    /// line's overrides.
    fn config(&self, dir: &Path) -> Result<FormatConfig> {
        let cfg = FormatConfig::discover(dir)?;
        Ok(FormatConfig {
            width: self.width.unwrap_or(cfg.width),
            indent: self.indent.unwrap_or(cfg.indent),
        })
    }
}

fn format_stdin(args: &Args) -> Result<()> {
    let kind = if args.interface { SourceKind::Interface } else { SourceKind::Program };
    let mut text = String::new();
    io::stdin().read_to_string(&mut text).context("reading stdin")?;
    let cfg = args.config(&std::env::current_dir()?)?;
    let formatted = format_source(kind, &text, &cfg)?;
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
            // CR claude for eric: [bug] The CLI discovers the config from the canonical
            // path, the LSP (graphix-lsp/src/handlers/formatting.rs:42) from the path
            // as opened, and gxfmt from the path as given, so a symlinked source is
            // laid out by the config above its target here and by the one above the
            // link in the editor: format-on-save and `graphix fmt --check` disagree on
            // it. FormatConfig::discover also keeps `..` from std::path::absolute, so a
            // relative `../x/y.gx` (gxfmt run over ../netidx) climbs the working
            // directory's ancestors. One function from a file path to its config
            // (absolute, lexically normalized, not canonical, the empty parent handled
            // once) would serve all three callers. probe:
            // design/review-2026-10-05/repro/t-format-resolver-11.py
            // (t-format-resolver-11)
            let file = fs::canonicalize(path)?;
            let cfg = args.config(file.parent().unwrap_or(&file))?;
            let formatted = format_source(SourceKind::of_path(path), &text, &cfg)?;
            if args.stdout {
                io::stdout().write_all(formatted.as_bytes())?;
                return Ok(false);
            }
            // CR claude for eric: [risk] format_source emits LF, so a CRLF file always
            // counts as changed. `graphix fmt --check` lists every file of a CRLF
            // checkout (core.autocrlf without this repo's eol=lf) without saying why,
            // and `graphix fmt` silently rewrites the line endings. format_stdin's
            // --check (line 41) fails CRLF input the same way, and a CRLF file with a
            // raw string spanning lines is refused as a 'formatter bug'. Keep the
            // input's newline style (in format_source, so stdin and the LSP share the
            // fix), or document LF-only and name line endings in --check's report. The
            // write at line 69 also truncates before writing, so a failed write leaves
            // the source cut short; writing a sibling temp file and renaming it over
            // the original avoids that. probe: printf 'let x = 1;\r\nx\r\n' > crlf.gx;
            // graphix fmt --check crlf.gx (shell-19)
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
