//! Build every example program under `book/src/examples` and every
//! bench program against the full shell environment, as `graphix
//! --expand` does: checked, elaborated and fused, never run. And parse
//! every ```graphix fence written inline in the book.

use anyhow::{Context, Result};
use futures::{StreamExt, stream};
use graphix_compiler::{
    CFlag,
    expr::{FilesResolver, Load, Origin, parser},
};
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

/// Every `.gx` file under `dir`, recursively, EXCEPT files that are a
/// `mod <stem>;` submodule of a sibling — those only compile as part
/// of their parent (e.g. the gui examples' shared `icon.gx`).
fn example_files(dir: &Path) -> Result<Vec<PathBuf>> {
    let mut dirs = vec![dir.to_path_buf()];
    let mut files = vec![];
    while let Some(d) = dirs.pop() {
        let mut submodules: HashSet<String> = HashSet::new();
        let mut here = vec![];
        for e in fs::read_dir(&d).with_context(|| d.display().to_string())? {
            let p = e?.path();
            if p.is_dir() {
                dirs.push(p);
            } else if p.extension().is_some_and(|x| x == "gx") {
                for line in fs::read_to_string(&p)?.lines() {
                    if let Some(m) = line.trim().strip_prefix("mod ")
                        && let Some(name) = m.strip_suffix(';')
                    {
                        submodules.insert(name.trim().to_string());
                    }
                }
                here.push(p);
            }
        }
        files.extend(here.into_iter().filter(|p| {
            p.file_stem().and_then(|s| s.to_str()).is_none_or(|s| !submodules.contains(s))
        }));
    }
    files.sort();
    Ok(files)
}

#[tokio::test(flavor = "multi_thread")]
async fn examples_compile() -> Result<()> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut files = example_files(&root.join("book/src/examples"))?;
    assert!(files.len() >= 100, "examples dir looks wrong: only {} files", files.len());
    files.extend(example_files(&root.join("bench"))?);
    let failures: Vec<String> = stream::iter(files)
        .map(|f| async move {
            let base = f.parent().expect("example has a parent dir").to_path_buf();
            let r = ShellBuilder::<NoExt>::default()
                .cache(CacheMode::Off)
                .module_resolvers(vec![FilesResolver::new(base, None)])
                .enable_flags(CFlag::ExpandSeq.into())
                .mode(Mode::Check(Load::File(f.clone())))
                .build()
                .expect("building shell")
                .check()
                .await;
            r.err().map(|e| format!("{}: {e:#}", f.display()))
        })
        .buffer_unordered(8)
        .filter_map(|r| async move { r })
        .collect()
        .await;
    assert!(
        failures.is_empty(),
        "{} example(s) failed to compile:\n{}",
        failures.len(),
        failures.join("\n")
    );
    Ok(())
}

/// A ```graphix fence of a book page: its info string after `graphix`,
/// its body, and where it starts.
struct Fence {
    at: String,
    tag: String,
    body: String,
}

fn book_fences(dir: &Path, fences: &mut Vec<Fence>) -> Result<()> {
    for e in fs::read_dir(dir).with_context(|| dir.display().to_string())? {
        let p = e?.path();
        if p.is_dir() {
            book_fences(&p, fences)?;
        } else if p.extension().is_some_and(|x| x == "md") {
            let text = fs::read_to_string(&p)?;
            let mut open: Option<Fence> = None;
            for (n, line) in text.lines().enumerate() {
                match &mut open {
                    None => {
                        if let Some(tag) = line.strip_prefix("```graphix") {
                            open = Some(Fence {
                                at: format!("{}:{}", p.display(), n + 1),
                                tag: tag.trim().to_string(),
                                body: String::new(),
                            })
                        }
                    }
                    Some(f) if line.starts_with("```") => {
                        fences.push(std::mem::replace(
                            f,
                            Fence {
                                at: String::new(),
                                tag: String::new(),
                                body: String::new(),
                            },
                        ));
                        open = None
                    }
                    Some(f) => {
                        f.body.push_str(line);
                        f.body.push('\n')
                    }
                }
            }
        }
    }
    Ok(())
}

/// Text parses as a program, or else as an interface.
fn parses(text: &str) -> Result<()> {
    match parser::parse(Origin::unspecified(text)) {
        Ok(_) => Ok(()),
        Err(e) => parser::parse_sig(Origin::unspecified(text)).map(|_| ()).map_err(|_| e),
    }
}

/// Every ```graphix fence in the book parses, unless it is tagged
/// `ignore` (a schematic fragment): an `{{#include}}` is checked where
/// the file lives, and a REPL transcript's inputs (lines after `〉`)
/// each parse. A fence tagged `refused` must also be refused by the
/// check.
#[tokio::test(flavor = "multi_thread")]
async fn book_fences_parse() -> Result<()> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../book/src");
    let mut fences = vec![];
    book_fences(&root, &mut fences)?;
    assert!(fences.len() >= 400, "book looks wrong: only {} fences", fences.len());
    let mut failures = vec![];
    for f in fences.iter().filter(|f| !f.tag.contains("ignore")) {
        if f.body.contains("{{#include") {
            continue;
        }
        if f.body.contains('〉') {
            for input in f.body.lines().filter_map(|l| l.strip_prefix('〉')) {
                if let Err(e) = parses(input) {
                    failures.push(format!("{}: {input}: {e:#}", f.at))
                }
            }
            continue;
        }
        if let Err(e) = parses(&f.body) {
            failures.push(format!("{}: {e:#}", f.at));
            continue;
        }
        if f.tag.contains("refused") {
            let r = ShellBuilder::<NoExt>::default()
                .cache(CacheMode::Off)
                .mode(Mode::Check(Load::Text(f.body.as_str().into())))
                .build()
                .expect("building shell")
                .check()
                .await;
            if r.is_ok() {
                failures
                    .push(format!("{}: tagged refused, but the check accepts it", f.at))
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} book fence(s) failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
    Ok(())
}
