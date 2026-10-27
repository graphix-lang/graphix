//! Build every example program under `book/src/examples` and every
//! bench program against the full shell environment, as `graphix
//! --expand` does: checked, elaborated and fused, never run. And parse
//! every ```graphix fence written inline in the book.

use anyhow::{Context, Result, anyhow};
use futures::{StreamExt, stream};
use graphix_compiler::{
    CFlag,
    expr::{Load, Origin, parser},
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
            let r = ShellBuilder::<NoExt>::default()
                .cache(CacheMode::Off)
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
/// its body, where it starts, and, for a `refused` fence followed by a
/// transcript of the refusal, the transcript's last line.
struct Fence {
    at: String,
    tag: String,
    body: String,
    shown: Option<String>,
}

/// The fence a page is inside.
enum Open {
    Graphix(Fence),
    Other(String),
}

/// A transcript's error message: its last line without the cause's
/// number, if it shows an error.
fn shown_error(transcript: &str) -> Option<String> {
    if !transcript.contains("Error:") {
        return None;
    }
    let last = transcript.lines().rev().find(|l| !l.trim().is_empty())?.trim();
    let msg = last.split_once(": ").filter(|(n, _)| n.parse::<u32>().is_ok());
    Some(msg.map_or(last, |(_, m)| m).to_string())
}

fn book_fences(dir: &Path, fences: &mut Vec<Fence>) -> Result<()> {
    for e in fs::read_dir(dir).with_context(|| dir.display().to_string())? {
        let p = e?.path();
        if p.is_dir() {
            book_fences(&p, fences)?;
        } else if p.extension().is_some_and(|x| x == "md") {
            let text = fs::read_to_string(&p)?;
            let mut open: Option<Open> = None;
            let mut refused: Option<usize> = None;
            for (n, line) in text.lines().enumerate() {
                match (&mut open, line.starts_with("```")) {
                    (None, false) => (),
                    (None, true) => {
                        open = Some(match line.strip_prefix("```graphix") {
                            Some(tag) => Open::Graphix(Fence {
                                at: format!("{}:{}", p.display(), n + 1),
                                tag: tag.trim().to_string(),
                                body: String::new(),
                                shown: None,
                            }),
                            None => Open::Other(String::new()),
                        })
                    }
                    (Some(_), true) => match open.take() {
                        Some(Open::Graphix(f)) => {
                            refused = matches!(Claim::of(&f.tag), Claim::Refused)
                                .then_some(fences.len());
                            fences.push(f)
                        }
                        Some(Open::Other(body)) => {
                            if let Some(i) = refused.take() {
                                fences[i].shown = shown_error(&body)
                            }
                        }
                        None => (),
                    },
                    (
                        Some(Open::Graphix(Fence { body, .. }) | Open::Other(body)),
                        false,
                    ) => {
                        body.push_str(line);
                        body.push('\n')
                    }
                }
            }
        }
    }
    Ok(())
}

/// The check's verdict on `text` as a script, or the reason it does not
/// parse as one.
async fn check(text: &str) -> Result<Result<()>> {
    parser::parse(Origin::unspecified(text))?;
    Ok(ShellBuilder::<NoExt>::default()
        .cache(CacheMode::Off)
        .mode(Mode::Check(Load::Text(text.into())))
        .build()
        .expect("building shell")
        .check()
        .await)
}

/// What the book claims of a ```graphix fence, by its info string.
enum Claim {
    /// `ignore`: a schematic fragment, nothing.
    Nothing,
    /// `sig`: an interface, which parses.
    Sig,
    /// `fragment`: a program that parses, written against names a page
    /// assumes.
    Fragment,
    /// `refused`: a program the check refuses.
    Refused,
    /// untagged: a program the check accepts, or a REPL transcript whose
    /// inputs (lines after `〉`) each parse.
    Accepted,
}

impl Claim {
    fn of(tag: &str) -> Self {
        let has = |t| tag.split(',').any(|x| x.trim() == t);
        match () {
            () if has("ignore") => Self::Nothing,
            () if has("sig") => Self::Sig,
            () if has("fragment") => Self::Fragment,
            () if has("refused") => Self::Refused,
            () => Self::Accepted,
        }
    }
}

/// Every ```graphix fence in the book holds what its tag claims
/// ([`Claim`]); an `{{#include}}` is checked where the file lives.
// XCR claude for claude: [test-gap/low] This gate only parses untagged fences and REPL
// inputs. A `refused` fence passes on any refusal, and transcript output is never
// compared, so stale type readouts and stale coverage rules in the book pass it. 45 of
// the 114 untagged fences in core/, functions/ and the top-level pages fail --check
// (schematic names, .gxi signatures). Tag those `ignore` (or a `sig` tag that runs
// parse_sig) and check the rest, and consider comparing each transcript's `-:` line.
// probe: ran --check over every fence in those pages. (book-core-11)
// 2026-10-27 claude: book_fences_check (was book_fences_parse) now checks every
// untagged program fence as a script; the 136 that fail as written are tagged
// ,fragment (parses: schematic names, context from earlier fences) or ,sig
// (parse_sig); a ,refused fence followed by an error transcript must refuse with the
// transcript's last message (shown_error). The check found two stale fences in
// packages/using.md (subscribe with no target type), fixed. Verified by untagging a
// let_binds fence and editing a select.md transcript: both fail the test. Not done:
// REPL transcripts' output lines (-:) are still not compared, and a refused fence
// without a transcript passes on any refusal.
#[tokio::test(flavor = "multi_thread")]
async fn book_fences_check() -> Result<()> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../book/src");
    let mut fences = vec![];
    book_fences(&root, &mut fences)?;
    assert!(fences.len() >= 400, "book looks wrong: only {} fences", fences.len());
    let failures: Vec<String> = stream::iter(fences.iter())
        .filter(|f| std::future::ready(!f.body.contains("{{#include")))
        .map(|f| async move {
            let failed = match Claim::of(&f.tag) {
                Claim::Nothing => return None,
                Claim::Sig => parser::parse_sig(Origin::unspecified(&f.body)).err(),
                Claim::Fragment => parser::parse(Origin::unspecified(&f.body)).err(),
                Claim::Accepted if f.body.contains('〉') => {
                    f.body.lines().filter_map(|l| l.strip_prefix('〉')).find_map(
                        |input| {
                            parser::parse(Origin::unspecified(input))
                                .err()
                                .map(|e| anyhow!("{input}: {e:#}"))
                        },
                    )
                }
                Claim::Accepted => match check(&f.body).await {
                    Ok(r) => r.err(),
                    Err(e) => Some(e),
                },
                Claim::Refused => match check(&f.body).await {
                    Ok(Ok(())) => {
                        Some(anyhow!("tagged refused, but the check accepts it"))
                    }
                    Ok(Err(e)) => f
                        .shown
                        .as_ref()
                        .filter(|shown| !format!("{e:#}").contains(shown.as_str()))
                        .map(|shown| {
                            anyhow!("the page shows `{shown}`, the check says {e:#}")
                        }),
                    Err(e) => Some(e),
                },
            };
            failed.map(|e| format!("{}: {e:#}", f.at))
        })
        .buffer_unordered(8)
        .filter_map(|r| async move { r })
        .collect()
        .await;
    assert!(
        failures.is_empty(),
        "{} book fence(s) failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
    Ok(())
}
