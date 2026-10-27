//! Build every example program under `book/src/examples` and every
//! bench program against the full shell environment, as `graphix
//! --expand` does: checked, elaborated and fused, never run. And
//! check every ```graphix fence and REPL type readout written inline in the book.

use anyhow::{Context, Result, anyhow};
use compact_str::{CompactString, format_compact};
use futures::{StreamExt, stream};
use graphix_compiler::{
    CFlag, ExecState, Node, PrintFlag, Saved, Scope, compile_stmt,
    expr::{Expr, Load, Origin, parser},
    format_with_flags,
};
use graphix_package::register_packages;
use graphix_rt::{GXRt, NoExt};
use graphix_shell::{CacheMode, Mode, ShellBuilder};
use poolshark::local::LPooled;
use std::{
    collections::HashSet,
    fmt::Write,
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
    page: PathBuf,
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
                                page: p.clone(),
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
    ReplSetup,
    /// `sig`: an interface, which parses.
    Sig,
    /// `fragment`: a program that parses, written against names a page
    /// assumes.
    Fragment,
    /// `refused`: a program the check refuses.
    Refused,
    /// untagged: a program the check accepts, or a REPL transcript whose
    /// inputs are checked in their page's REPL session.
    Accepted,
}

impl Claim {
    fn of(tag: &str) -> Self {
        let has = |t| tag.split(',').any(|x| x.trim() == t);
        match () {
            () if has("ignore") => Self::Nothing,
            () if has("repl") => Self::ReplSetup,
            () if has("sig") => Self::Sig,
            () if has("fragment") => Self::Fragment,
            () if has("refused") => Self::Refused,
            () => Self::Accepted,
        }
    }
}

struct Repl {
    ctx: ExecState<GXRt<NoExt>>,
    scope: Scope,
    nodes: LPooled<Vec<Node<GXRt<NoExt>>>>,
}

impl Repl {
    async fn new() -> Result<Self> {
        let mut ctx = GXRt::new_state()?;
        let packages = graphix_shell::stdlib_packages::<NoExt>();
        let (resolver, root) =
            register_packages(&mut ctx, packages.iter().map(|p| &**p))?;
        let resolvers = std::sync::Arc::from([resolver]);
        let mut repl = Self { ctx, scope: Scope::root(), nodes: LPooled::take() };
        for e in parser::parse(Origin::unspecified(&root))?.iter() {
            let e = e.resolve_modules(&resolvers, None).await?;
            repl.statement(e)?;
        }
        Ok(repl)
    }

    fn statement(&mut self, e: Expr) -> Result<CompactString> {
        let (n, scope) = compile_stmt(
            &mut self.ctx.view(),
            CFlag::CheckOnly | CFlag::FusionDisabled,
            &self.scope,
            e,
        )?;
        self.scope = scope;
        let typ = n.typ().with_deref(|t| t.cloned()).unwrap_or_else(|| n.typ().clone());
        let shown =
            format_with_flags(PrintFlag::DerefTVars | PrintFlag::ReplacePrims, || {
                format_compact!("{typ}")
            });
        self.nodes.push(n);
        Ok(shown)
    }

    fn input(&mut self, text: &str) -> Result<CompactString> {
        let exprs = parser::parse(Origin::unspecified(text))?;
        let saved = Saved::take(&self.ctx);
        let scope = self.scope.clone();
        let before = self.nodes.len();
        let mut shown = CompactString::default();
        for e in exprs.iter() {
            match self.statement(e.clone()) {
                Ok(t) => shown = t,
                Err(e) => {
                    for mut node in self.nodes.drain(before..).rev() {
                        node.delete(&mut self.ctx.view());
                    }
                    self.scope = scope;
                    saved.restore(&mut self.ctx);
                    return Err(e);
                }
            }
        }
        Ok(shown)
    }

    fn transcript(&mut self, text: &str) -> Result<()> {
        let mut lines = text.lines().peekable();
        while let Some(line) = lines.next() {
            let Some(input) = line.strip_prefix('〉') else { continue };
            let mut shown = None;
            let mut refused = false;
            while let Some(line) = lines.next_if(|l| !l.starts_with('〉')) {
                if let Some(typ) = line.strip_prefix("-:") {
                    shown = Some(typ.trim());
                }
                refused |= line.starts_with("error:") || line.starts_with("Error:");
            }
            let result = self.input(input);
            if let Some(shown) = shown {
                let actual = result.with_context(|| format_compact!("input {input}"))?;
                anyhow::ensure!(
                    normalize_type(&actual) == normalize_type(shown),
                    "input {input}: the page shows -: {shown}, the check says -: {actual}"
                );
            } else if refused {
                anyhow::ensure!(
                    result.is_err(),
                    "input {input}: the page shows a refusal, but the check accepts it"
                );
            } else {
                result.with_context(|| format_compact!("input {input}"))?;
            }
        }
        Ok(())
    }
}

impl Drop for Repl {
    fn drop(&mut self) {
        for mut node in self.nodes.drain(..).rev() {
            node.delete(&mut self.ctx.view());
        }
    }
}

fn normalize_type(mut text: &str) -> CompactString {
    let mut ids: LPooled<Vec<&str>> = LPooled::take();
    let mut out = CompactString::default();
    while let Some(at) = text.find("'_") {
        out.push_str(&text[..at + 2]);
        text = &text[at + 2..];
        let n = text.bytes().take_while(u8::is_ascii_digit).count();
        if n != 0 {
            let id = &text[..n];
            let i = ids.iter().position(|s| *s == id).unwrap_or_else(|| {
                ids.push(id);
                ids.len() - 1
            });
            write!(out, "{i}").unwrap();
            text = &text[n..];
        }
    }
    out.push_str(text);
    out
}

#[tokio::test]
async fn transcript_type_readouts_are_checked() -> Result<()> {
    let mut repl = Repl::new().await?;
    repl.transcript("〉let x = 1\n〉x\n-: i64\n1\n")?;
    let error = repl.transcript("〉x\n-: string\n1\n").unwrap_err();
    assert!(error.to_string().contains("the page shows -: string"));
    assert_eq!(normalize_type("('_123, '_123, '_456)"), "('_0, '_0, '_1)");
    assert_ne!(normalize_type("('_123, '_123)"), normalize_type("('_1, '_2)"));
    Ok(())
}

/// Every ```graphix fence in the book holds what its tag claims
/// ([`Claim`]); an `{{#include}}` is checked where the file lives.
// CR claude for claude: [test-gap/low] This gate only parses untagged fences and REPL
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
// 2026-10-27 claude: Done: untagged fences are checked as scripts, the 136 that cannot
// be are tagged ,fragment or ,sig, and a ,refused fence followed by an error
// transcript must refuse with that transcript's message. CLAUDE.md states this. Still
// open, and part of the defect this CR describes: REPL transcripts' output lines (-:)
// are never compared, so a stale type readout or value in the book still passes, and a
// ,refused fence with no transcript passes on any refusal. Remaining: compare each
// transcript input's -: line with the checked type (or the printed value), or say in
// the CR why that cannot be done.
// 2026-10-27 codex: The requested REPL -: check is implemented. Each page has a
// persistent check-only REPL context, explicitly seeded by ,repl fences; each prompt's
// checked type is compared with the printed type, renaming only generated variable IDs
// consistently. Values are not executed. Failed inputs restore the session. The new
// transcript_type_readouts_are_checked pin rejects a deliberately wrong -: string for
// an i64; observed it fail with the comparison disabled, then pass with comparison
// enabled. The full examples_compile target passed its examples and pin but found
// three stale book readouts: Error<'a> omitted the inferred `Foo constraint, and two
// struct readouts omitted printer spaces. Corrected those; book_fences_check then
// passed (direct current test executable), and mdbook build passed. Added three ,repl
// setup tags for earlier declarations that transcripts use. Generated site output was
// restored because the installed mdbook rewrites site-wide assets unrelated to this
// edit. The older CR's separate issue remains: a ,refused fence without an error
// transcript still accepts any refusal. Leaving the CR open for that remaining policy;
// no image bytes changed.
#[tokio::test(flavor = "multi_thread")]
async fn book_fences_check() -> Result<()> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../book/src");
    let mut fences = vec![];
    book_fences(&root, &mut fences)?;
    assert!(fences.len() >= 400, "book looks wrong: only {} fences", fences.len());
    let mut failures: Vec<String> = stream::iter(fences.iter())
        .filter(|f| std::future::ready(!f.body.contains("{{#include")))
        .map(|f| async move {
            let failed = match Claim::of(&f.tag) {
                Claim::Nothing => return None,
                Claim::Sig => parser::parse_sig(Origin::unspecified(&f.body)).err(),
                Claim::Fragment => parser::parse(Origin::unspecified(&f.body)).err(),
                Claim::ReplSetup => return None,
                Claim::Accepted if f.body.contains('〉') => return None,
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
    let mut page = None;
    let mut repl = None;
    for f in &fences {
        let setup = matches!(Claim::of(&f.tag), Claim::ReplSetup);
        if !setup
            && !(matches!(Claim::of(&f.tag), Claim::Accepted) && f.body.contains('〉'))
        {
            continue;
        }
        if page.as_ref() != Some(&f.page) {
            repl = Some(Repl::new().await?);
            page = Some(f.page.clone());
        }
        let repl = repl.as_mut().unwrap();
        let result = if setup {
            repl.input(&f.body).map(|_| ())
        } else {
            repl.transcript(&f.body)
        };
        if let Err(e) = result {
            failures.push(format!("{}: {e:#}", f.at));
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
