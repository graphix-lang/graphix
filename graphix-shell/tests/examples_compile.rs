//! Build every example program under `book/src/examples` and every
//! bench program against the full shell environment, as `graphix
//! --expand` does: checked, elaborated and fused, never run.

use anyhow::{Context, Result};
use futures::{StreamExt, stream};
use graphix_compiler::{
    CFlag,
    expr::{FilesResolver, Source},
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

// CR claude for claude: [test-gap/medium] This gate checks only the files under
// book/src/examples and bench. None of the ```graphix fences inline in the book's
// chapters (about 560 of them) are parsed or checked. So a syntax or typing change (the
// comment rule, Singleton arithmetic, type-directed reads) breaks book snippets with no
// failing test, which is what book-core-03, 04, 05, 06, 12, 13 and 14 found. Fix: add a
// test that extracts the fenced graphix blocks from book/src/**/*.md, skips REPL
// transcripts (lines starting with 〉) and blocks marked e.g. ```graphix,ignore or
// ```graphix,refused, and runs the rest through this same check. A refused block should
// be checked to be refused. Also reported as (book-stdlib-06). (book-core-10)
#[tokio::test(flavor = "multi_thread")]
// CR claude for claude: [test-gap/low] The TUI examples are only typechecked here.
// Nothing runs one for a few cycles or presses a key in one, so runtime defects pass
// the gate, such as the blank view after one Down in scroll_paragraph.gx
// (examples-tui-03). Add a graphix-package-tui test that loads each
// book/src/examples/tui/*.gx with TuiTestHarness, renders through a few updates, and
// asserts no error. For the interactive examples (list_interactive, tabs_navigation,
// overlay_modal, layout_nested_focus, the scroll examples), also dispatch a scripted
// key sequence and assert the rendered lines. (examples-tui-09)
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
                .mode(Mode::Check(Source::File(f.clone())))
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
