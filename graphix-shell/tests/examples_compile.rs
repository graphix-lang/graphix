//! Compile-check every example program under `book/src/examples`
//! against the FULL shell environment (all stdlib packages), exactly
//! as `graphix --check` would. The examples double as documentation
//! and as a regression net for typing changes — the 2026-07-15
//! env-sensitive elaboration bug broke a GUI example without any test
//! noticing, because example compilation was manual-only.

use anyhow::{Context, Result};
use futures::{StreamExt, stream};
use graphix_compiler::expr::{ModuleResolver, Source};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
use netidx::InternalOnly;
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
            p.file_stem()
                .and_then(|s| s.to_str())
                .is_none_or(|s| !submodules.contains(s))
        }));
    }
    files.sort();
    Ok(files)
}

#[tokio::test(flavor = "multi_thread")]
async fn examples_compile() -> Result<()> {
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../book/src/examples");
    let files = example_files(&examples)?;
    assert!(
        files.len() >= 100,
        "examples dir looks wrong: only {} files under {}",
        files.len(),
        examples.display()
    );
    let internal = InternalOnly::new().await?;
    let failures: Vec<String> = stream::iter(files)
        .map(|f| {
            let publisher = internal.publisher().clone();
            let subscriber = internal.subscriber().clone();
            async move {
                let base = f.parent().expect("example has a parent dir").to_path_buf();
                let r = ShellBuilder::<NoExt>::default()
                    .publisher(publisher)
                    .subscriber(subscriber)
                    .module_resolvers(vec![ModuleResolver::Files {
                        base,
                        overrides: None,
                    }])
                    .mode(Mode::Check(Source::File(f.clone())))
                    .build()
                    .expect("building shell")
                    .check()
                    .await;
                r.err().map(|e| format!("{}: {e:#}", f.display()))
            }
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
