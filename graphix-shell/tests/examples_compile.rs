//! Compile-check every example program under `book/src/examples`
//! against the full shell environment, as `graphix --check` would.

use anyhow::{Context, Result};
use futures::{StreamExt, stream};
use graphix_compiler::expr::{FilesResolver, Source};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
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

// CR claude for eric: [test-gap] Mode::Check is the check alone (CFlag::CheckOnly): the
// examples get typecheck0 and its settle but no elaboration, analysis or fusion, and no
// other test builds them. An example that elaboration refuses after the check accepted
// it (a type-system bug by CLAUDE.md), or one whose fusion link panics, ships with this
// test green. Also build each example without running it. CFlag::ExpandSeq is today's
// only build-without-run path (all 122 build that way in about 12 s with the debug
// binary); a build-only flag would also do. (tests-shell-compiler-14)
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
    let failures: Vec<String> = stream::iter(files)
        .map(|f| async move {
            let base = f.parent().expect("example has a parent dir").to_path_buf();
            // CR claude for eric: [risk] This shell uses the developer's real image
            // cache, as do those in check_mode_parity, check_numeric_singleton,
            // check_runs_analyze, check_whole_script and the deep_nesting children.
            // init_limit_logs, swallowed_error_logs, interrupt_wedge and
            // recursion_memory spawn graphix without --no-cache. Every store removes
            // every other build id's directory (graphix-shell/src/cache.rs:200-205), so
            // a test run wipes the installed graphix's warm entries and other
            // worktrees'. Whether a test compiles cold or restores an image then
            // depends on earlier runs; recursion_memory's peak RSS includes an image
            // encode only when cold. Set .no_cache(true) as import_cycle.rs does and
            // pass --no-cache to spawned binaries, or point XDG_CACHE_HOME at the
            // test's temp dir where cold versus warm should be explicit.
            // (tests-shell-compiler-11)
            let r = ShellBuilder::<NoExt>::default()
                .module_resolvers(vec![FilesResolver::new(base, None)])
                // CR claude for eric: [test-gap] Mode::Check runs the check alone
                // (CFlag::CheckOnly), with no elaboration and no fusion. So an example
                // that elaboration refuses, or a failing def assertion, still passes.
                // bench/*.gx and bench/collection/*.gx are only parsed
                // (graphix-compiler/tests/expr_spans.rs:118), so nothing guards
                // par_symbolic.gx's `#[parallel]`: `--check` accepts `#[parallel] x +
                // 1`, which `--expand` refuses. All 163 example and bench programs
                // build under --expand today; compile them here through that build path
                // (elaborate and fuse, no run). book/src/examples/README.md:25-28 and
                // CLAUDE.md:840-842 still say some examples reference undefined names,
                // which this test forbids. (examples-11)
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
