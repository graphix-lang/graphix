//! `--check` runs the check alone: the def assertions
//! (`#[tail_recursive]`/`#[sync]`/`#[async]`) are facts of the build's
//! `analysis::analyze`, which `--expand` runs.

use anyhow::Result;
use enumflags2::BitFlags;
use graphix_compiler::{CFlag, expr::Source};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};

const FALSE_ASSERTION: &str = r#"
#[tail_recursive]
let f = |n: i64| -> i64 n;
f(i64:1)
"#;

async fn check(flags: BitFlags<CFlag>) -> Result<()> {
    ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::Internal(FALSE_ASSERTION.into())))
        .enable_flags(flags)
        .build()?
        .check()
        .await
}

// CR claude for claude: [readability] The file is named for the opposite of what it pins:
// the check no longer runs analyze, and this test asserts that --check accepts a false
// #[tail_recursive] that --expand refuses. Rename it, e.g.
// check_leaves_assertions_to_the_build.rs. check_whole_script.rs's
// check_leaves_native_to_the_build belongs beside it, but its NATIVE program builds
// too. A #[native] the build refuses, `let f = |x: i64| -> i64 { print(x); x * 2 + 1 };
// let r = #[native] f(20); r` (--check exits 0, --expand exits 1), would pin the native
// half the way FALSE_ASSERTION pins this one. (tests-shell-compiler-16)
#[tokio::test(flavor = "multi_thread")]
async fn check_leaves_def_assertions_to_the_build() -> Result<()> {
    check(BitFlags::empty()).await?;
    let e = match check(CFlag::ExpandSeq.into()).await {
        Ok(()) => panic!("--expand accepted a false #[tail_recursive]"),
        Err(e) => format!("{e:#}"),
    };
    assert!(
        e.contains("not recursive"),
        "--expand rejected it for the wrong reason: {e}"
    );
    Ok(())
}
