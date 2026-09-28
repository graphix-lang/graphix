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
