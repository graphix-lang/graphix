//! `--check` runs the check alone: the def assertions
//! (`#[tail_recursive]`/`#[sync]`/`#[async]`) are facts of the build's
//! `analysis::analyze`, and `#[native]` of its fusion pass, both of which
//! `--expand` runs.

use anyhow::Result;
use enumflags2::BitFlags;
use graphix_compiler::{CFlag, expr::Source};
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};

const FALSE_ASSERTION: &str = r#"
#[tail_recursive]
let f = |n: i64| -> i64 n;
f(i64:1)
"#;

/// A `#[native]` call whose callee prints, which no kernel can hold.
const FALSE_NATIVE: &str = r#"
let f = |x: i64| -> i64 { print(x); x * 2 + 1 };
let r = #[native] f(20);
r
"#;

async fn check(program: &'static str, flags: BitFlags<CFlag>) -> Result<()> {
    ShellBuilder::<NoExt>::default()
        .cache(CacheMode::Off)
        .mode(Mode::Check(Source::Internal(program.into())))
        .enable_flags(flags)
        .build()?
        .check()
        .await
}

#[tokio::test(flavor = "multi_thread")]
async fn check_leaves_def_assertions_to_the_build() -> Result<()> {
    check(FALSE_ASSERTION, BitFlags::empty()).await?;
    let e = match check(FALSE_ASSERTION, CFlag::ExpandSeq.into()).await {
        Ok(()) => panic!("--expand accepted a false #[tail_recursive]"),
        Err(e) => format!("{e:#}"),
    };
    assert!(
        e.contains("not recursive"),
        "--expand rejected it for the wrong reason: {e}"
    );
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn check_leaves_native_to_the_build() -> Result<()> {
    check(FALSE_NATIVE, BitFlags::empty()).await?;
    let e = match check(FALSE_NATIVE, CFlag::ExpandSeq.into()).await {
        Ok(()) => panic!("--expand accepted a #[native] that cannot fuse"),
        Err(e) => format!("{e:#}"),
    };
    assert!(
        e.contains("did not fully fuse"),
        "--expand rejected it for the wrong reason: {e}"
    );
    Ok(())
}
