//! `--check` must run `analysis::analyze`: the def assertions
//! (`#[tail_recursive]`/`#[sync]`/`#[async]`) are verified there.

use anyhow::Result;
use graphix_compiler::expr::Source;
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};

const FALSE_ASSERTION: &str = r#"
#[tail_recursive]
let f = |n: i64| -> i64 n;
f(i64:1)
"#;

#[tokio::test(flavor = "multi_thread")]
async fn check_rejects_false_def_assertion() -> Result<()> {
    let r = ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::Internal(FALSE_ASSERTION.into())))
        .build()?
        .check()
        .await;
    let e = match r {
        Ok(()) => panic!("--check accepted a false #[tail_recursive]"),
        Err(e) => format!("{e:#}"),
    };
    assert!(
        e.contains("not recursive"),
        "--check rejected the witness for the wrong reason: {e}"
    );
    Ok(())
}
