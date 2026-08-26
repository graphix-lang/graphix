//! `--check` must run `analysis::analyze` — the def assertions
//! (`#[tail_recursive]`/`#[sync]`/`#[async]`) are verified there, so a
//! check channel that stops before it passes a program that fails at
//! load (design/recursive_activations.md, "Found during P2b" item 1;
//! MUST FIX, Eric 2026-08-25). `compile_stmt` — the per-statement
//! entry `check_inner` drives — carries the analyze call today; this
//! pins the contract against any future split of the two channels.

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
