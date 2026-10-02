//! `--check` checks a script as it runs: the file is one block, so a
//! top-level `let` over ⊥ takes its type from the writers below it.

use anyhow::Result;
use graphix_compiler::expr::Source;
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};

const WRITERS_BELOW: &str = r#"
let x = never();
x <- u64:0;
let y: u64 = x;
y
"#;

#[tokio::test(flavor = "multi_thread")]
async fn check_types_a_top_level_let_by_its_writers() -> Result<()> {
    ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::Internal(WRITERS_BELOW.into())))
        .build()?
        .check()
        .await
}

/// The check runs no fusion pass, so it leaves `#[native]` to the build:
/// an attribute that a build would dispatch is no error of the check.
const NATIVE: &str = r#"
let f = |x: i64| -> i64 x * 2 + 1;
let r = #[native] f(20);
let a = #[native] array::map([1, 2, 3], |x| x + 1);
(r, a)
"#;

#[tokio::test(flavor = "multi_thread")]
async fn check_leaves_native_to_the_build() -> Result<()> {
    ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::Internal(NATIVE.into())))
        .build()?
        .check()
        .await
}
