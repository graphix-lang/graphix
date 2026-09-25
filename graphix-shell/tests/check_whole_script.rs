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
