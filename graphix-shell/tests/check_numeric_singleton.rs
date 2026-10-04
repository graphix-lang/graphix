//! The check refuses arithmetic over a type holding two numeric types
//! where a generic definition is called (`'a: Number + Singleton`), so
//! `--check` and the language server refuse what the build refuses.

use anyhow::Result;
use graphix_compiler::expr::Source;
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};

async fn check(src: &str) -> Result<()> {
    ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::Internal(src.into())))
        .build()?
        .check()
        .await
}

#[tokio::test(flavor = "multi_thread")]
async fn check_refuses_generic_arithmetic_over_a_mixed_union() -> Result<()> {
    check("let f = |x, y| x + y;\nlet a: i64 = 1;\nf(a, a)\n").await?;
    for src in [
        "let f = |x, y| x + y;\nlet a: [i64, f64] = 1;\nf(a, a)\n",
        "let f = 'a: Number |x: 'a, y: 'a| -> 'a x + y;\nlet a: [i64, f64] = 1;\nf(a, a)\n",
    ] {
        let e = match check(src).await {
            Ok(()) => panic!("--check accepted {src}"),
            Err(e) => format!("{e:#}"),
        };
        assert!(e.contains("Singleton"), "{src}: {e}");
    }
    Ok(())
}
