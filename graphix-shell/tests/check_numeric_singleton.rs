//! The check refuses arithmetic over a type holding two numeric types
//! where a generic definition is called (`'a: Number + Singleton`), so
//! `--check` and the language server refuse what the build refuses.

use anyhow::Result;
use graphix_compiler::expr::Source;
use graphix_rt::NoExt;
use graphix_shell::{CacheMode, Mode, ShellBuilder};

async fn check(src: &str) -> Result<()> {
    ShellBuilder::<NoExt>::default()
        .cache(CacheMode::Off)
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

/// `'a: [Number, null] + OneNumber` takes a nullable number of one
/// numeric type, and the check refuses a type holding two.
#[tokio::test(flavor = "multi_thread")]
async fn check_refuses_two_numbers_under_one_number() -> Result<()> {
    const F: &str = "let f = 'a: [Number, null] + OneNumber |x: 'a| -> 'a x;\n";
    for ok in ["let a: [i64, null] = null;\nf(a)\n", "f(u8:3)\n"] {
        check(&format!("{F}{ok}")).await?;
    }
    for src in ["let a: [i64, f64, null] = 1;\nf(a)\n", "let a: [i64, f64] = 1;\nf(a)\n"]
    {
        let src = format!("{F}{src}");
        let e = match check(&src).await {
            Ok(()) => panic!("--check accepted {src}"),
            Err(e) => format!("{e:#}"),
        };
        assert!(e.contains("OneNumber"), "{src}: {e}");
    }
    Ok(())
}
