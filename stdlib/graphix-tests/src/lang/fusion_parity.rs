//! Fusion must never change what the typechecker sees: a program compiled
//! statement by statement, as the REPL does (fusion runs after each
//! statement, before the next is checked), is refused with the same
//! diagnostic node-walked and fused (modulo tvar numbering).

use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::{Origin, Source, parser};
use graphix_package_core::testing::{Mode, fixture_runtime};
use std::path::Path;

/// Witnesses, each a program one of whose statements must be refused.
const WITNESSES: &[&str] = &[
    "fusion-mutates-tvars-aug2026/00_check_diagnostic_type_differs.gx",
    "typedef-cell-mode-parity-aug2026/00_stdlib_alias_partial_bind.gx",
    "typedef-cell-mode-parity-aug2026/01_labeled_args_sort.gx",
    "typedef-cell-mode-parity-aug2026/02_fold_seeded_acc.gx",
];

/// The first refusal compiling `file`'s statements one at a time, the
/// earlier ones kept.
async fn first_refusal(file: &Path, mode: Mode) -> Result<String> {
    let text = ArcStr::from(std::fs::read_to_string(file)?);
    let ori = Origin { parent: None, source: Source::Unspecified, text };
    let stmts = parser::parse(ori)?;
    let (ctx, _rx) = fixture_runtime([], crate::TEST_REGISTER, mode, |_| {}).await?;
    let mut held = Vec::new();
    for stmt in stmts.iter() {
        match ctx.rt.compile(ArcStr::from(stmt.to_string())).await {
            Ok(res) => held.push(res),
            Err(e) => {
                ctx.shutdown().await;
                return Ok(format!("{e:#}"));
            }
        }
    }
    anyhow::bail!("{} compiled", file.display())
}

/// Strip tvar numbers (`'_6070` → `'_N`) so counter drift between the
/// two compiles neither hides nor fakes a difference.
fn norm(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        out.push(c);
        if c == '\'' && chars.peek() == Some(&'_') {
            out.push(chars.next().unwrap());
            while chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                chars.next();
            }
            out.push('N');
        }
    }
    out
}

#[tokio::test(flavor = "current_thread")]
async fn refusals_identical_across_fusion() -> Result<()> {
    let findings =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../../graphix-fuzz/findings");
    for w in WITNESSES {
        let f = findings.join(w);
        let fused = first_refusal(&f, Mode::Jit).await.context(*w)?;
        let walked = first_refusal(&f, Mode::Interp).await.context(*w)?;
        assert_eq!(
            norm(&fused),
            norm(&walked),
            "{w}: the refusal differs between fusion modes:\nfused: {fused}\nnode-walked: {walked}"
        );
    }
    Ok(())
}
