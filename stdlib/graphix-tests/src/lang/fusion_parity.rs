//! Fusion must never change what the typechecker sees: a program compiled
//! statement by statement, as the REPL does (fusion runs after each
//! statement, before the next is checked), is refused with the same
//! diagnostic node-walked and fused (modulo tvar numbering).

use anyhow::{Context, Result};
use arcstr::ArcStr;
use graphix_compiler::expr::{Origin, Source, parser};
use graphix_package_core::testing::{Mode, fixture_runtime};

/// A fuzz finding, by its path under `graphix-fuzz/findings`, and its text.
macro_rules! witness {
    ($path:literal) => {
        ($path, include_str!(concat!("../../../../graphix-fuzz/findings/", $path)))
    };
}

/// Witnesses, each a program one of whose statements must be refused.
const WITNESSES: &[(&str, &str)] = &[
    // CR claude for claude: [test-gap/medium] This witness no longer depends on what
    // fusion does. The select's type is already i64 before fusion runs: the node-walk
    // build, the fused build and the check alone all refuse with "string does not
    // contain i64". Its header records a union for the node-walk instead. If fusion
    // went back to normalizing the original type in place (the hazard
    // try_freeze_for_abi_normalized documents), this text would not change and the gate
    // would still pass. Fix: find a witness that fails with the in-place normalize
    // reintroduced, or also compare each statement's compiled type (printed without
    // normalizing) between Mode::Jit and Mode::Interp. probe:
    // design/review-2026-10-26/repro/tests-fusion-traits-parity-witness-vacuous.sh
    // (tests-fusion-traits-02)
    witness!("fusion-mutates-tvars-aug2026/00_check_diagnostic_type_differs.gx"),
    witness!("typedef-cell-mode-parity-aug2026/00_stdlib_alias_partial_bind.gx"),
    witness!("typedef-cell-mode-parity-aug2026/01_labeled_args_sort.gx"),
    witness!("typedef-cell-mode-parity-aug2026/02_fold_seeded_acc.gx"),
];

/// The first refusal compiling `text`'s statements one at a time, the
/// earlier ones kept.
async fn first_refusal(name: &str, text: &str, mode: Mode) -> Result<String> {
    let text = ArcStr::from(text);
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
    anyhow::bail!("{name} compiled")
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
    for (w, text) in WITNESSES {
        let fused = first_refusal(w, text, Mode::Jit).await.context(*w)?;
        let walked = first_refusal(w, text, Mode::Interp).await.context(*w)?;
        assert_eq!(
            norm(&fused),
            norm(&walked),
            "{w}: the refusal differs between fusion modes:\nfused: {fused}\nnode-walked: {walked}"
        );
    }
    Ok(())
}
