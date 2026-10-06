//! The `--check` diagnostic for a program must be identical with and
//! without fusion (modulo tvar numbering): attempting fusion must not
//! change the program's static types.

use anyhow::Result;
use graphix_compiler::{CFlag, expr::Source};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
use std::path::Path;

// CR claude for eric: [test-gap] This comparison cannot fail. Mode::Check runs
// GXRt::check with CFlag::CheckOnly, which returns after typecheck0
// (graphix-compiler/src/lib.rs:1958) before anything reads ctx.fusion.enabled, and the
// package root always compiles with fusion off, so both sides run the same code:
// witness 00 prints the same `i64` diagnostic with and without --no-fusion. That leaves
// 'a pass the fusion gate owns must never change what the typechecker sees' unpinned.
// Instead, drive each witness as separate per-statement compiles inside
// ctx.rt.with_ctx, the REPL's shape where fusion runs between statements
// (kernel_outlives_jit_reset in stdlib/graphix-tests/src/lib_tests/lsp_fusion.rs does
// this). Compile once with FusionDisabled and once without, and compare the later
// statement's error. (tests-shell-compiler-03)
async fn check_err(file: &Path, no_fusion: bool) -> String {
    let mut b = ShellBuilder::<NoExt>::default()
        .mode(Mode::Check(Source::File(file.to_path_buf())));
    if no_fusion {
        b = b.enable_flags(CFlag::FusionDisabled.into());
    }
    match b.build().expect("building shell").check().await {
        Ok(()) => String::from("OK"),
        Err(e) => format!("{e:#}"),
    }
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

/// Witnesses, each a program that must fail `--check` identically in
/// both modes.
const WITNESSES: &[&str] = &[
    "../graphix-fuzz/findings/fusion-mutates-tvars-aug2026/00_check_diagnostic_type_differs.gx",
    "../graphix-fuzz/findings/typedef-cell-mode-parity-aug2026/00_stdlib_alias_partial_bind.gx",
    "../graphix-fuzz/findings/typedef-cell-mode-parity-aug2026/01_labeled_args_sort.gx",
    "../graphix-fuzz/findings/typedef-cell-mode-parity-aug2026/02_fold_seeded_acc.gx",
];

#[tokio::test(flavor = "multi_thread")]
async fn check_diagnostics_mode_identical() -> Result<()> {
    for w in WITNESSES {
        let f = Path::new(env!("CARGO_MANIFEST_DIR")).join(w);
        let fused = check_err(&f, false).await;
        let interp = check_err(&f, true).await;
        assert_ne!(fused, "OK", "{w}: the witness program must fail --check");
        assert_eq!(
            norm(&fused),
            norm(&interp),
            "{w}: --check diagnostic differs between fusion modes:\nfusion: {fused}\ninterp: {interp}"
        );
    }
    Ok(())
}
