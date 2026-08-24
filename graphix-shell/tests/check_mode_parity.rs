//! The `--check` diagnostic for a program must be IDENTICAL with and
//! without fusion (modulo tvar numbering): merely ATTEMPTING fusion
//! must not rewrite the program's static types
//! (fusion-mutates-tvars-aug2026 — `freeze_for_abi_normalized`'s old
//! middle rung normalized shared TVar cells in place, so `--check`
//! reported different types per mode on a program where nothing
//! fuses), and no pass the fusion gate owns may leave the two modes
//! with different type STATE to walk (typedef-cell-mode-parity-aug2026
//! — `seed_typedef_refs` filled the stdlib's carried resolution cells
//! only when fusion was enabled).

use anyhow::Result;
use graphix_compiler::{CFlag, expr::Source};
use graphix_rt::NoExt;
use graphix_shell::{Mode, ShellBuilder};
use std::path::Path;

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

/// Strip tvar numbers (`'_6070` → `'_N`) so fresh-counter drift
/// between the two compiles doesn't hide or fake a difference.
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

/// Witnesses, each a program that must FAIL `--check` identically in
/// both modes. `fusion-mutates-tvars` is the region-classification
/// rewrite; `typedef-cell-mode-parity` is the stdlib typedef whose
/// resolution cells were seeded only under fusion, so the two modes
/// walked `contains` differently through a `List<'a>` alias.
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
