//! A builtin is bound only in the source of a package: the signature a
//! binding writes is trusted, so a program that writes one of its own
//! over a registered builtin is refused, by the check and by a run, cold
//! and from a warm image.

use super::common;
use common::graphix;

const REFUSAL: &str =
    "builtin function str_trim can be bound only in the source of a package";

#[test]
fn a_program_binds_no_builtin() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "let f = |x: string| -> i64 'str_trim;\nprintln(f(\" a \") + 1);\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0))\n";
    for args in [&["--no-cache", "--check"][..], &["--no-cache"], &[], &[]] {
        let ran = graphix(dir.path(), program, args, &[]);
        assert!(!ran.status.success(), "{args:?} ran: {}", ran.stdout);
        assert!(ran.stderr.contains(REFUSAL), "{args:?}: {}", ran.stderr);
    }
}

#[test]
fn a_package_binds_its_own() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "println(str::trim(\" a \"));\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0))\n";
    let ran = graphix(dir.path(), program, &["--no-cache"], &[]);
    assert!(ran.status.success(), "{:?}: {}", ran.status, ran.stderr);
    assert_eq!(ran.stdout.trim(), "a");
}
