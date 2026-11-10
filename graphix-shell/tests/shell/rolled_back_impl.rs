//! A REPL input that fails takes its impls with it: a comparison an
//! earlier input's value takes is judged again without them.

use super::common::repl;

fn refused_after_rollback(input: &str, refusal: &str, ok: &str) {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = repl(dir.path(), &["--no-cache"], input);
    assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
    let all = format!("{}{}", ran.stdout, ran.stderr);
    assert!(all.contains(refusal), "{all}");
    assert!(!ran.stdout.contains(ok), "{all}");
}

#[test]
fn a_rolled_back_eq_impl_leaves_no_equality() {
    refused_after_rollback(
        "type U = Abstract<[fn(x: i64) -> i64, fn(x: string) -> i64]>\n\
         let f: [fn(x: i64) -> i64, fn(x: string) -> i64] = |x: i64| x\n\
         let u = [U(f)]\n\
         impl Eq for U { let eq = |a, b| true }; let r = u == u; let bad: i64 = \"s\"\n\
         u == u\n\
         sys::exit(3)\n",
        "U has no Eq implementation",
        "-: bool",
    )
}

#[test]
fn a_rolled_back_ord_impl_leaves_no_order() {
    refused_after_rollback(
        "type B = Abstract<[i64, f64]>\n\
         let u = [B(1)]\n\
         impl Ord for B { let cmp = |a, b| `Equal }; let r = u < u; let bad: i64 = \"s\"\n\
         u < u\n\
         sys::exit(3)\n",
        "B has no Ord implementation",
        "-: bool",
    )
}
