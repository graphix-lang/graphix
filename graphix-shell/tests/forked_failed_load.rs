//! A dynamic module load that fails in a forked branch nested in another
//! puts back the branch's own compile state, never the parent's it read
//! through: restored into the branch, the parent's warning buffer locked
//! twice when the branch joined, and the cycle never ended.

mod common;
use common::graphix;

#[test]
fn a_failed_load_in_a_nested_branch_joins() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = r#"let result = {
    queuefn(#trigger: 0, |x: i64| -> i64 x);
    (mod foo dynamic {
        sandbox whitelist [core];
        sig { val n: i64 };
        source "let len = |s: string| -> i64 'str_len; let n = len(\"abc\")"
    }, 7).1
};
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;
    let ran = graphix(dir.path(), program, &["--no-cache"], &[("GRAPHIX_PAR", "force")]);
    assert!(ran.status.success(), "{:?}: {}", ran.status, ran.stderr);
}
