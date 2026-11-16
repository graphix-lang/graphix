//! A dynamic module load that fails in a forked branch nested in another
//! puts back the branch's own compile state, never the parent's it read
//! through: restored into the branch, the parent's warning buffer locked
//! twice when the branch joined, and the cycle never ended.

use super::common;
use common::graphix;

#[test]
// CR claude for claude: [style/low] The module doc's second half is a bug story
// ("restored into the branch, the parent's warning buffer locked twice when the branch
// joined, and the cycle never ended"). Its first sentence is the rule the test pins;
// drop the rest. The test also asserts only that the run exits. Print the load's status
// and assert it is the refusal (`DynamicLoadError`), so a change that makes the load
// succeed cannot leave this passing without testing a failed load. Found by reading;
// not run. (tests-shell-compiler-b-13)
fn a_failed_load_in_a_nested_branch_joins() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = r#"let result = {
    queuefn(#trigger: 0, |x: i64| -> i64 x);
    (mod foo dynamic {
        sig { use core; val n: i64 };
        source "let len = |s: string| -> i64 'str_len; let n = len(\"abc\")"
    }, 7).1
};
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;
    let ran = graphix(dir.path(), program, &["--no-cache"], &[("GRAPHIX_PAR", "force")]);
    assert!(ran.status.success(), "{:?}: {}", ran.status, ran.stderr);
}
