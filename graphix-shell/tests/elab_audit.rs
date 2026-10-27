//! Every instance of a collection intrinsic is typed from its
//! definition's check table, its prototype call included: the
//! elaboration audit stays silent on a program that maps and folds.

mod common;
use common::graphix;

#[test]
fn collection_instances_substitute() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n\
                   let a = array::map([1, 2], |x| x + 1);\n\
                   let b = array::fold([1, 2, 3], 0, |acc, x| acc + x);\n\
                   let c = list::map(list::from_array([1, 2]), |x| x * 3);\n\
                   (a, b, c)\n";
    let ran =
        graphix(dir.path(), program, &["--no-cache"], &[("GRAPHIX_ELAB_AUDIT", "1")]);
    assert!(ran.status.success(), "{}", ran.stderr);
    assert_eq!(
        ran.stdout.lines().last(),
        Some("([2, 3], 6, [<3, 6>])"),
        "{}",
        ran.stdout
    );
    assert!(!ran.stderr.contains("ELAB-AUDIT"), "{}", ran.stderr);
}
