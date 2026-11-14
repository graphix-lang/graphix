//! An instance is typed from its definition's check table and decides
//! nothing its call's check left open: the elaboration audit stays silent.

use super::common;
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

/// A cell a call's check left open beside a decided member of its union
/// is open in the instance too: the instance's result narrows nothing.
#[test]
fn an_instance_leaves_open_what_its_call_left_open() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "type P<'p, 'q> = [Array<'p>, Array<'q>];\n\
                   let f = |k: fn(q: P<'b, 'a>) -> i64, y: 'a| k;\n\
                   let g = |t| f(|q| 7, t);\n\
                   let r = g(1.5);\n\
                   let s = g(\"s\");\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0));\n\
                   (r([1.5]), s([\"s\"]))\n";
    let ran =
        graphix(dir.path(), program, &["--no-cache"], &[("GRAPHIX_ELAB_AUDIT", "1")]);
    assert!(ran.status.success(), "{}", ran.stderr);
    assert_eq!(ran.stdout.lines().last(), Some("(7, 7)"), "{}", ran.stdout);
    assert!(!ran.stderr.contains("ELAB-AUDIT"), "{}", ran.stderr);
}

/// A site's return that holds a union lower bound member by member takes
/// an instance's equal union as the same bound: the unify-back narrows
/// nothing, under the second check as under substitution.
#[test]
fn an_instance_returning_the_sites_union_narrows_nothing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "type B<'p> = Abstract<'p>;\n\
                   let t = sys::time::timer(duration:10.ms, false);\n\
                   let go = |t| {\n\
                     let x = never();\n\
                     let b = B([x]);\n\
                     b <- B(x);\n\
                     let v = b.0;\n\
                     x <- t ~ 1;\n\
                     println(array::map([v], |e| e));\n\
                     null\n\
                   };\n\
                   go(t);\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0))\n";
    for env in [
        &[("GRAPHIX_ELAB_AUDIT", "1")][..],
        &[("GRAPHIX_ELAB_AUDIT", "1"), ("GRAPHIX_NO_SUBST", "1")][..],
    ] {
        let ran = graphix(dir.path(), program, &["--no-cache"], env);
        assert!(ran.status.success(), "{}", ran.stderr);
        assert_eq!(
            ran.stdout.lines().collect::<Vec<_>>(),
            ["[[1]]", "[1]"],
            "{}",
            ran.stdout
        );
        assert!(!ran.stderr.contains("ELAB-AUDIT"), "{env:?}: {}", ran.stderr);
    }
}
