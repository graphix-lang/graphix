//! A lambda kernel's body is built once however many regions call it,
//! whatever else those regions call: `outer` calls `inner`, and each of
//! ten regions calls `outer` beside a leaf of its own.

mod common;
use common::graphix;

const PROGRAM: &str = "\
let inner = |x: i64| x * x + 1;
let outer = |x: i64| inner(x) + inner(x + 1);
let leaf0 = |x: i64| x * 2;
let leaf1 = |x: i64| x * 3;
let leaf2 = |x: i64| x * 4;
let leaf3 = |x: i64| x * 5;
let leaf4 = |x: i64| x * 6;
let g = |a: i64| {
  let r0 = outer(a) + leaf0(a);
  let r1 = outer(a) + leaf1(a);
  let r2 = outer(a) + leaf2(a);
  let r3 = outer(a) + leaf3(a);
  let r4 = outer(a) + leaf4(a);
  let c = count(a);
  [r0, r1, r2, r3, r4, c]
};
let r = g(sys::time::after_idle(duration:1.ms, 2));
sys::exit(select r { [19, 21, 23, 25, 27, 1] => 0, _ => 1 })
";

#[test]
fn a_body_with_calls_builds_once() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran =
        graphix(dir.path(), PROGRAM, &["--no-cache"], &[("GRAPHIX_DUMP_CLIF", "1")]);
    assert!(ran.status.success(), "the program computed the wrong value: {}", ran.stderr);
    let built = |name: &str| {
        let header = format!(";; clif {name}");
        ran.stderr.lines().filter(|l| *l == header).count()
    };
    assert_eq!(built("leaf0"), 1, "the regions did not fuse:\n{}", ran.stderr);
    assert_eq!(built("inner"), 1);
    assert_eq!(built("outer"), 1, "`outer` was built once per region");
}
