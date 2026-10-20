//! A script whose root fuses prints its value through the types its
//! typedefs name: the kernel owns them once the region it replaced, which
//! declared them, is gone.

mod common;
use common::graphix;

const PROGRAM: &str = r#"
type C = {col: i64};
type S = {inner: C, n: i64};
let s: S = {inner: {col: 1}, n: 2};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
s
"#;

#[test]
fn fused_root_prints_through_its_typedefs() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), PROGRAM, &["--no-cache"], &[]);
    assert!(ran.stdout.contains("{inner: {col: 1}, n: 2}"), "{}", ran.stdout);
}
