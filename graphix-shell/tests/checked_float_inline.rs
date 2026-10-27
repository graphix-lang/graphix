//! Checked float arithmetic follows IEEE and never fails, so a fused
//! `+?`, `-?`, `*?` or `/?` over one float type is the plain operation
//! inline: its kernel calls no helper.

mod common;
use common::graphix;

const PROGRAM: &str = "\
let add = |x: f64, y: f64| x +? y;
let sub = |x: f64, y: f64| x -? y;
let mul = |x: f64, y: f64| x *? y;
let div = |x: f32, y: f32| x /? y;
let x = sys::time::after_idle(duration:1.ms, 6.0);
let r = (add(x, 3.0), sub(x, 3.0), mul(x, 3.0), div(cast<f32>(x)$, f32:3.0));
sys::exit(select r { (9.0, 3.0, 18.0, f32:2.0) => 0, _ => 1 })
";

/// The lines of the kernel `name`'s CLIF.
fn kernel<'a>(clif: &'a str, name: &str) -> Vec<&'a str> {
    let header = format!(";; clif {name}");
    let mut lines = clif.lines().skip_while(|l| *l != header);
    match lines.next() {
        Some(_) => lines.take_while(|l| *l != "}").collect(),
        None => panic!("`{name}` did not fuse:\n{clif}"),
    }
}

#[test]
fn checked_float_arith_calls_no_helper() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran =
        graphix(dir.path(), PROGRAM, &["--no-cache"], &[("GRAPHIX_DUMP_CLIF", "1")]);
    assert!(ran.status.success(), "the program computed the wrong value: {}", ran.stderr);
    for name in ["add", "sub", "mul", "div"] {
        let calls: Vec<_> = kernel(&ran.stderr, name)
            .into_iter()
            .filter(|l| l.contains("call "))
            .collect();
        assert!(calls.is_empty(), "`{name}` calls helpers: {calls:#?}");
    }
}
