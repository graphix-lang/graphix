//! A builtin whose `eval` gave no value over arguments that do not
//! change stands bottom: it is not evaluated again, and logs once. Its
//! arithmetic diagnostic is the node-walk's alone, as an operator's is:
//! fused, it logs nothing.

mod common;
use common::graphix;

/// `divide` over args that fire only at init, beside a counter that
/// keeps its statement updating.
const PROGRAM: &str = r#"
let b = 0;
let ep = 0;
ep <- select ep { n if n < 20 => n + 1, _ => never() };
let t = (ep, divide(10, b));
sys::exit(select ep { 20 => 0, _ => never() })
"#;

fn divide_logs(fusion: bool) -> usize {
    let dir = tempfile::tempdir().expect("tempdir");
    let args: &[&str] = match fusion {
        true => &["--no-cache"],
        false => &["--no-cache", "--no-fusion"],
    };
    let ran = graphix(dir.path(), PROGRAM, args, &[("RUST_LOG", "error")]);
    assert!(ran.status.success(), "graphix failed: {}", ran.stderr);
    ran.log.matches("divide").count()
}

#[test]
fn a_standing_divide_by_zero_logs_once() {
    assert_eq!(divide_logs(false), 1, "node-walk");
    assert_eq!(divide_logs(true), 0, "fusion");
}

/// A failing `divide` is reported on stderr by the node-walk, as `/`'s
/// failure is.
#[test]
fn a_failing_divide_is_reported() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "let z = 0;\nlet r = divide(7, z);\n\
                   sys::exit(sys::time::after_idle(duration:100.ms, 0))\n";
    let ran = graphix(dir.path(), program, &["--no-cache", "--no-fusion"], &[]);
    assert!(ran.stderr.contains("divide:"), "{}", ran.stderr);
}
