//! An `array::init` count over the element limit is bottom, and the
//! node-walk says so once per fired count, not once per cycle it reads
//! the standing count.

mod common;
use common::graphix;

/// A fixed oversize count beside a clock that keeps the graph cycling.
const PROGRAM: &str = r#"
let clock = sys::time::timer(duration:10.ms, true);
let n = 0;
n <- clock ~ n + 1;
let a = array::init(20000000, |i| i);
sys::exit(select n { 5 => 0, _ => never() })
"#;

#[test]
fn oversize_init_logs_once() {
    let dir = tempfile::tempdir().expect("tempdir");
    let args = ["--no-cache", "--no-fusion"];
    let ran = graphix(dir.path(), PROGRAM, &args, &[("RUST_LOG", "warn")]);
    assert!(ran.status.success(), "graphix failed: {}", ran.stderr);
    assert_eq!(ran.log.matches("exceeds the").count(), 1, "{}", ran.log);
}
