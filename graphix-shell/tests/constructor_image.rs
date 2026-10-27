mod common;
use common::{CACHE_LOG, graphix, program_image};

#[test]
fn constructor_nodes_agree_cold_and_warm() {
    let dir = tempfile::tempdir().unwrap();
    let source = "\
type Box = Abstract<i64>;
let tick = sys::time::timer(duration:10.ms, false);
let p = {x: 1, y: 2};
let q = {p with x: 3};
let pair = (q.x, Box(5));
let m = {\"key\" => pair.0};
let r = seq tick; abort(never()) { m{\"key\"}?; `Got(pair.1.0) };
let result = select r { `Got(n) => q.x + pair.0 + pair.1.0 + m{\"key\"}$ + n };
sys::exit(sys::time::after_idle(duration:50.ms, 0));
result
";
    let cold = graphix(dir.path(), source, &["--no-fusion"], &[CACHE_LOG]);
    assert!(cold.status.success(), "{}", cold.stderr);
    assert_eq!(program_image(&cold), (true, false), "{}", cold.log);
    assert_eq!(cold.stdout.lines().last(), Some("19"), "{}", cold.stdout);
    let warm = graphix(dir.path(), source, &["--no-fusion"], &[CACHE_LOG]);
    assert!(warm.status.success(), "{}", warm.stderr);
    assert_eq!(program_image(&warm), (false, true), "{}", warm.log);
    assert_eq!(warm.stdout, cold.stdout);
}
