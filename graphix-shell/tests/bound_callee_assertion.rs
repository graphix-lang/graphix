//! A definition reached only through a callee bound at runtime has its
//! assertion checked when the call binds it; the program already runs,
//! so a failure is reported on stderr and in the log.

mod common;
use common::{CACHE_LOG, graphix, program_image};

const PROGRAM: &str = r#"
#[sync]
let f = |n: i64| throttle(#rate: duration:0.001s, n);
let b = true;
let g = select b { true => f, false => |n: i64| n };
println(g(1));
sys::exit(sys::time::after_idle(duration:100.ms, 0))
"#;

const MSG: &str = "#[sync]: this function is async";

#[test]
fn bound_callee_assertion_is_checked() {
    let dir = tempfile::tempdir().expect("tempdir");
    let ran = graphix(dir.path(), PROGRAM, &["--no-cache"], &[("RUST_LOG", "error")]);
    assert!(ran.stderr.contains(MSG), "no assertion failure on stderr:\n{}", ran.stderr);
    assert!(ran.log.contains(MSG), "no assertion failure in the log:\n{}", ran.log);
}

/// A pending assertion travels in the session image: a warm start's
/// run-time bind reports it as the cold start's does, and the program
/// runs on.
#[test]
fn bound_callee_assertion_survives_the_image() {
    let dir = tempfile::tempdir().expect("tempdir");
    let cold = graphix(dir.path(), PROGRAM, &[], &[CACHE_LOG]);
    let warm = graphix(dir.path(), PROGRAM, &[], &[CACHE_LOG]);
    assert_eq!(program_image(&cold), (true, false), "cold:\n{}", cold.log);
    assert_eq!(program_image(&warm), (false, true), "warm:\n{}", warm.log);
    for (run, ran) in [("cold", &cold), ("warm", &warm)] {
        assert!(ran.stderr.contains(MSG), "no assertion failure {run}:\n{}", ran.stderr);
        assert!(
            ran.stdout.lines().any(|l| l == "1"),
            "{run} did not run:\n{}",
            ran.stderr
        );
    }
}
