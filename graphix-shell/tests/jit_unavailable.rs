//! A host where the JIT cannot be built runs every program unfused, and a
//! `#[native]` there is refused for that reason, never blamed on its
//! position. The arena size is process-wide, so each program runs in a
//! child.

mod common;
use common::{Ran, graphix};

fn run(program: &str) -> Ran {
    let dir = tempfile::tempdir().expect("tempdir");
    graphix(
        dir.path(),
        program,
        &["--no-cache"],
        &[("GRAPHIX_JIT_ARENA", "1000000000000000")],
    )
}

#[test]
fn native_names_the_missing_jit() {
    let exit = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n";
    let ran = run(&format!("{exit}let f = |x: i64| x * 2 + 1;\nf(20)\n"));
    assert_eq!(ran.stdout.lines().last(), Some("41"));
    let ran = run(&format!("{exit}let f = |x: i64| x * 2 + 1;\n#[native] f(20)\n"));
    assert!(ran.stderr.contains("#[native] cannot hold"), "{}", ran.stderr);
    assert!(ran.stderr.contains("fusion is unavailable"), "{}", ran.stderr);
}
