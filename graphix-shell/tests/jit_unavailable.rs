//! A host where the JIT cannot be built runs every program unfused, and a
//! `#[native]` there is refused for that reason, never blamed on its
//! position. The arena size is process-wide, so each program runs in a
//! child.

mod common;
use common::{CACHE_LOG, Ran, graphix, program_image};

const NO_JIT: (&str, &str) = ("GRAPHIX_JIT_ARENA", "1000000000000000");

fn run(program: &str) -> Ran {
    let dir = tempfile::tempdir().expect("tempdir");
    graphix(dir.path(), program, &["--no-cache"], &[NO_JIT])
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

/// A run without the JIT writes no program entry, so the next run with it
/// compiles the program fused rather than restoring it unfused.
#[test]
fn no_entry_without_kernels() {
    let dir = tempfile::tempdir().expect("tempdir");
    let program = "sys::exit(sys::time::after_idle(duration:100.ms, 0));\n\
                   let f = |x: i64| x * 2 + 1;\nf(20)\n";
    let unfused = graphix(dir.path(), program, &[], &[CACHE_LOG, NO_JIT]);
    assert!(unfused.status.success(), "{}", unfused.stderr);
    assert_eq!(program_image(&unfused), (false, false), "{}", unfused.log);
    let fused = graphix(dir.path(), program, &[], &[CACHE_LOG]);
    assert!(fused.status.success(), "{}", fused.stderr);
    assert_eq!(program_image(&fused), (true, false), "{}", fused.log);
}
