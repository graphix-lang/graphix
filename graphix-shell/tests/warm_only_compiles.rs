//! `--warm` writes the image cache and runs nothing: the program's
//! effects wait for a real run.

mod common;
use common::{CACHE_LOG, graphix, program_image};

#[test]
fn warm_writes_the_entry_and_runs_nothing() {
    let dir = tempfile::tempdir().expect("tempdir");
    let out = dir.path().join("out.txt");
    let program = format!(
        "println(\"SIDE EFFECT\");\n\
         sys::fs::write_all(#path: \"{}\", \"written\")\n",
        out.display()
    );
    let warm = graphix(dir.path(), &program, &["--warm"], &[CACHE_LOG]);
    assert!(warm.status.success(), "{}", warm.stderr);
    assert_eq!(program_image(&warm), (true, false), "{}", warm.log);
    assert!(!warm.stdout.contains("SIDE EFFECT"), "{}", warm.stdout);
    assert!(!out.exists(), "--warm ran the program");
}

/// A program that does not compile fails `--warm`.
#[test]
fn warm_fails_a_program_that_does_not_compile() {
    let dir = tempfile::tempdir().expect("tempdir");
    let warm = graphix(dir.path(), "1 + \"a\"\n", &["--warm"], &[]);
    assert!(!warm.status.success(), "{}", warm.stderr);
}
