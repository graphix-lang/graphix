//! `sys::exit` ends a script after its output: the value of the cycle
//! that asks to exit, and of every cycle before it, is printed first, on
//! a cold cache as on a warm one. A runtime that stops reports why.

use std::{fs, process::Command};

/// Run `program` as a script with `env`, cached under `cache`; its
/// stdout and stderr.
fn run(program: &str, cache: &std::path::Path, env: &[(&str, &str)]) -> (String, String) {
    let dir = tempfile::tempdir().expect("tempdir");
    let path = dir.path().join("main.gx");
    fs::write(&path, program).expect("write the script");
    let out = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-netidx")
        .arg(&path)
        .env("XDG_CACHE_HOME", cache)
        .envs(env.iter().copied())
        .output()
        .expect("run graphix");
    (
        String::from_utf8_lossy(&out.stdout).into(),
        String::from_utf8_lossy(&out.stderr).into(),
    )
}

#[test]
fn exit_prints_its_own_cycle() {
    for program in [
        "let r = 42;\nsys::exit(r ~ 0);\nr\n",
        "let x = sys::time::after_idle(duration:50.ms, 42);\nsys::exit(x ~ 0);\nx\n",
    ] {
        for _ in 0..5 {
            // a fresh cache each time: the first run stores its images
            let cache = tempfile::tempdir().expect("tempdir");
            let (out, err) = run(program, cache.path(), &[]);
            assert_eq!(out.trim(), "42", "{program}\n{err}");
        }
    }
}

#[test]
fn a_stopped_runtime_says_why() {
    let program = "let rec f = |n: i64| -> i64 select n { 0 => 0, k => k + f(k - 1) };\n\
                   let depth = 0;\n\
                   depth <- select depth { d if d < 100 => d + 10, _ => never() };\n\
                   sys::exit(sys::time::after_idle(duration:300.ms, depth ~ 0));\n\
                   f(depth)\n";
    for _ in 0..5 {
        let cache = tempfile::tempdir().expect("tempdir");
        let (_, err) = run(program, cache.path(), &[("GRAPHIX_ACTIVATION_BUDGET", "20")]);
        assert!(err.contains("exceeded the runtime's budget"), "{err}");
        assert!(!err.contains("panicked"), "{err}");
    }
}

/// An abstract value read before the program mints any decodes as one:
/// the wrapper is registered before the first value is read.
#[test]
fn an_abstract_value_reads_before_any_is_minted() {
    let program = "type Box = Abstract<i64>;\n\
        let t = sys::time::after_idle(duration:50.ms, null);\n\
        let r: Result<Box, [`PackErr(string), `InvalidCast(string)]> = \
        pack::read(t ~ bytes:GydaDDF+kktOYbgtbxPJpH0IgVt4u3Lm5Q8DQm94AAYAAAAAAAAABQ==);\n\
        sys::exit(r ~ 0);\n\
        select r { error as e => \"refused: [e]\", b => \"accepted: [b.0]\" }\n";
    let cache = tempfile::tempdir().expect("tempdir");
    let (out, err) = run(program, cache.path(), &[]);
    assert_eq!(out.trim(), "\"accepted: 5\"", "{err}");
}
