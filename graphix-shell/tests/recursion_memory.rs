//! A recursion's dynamic scope must not grow with its depth.
//!
//! `Scope.dynamic` is the chain of error handlers visible at a point,
//! and an instantiated body starts from its call site's — so a
//! representation that re-spelled the whole chain per activation made
//! every level cost O(depth): 2GB for 20,000 activations of a body that
//! installs no handler (2026-08-25). This runs such a recursion under
//! `--no-fusion` (the JIT never instantiates activations) in a child
//! process and bounds its peak RSS at a level the per-activation
//! constant fits with room and the quadratic cannot.
//!
//! 20,000 interpreted activations take ~10s in a dev build and peak at
//! ~420MB (release: ~1s, ~240MB); the string representation peaked past
//! 2GB in either.

#![cfg(target_os = "linux")]

use std::{
    env, fs,
    process::{Command, Stdio},
};

const DEPTH: i64 = 20_000;
const MAX_RSS_MB: i64 = 800;

#[test]
fn deep_recursion_memory_is_linear() {
    let expected = DEPTH * (DEPTH + 1) / 2;
    let program = format!(
        "let rec f = |n: i64| -> i64 select n {{\n  0 => 0,\n  _ => n + f(n - 1)\n}};\n\
         sys::exit(select f({DEPTH}) {{ {expected} => 0, _ => 1 }})\n"
    );
    let path =
        env::temp_dir().join(format!("recursion_memory_{}.gx", std::process::id()));
    fs::write(&path, program).unwrap();
    let child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-fusion")
        .arg(&path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let pid = child.id() as libc::pid_t;
    let mut status = 0;
    let mut usage: libc::rusage = unsafe { std::mem::zeroed() };
    let reaped = unsafe { libc::wait4(pid, &mut status, 0, &mut usage) };
    let _ = fs::remove_file(&path);
    assert_eq!(reaped, pid, "wait4 failed");
    assert!(
        libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
        "the recursion did not compute f({DEPTH}) = {expected} (status {status:#x})"
    );
    let rss_mb = usage.ru_maxrss / 1024;
    eprintln!("{DEPTH} interpreted activations peaked at {rss_mb}MB");
    assert!(
        rss_mb < MAX_RSS_MB,
        "{DEPTH} interpreted activations peaked at {rss_mb}MB (bound {MAX_RSS_MB}MB): \
         something per activation grows with the depth"
    );
}
