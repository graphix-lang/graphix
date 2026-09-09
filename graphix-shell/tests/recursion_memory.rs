//! A recursion's dynamic scope must not grow with its depth: a deep
//! interpreted recursion under `--no-fusion` runs in a child process
//! with its peak RSS bounded.

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
