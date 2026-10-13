//! A recursion's dynamic scope must not grow with its depth: a deep
//! interpreted recursion under `--no-fusion` runs in a child process
//! with its peak RSS bounded.

#![cfg(target_os = "linux")]

use std::{
    env, fs,
    process::{Command, Stdio},
};

// CR claude for claude: [test-gap/low] One depth against an absolute bound cannot show
// linearity. Peak RSS is about 224MB at 20k, so a regression where each activation
// copies about one byte per enclosing activation adds about 200MB, stays under
// MAX_RSS_MB, and passes. Fix: run two depths (for example 5k and 20k, plus a depth-1
// baseline), and require the RSS growth ratio to stay near the depth ratio, or bound
// bytes per activation directly. (tests-shell-compiler-07)
const DEPTH: i64 = 20_000;
// CR claude for claude: [test-gap/medium] The bound only catches growth that is worse
// than linear. At DEPTH 20000 the child peaks near 223 MB (about 8.7 KB per node-walk
// activation over a 53 MB baseline), so an activation could grow 3-4x and still pass,
// and the recent activation shrink is not pinned. Tighten the bound to the measured
// value plus a margin (about 300 MB) so it pins bytes per activation. 9 KB per
// activation of a three-arm function is itself the next thing to shrink: naive fib(26)
// and fib(27) under --no-fusion pass a 6 GB cap, and the node-walk is the fallback for
// every recursion that does not fuse. Probe:
// design/review-2026-10-12/repro/x-unsafe-04.gx. (x-unsafe-04)
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
        .arg("--no-cache")
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
