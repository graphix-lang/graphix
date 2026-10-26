//! A recursion's dynamic scope must not grow with its depth: a deep
//! interpreted recursion under `--no-fusion` runs in a child process,
//! and its peak RSS grows linearly with the depth, at a bounded cost per
//! activation.

#![cfg(target_os = "linux")]

use std::{
    env, fs,
    process::{Command, Stdio},
};

/// The depths measured: a baseline, and two that a linear cost puts 15k
/// activations apart.
const DEPTHS: [i64; 3] = [1, 5_000, 20_000];
/// What an interpreted activation of a three-arm function may cost: about
/// 8.7KB measured, with a margin.
// CR claude for claude: [doc-drift/low] The "about 8.7KB measured" above is stale. At
// HEAD the dev-profile binary measures 11964 B per activation below 5000 deep and 11724
// B above, 97% of this 12288 B ceiling, so the next few hundred bytes added to an
// activation fail the plain gate. Re-measure, write the current figure, and choose the
// ceiling as part of x-fix-nodes-03's decision on activation cost, with a real margin.
// probe: design/review-2026-10-26/repro/tests-shell-compiler-activation-cost.py
// (tests-shell-compiler-04)
const MAX_BYTES_PER_ACTIVATION: i64 = 12 * 1024;

/// The peak RSS in KB of `f(depth)`, which must compute its sum.
fn peak_rss_kb(depth: i64) -> i64 {
    let expected = depth * (depth + 1) / 2;
    let program = format!(
        "let rec f = |n: i64| -> i64 select n {{\n  0 => 0,\n  _ => n + f(n - 1)\n}};\n\
         sys::exit(select f({depth}) {{ {expected} => 0, _ => 1 }})\n"
    );
    let path = env::temp_dir()
        .join(format!("recursion_memory_{}_{depth}.gx", std::process::id()));
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
        "the recursion did not compute f({depth}) = {expected} (status {status:#x})"
    );
    usage.ru_maxrss
}

#[test]
fn deep_recursion_memory_is_linear() {
    let [base, mid, deep] = DEPTHS.map(peak_rss_kb);
    eprintln!("peak RSS by depth {DEPTHS:?}: {base}KB {mid}KB {deep}KB");
    // the cost of an activation, below the midpoint and above it
    let low = (mid - base) * 1024 / (DEPTHS[1] - DEPTHS[0]);
    let high = (deep - mid) * 1024 / (DEPTHS[2] - DEPTHS[1]);
    assert!(
        high <= MAX_BYTES_PER_ACTIVATION,
        "an activation costs {high} bytes, over {MAX_BYTES_PER_ACTIVATION}"
    );
    // linear: a deeper activation costs what a shallow one does
    assert!(
        high <= 2 * low.max(1024) && low <= 2 * high.max(1024),
        "activation cost {low} bytes below {} deep, {high} bytes above",
        DEPTHS[1]
    );
}
