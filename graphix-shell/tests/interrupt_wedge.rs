//! Ctrl-C must always get the user their process back: a program may
//! spin forever inside one cycle, and the cooperative interrupt
//! (`GXHandle::interrupt`) is what stops it. These tests spawn the real
//! binary on wedging programs, in both engines, and assert SIGINT
//! still exits the shell.
#![cfg(unix)]

use std::{
    fs,
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};

/// A pure infinite tail recursion that wedges on the FIRST cycle,
/// inside the shell's env load, before the input loop exists.
const FIRST_CYCLE_WEDGE: &str = "{ let rec f = |v: i64| -> i64 f(v + i64:1); f(i64:0) }";

/// Prints one value, then a timer moves `x` and the recursion spins
/// inside a later cycle, while the input loop is live.
const LATER_CYCLE_WEDGE: &str = "{ let x = i64:0; \
     x <- sys::time::timer(duration:1.s, false) ~ i64:1; \
     let rec f = |n: i64| -> i64 select n { i64:0 => i64:0, _ => f(n + i64:1) }; \
     f(x) }";

fn sigint(child: &Child) {
    unsafe { libc::kill(child.id() as libc::pid_t, libc::SIGINT) };
}

/// True iff `child` exited within `budget`.
fn exited_within(child: &mut Child, budget: Duration) -> bool {
    let deadline = Instant::now() + budget;
    while Instant::now() < deadline {
        if child.try_wait().expect("try_wait").is_some() {
            return true;
        }
        thread::sleep(Duration::from_millis(100));
    }
    false
}

fn spawn(path: &std::path::Path, out: fs::File, no_fusion: bool) -> Child {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
    cmd.arg("--no-cache");
    if no_fusion {
        cmd.arg("--no-fusion");
    }
    // --no-netidx keeps the test off the network (NetConfig::Internal).
    cmd.arg("--no-netidx")
        .arg(path)
        .stdin(Stdio::null())
        .stdout(out)
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn graphix")
}

/// Run `program` until `wedged_after`, check it is alive and printing
/// nothing more, then SIGINT it and require it to exit.
fn interrupt_frees_process(
    program: &str,
    no_fusion: bool,
    label: &str,
    wedged_after: Duration,
) {
    let dir =
        std::env::temp_dir().join(format!("gx-wedge-{}-{}", std::process::id(), label));
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("wedge.gx");
    fs::write(&path, program).expect("write program");
    let out = dir.join("out");
    let mut child = spawn(&path, fs::File::create(&out).expect("output file"), no_fusion);
    // If it has already exited it is not wedging and the test proves nothing.
    thread::sleep(wedged_after);
    let alive = child.try_wait().expect("try_wait").is_none();
    assert!(
        alive,
        "{label}: program exited on its own — it is not a wedge, so this test is vacuous"
    );
    let printed = fs::metadata(&out).expect("output").len();
    thread::sleep(Duration::from_millis(500));
    assert_eq!(
        fs::metadata(&out).expect("output").len(),
        printed,
        "{label}: the program is still printing, so it is not wedged"
    );
    // Two signals, as a user would: cancel, then exit.
    sigint(&child);
    thread::sleep(Duration::from_millis(750));
    if child.try_wait().expect("try_wait").is_none() {
        sigint(&child);
    }
    let freed = exited_within(&mut child, Duration::from_secs(30));
    if !freed {
        let _ = child.kill();
        let _ = child.wait();
    }
    let _ = fs::remove_dir_all(&dir);
    assert!(freed, "{label}: wedged shell survived SIGINT — only SIGKILL frees it");
}

#[test]
fn interrupt_frees_first_cycle_wedge_jit() {
    interrupt_frees_process(
        FIRST_CYCLE_WEDGE,
        false,
        "first-cycle/jit",
        Duration::from_secs(6),
    );
}

#[test]
fn interrupt_frees_first_cycle_wedge_interp() {
    interrupt_frees_process(
        FIRST_CYCLE_WEDGE,
        true,
        "first-cycle/interp",
        Duration::from_secs(6),
    );
}

#[test]
fn interrupt_frees_later_cycle_wedge_jit() {
    interrupt_frees_process(
        LATER_CYCLE_WEDGE,
        false,
        "later-cycle/jit",
        Duration::from_secs(2),
    );
}

#[test]
fn interrupt_frees_later_cycle_wedge_interp() {
    interrupt_frees_process(
        LATER_CYCLE_WEDGE,
        true,
        "later-cycle/interp",
        Duration::from_secs(2),
    );
}
