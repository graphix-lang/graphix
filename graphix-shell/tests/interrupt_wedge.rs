//! Ctrl-C must always get the user their process back.
//!
//! Recursion evaluates ATOMICALLY within a cycle (the recursion ruling:
//! it fires like the hand-inlined chain), so a program can legally spin
//! forever inside one cycle and the engine does not bound it — the
//! no-wedge property the old one-eval-per-cycle model gave away for JIT
//! performance and predictable semantics. What replaces it is
//! CONTAINMENT, which lives outside the language: the cooperative
//! interrupt (`GXHandle::interrupt`, polled by the interp's tail driver
//! and at every emitted loop head). No program can observe it, because
//! nothing arms it except a human or an embedder.
//!
//! Until 2026-08-15 the shell never armed it, so a wedged program made
//! the PROCESS unkillable by Ctrl-C: the tokio runtime's shutdown waits
//! for the `block_in_place` section `do_cycle` runs in, and SIGKILL was
//! the only way out. These tests spawn the real binary on programs that
//! wedge, in both engines, and assert SIGINT still gets the shell to
//! exit.
#![cfg(unix)]

use std::{
    fs,
    process::{Child, Command, Stdio},
    thread,
    time::{Duration, Instant},
};

/// A pure infinite tail recursion: constant stack, bounded memory, never
/// errors (`+` wraps at i64::MAX). Fuses to a native rebind-and-jump
/// loop under the JIT and node-walks under `--no-fusion`, so the two
/// modes exercise the kernel's `emit_interrupt_check` and the interp
/// driver's own poll. This one wedges on the FIRST cycle — inside the
/// shell's env load, before the input loop exists — which is why the
/// shell arms its signal handler before that point.
const FIRST_CYCLE_WEDGE: &str = "{ let rec f = |v: i64| -> i64 f(v + i64:1); f(i64:0) }";

/// Wedges only after producing a few values, so the wedge lands while
/// the input loop is live (the other half of the shell's cancel path).
/// The aug14f `connect_in_call_arg` witness, reduced: `s` advances once
/// per the seed-applies-once rule, so `f(6)` tail-calls itself forever.
const LATER_CYCLE_WEDGE: &str = "{let x = array::iter([i64:1, i64:2, i64:3, i64:4]); \
     let m = x / i64:3; \
     let rec f = |n: i64| -> i64 select n {i64:0 => i64:0, \
     _ => f({let s = i64:0; s <- array::fold([i64:1, i64:2, i64:3], i64:0, |a, e| a + e); s})}; \
     f(m)}";

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

fn spawn(path: &std::path::Path, no_fusion: bool) -> Child {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_graphix"));
    if no_fusion {
        cmd.arg("--no-fusion");
    }
    // --no-netidx keeps the test off the network (NetConfig::Internal).
    cmd.arg("--no-netidx")
        .arg(path)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn graphix")
}

fn interrupt_frees_process(program: &str, no_fusion: bool, label: &str) {
    let dir = std::env::temp_dir().join(format!("gx-wedge-{}-{}", std::process::id(), label));
    fs::create_dir_all(&dir).expect("tempdir");
    let path = dir.join("wedge.gx");
    fs::write(&path, program).expect("write program");
    let mut child = spawn(&path, no_fusion);
    // Let it compile the stdlib and reach the wedge. If it has already
    // exited, the program isn't wedging and the test proves nothing.
    thread::sleep(Duration::from_secs(6));
    let alive = child.try_wait().expect("try_wait").is_none();
    assert!(alive, "{label}: program exited on its own — it is not a wedge, so this test is vacuous");
    // Two signals, mirroring a user: the first cancels the in-flight
    // computation, the second (if the loop re-entered) exits.
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
    interrupt_frees_process(FIRST_CYCLE_WEDGE, false, "first-cycle/jit");
}

#[test]
fn interrupt_frees_first_cycle_wedge_interp() {
    interrupt_frees_process(FIRST_CYCLE_WEDGE, true, "first-cycle/interp");
}

#[test]
fn interrupt_frees_later_cycle_wedge_jit() {
    interrupt_frees_process(LATER_CYCLE_WEDGE, false, "later-cycle/jit");
}

#[test]
fn interrupt_frees_later_cycle_wedge_interp() {
    interrupt_frees_process(LATER_CYCLE_WEDGE, true, "later-cycle/interp");
}
