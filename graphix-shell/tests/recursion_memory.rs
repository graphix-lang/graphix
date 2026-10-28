#![cfg(target_os = "linux")]

use compact_str::{CompactString, format_compact};
use std::{
    fs,
    process::{Command, Stdio},
};

const SIZES: [i64; 3] = [1, 5_000, 20_000];
const MAX_BYTES_PER_ACTIVATION: i64 = 10 * 1024;

fn peak_rss_kb(program: &str) -> i64 {
    let file = tempfile::Builder::new().suffix(".gx").tempfile().unwrap();
    fs::write(file.path(), program).unwrap();
    let child = Command::new(env!("CARGO_BIN_EXE_graphix"))
        .arg("--no-cache")
        .arg("--no-fusion")
        .arg("--no-netidx")
        .env("GRAPHIX_PAR", "off")
        .arg(file.path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .spawn()
        .unwrap();
    let pid = child.id() as libc::pid_t;
    let mut status = 0;
    let mut usage: libc::rusage = unsafe { std::mem::zeroed() };
    let reaped = unsafe { libc::wait4(pid, &mut status, 0, &mut usage) };
    assert_eq!(reaped, pid, "wait4 failed");
    assert!(
        libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
        "the program did not compute the expected result (status {status:#x}): {program}"
    );
    usage.ru_maxrss
}

fn assert_linear_cost(what: &str, ceiling: i64, program: impl Fn(i64) -> CompactString) {
    let [base, mid, deep] = SIZES.map(|n| peak_rss_kb(&program(n)));
    let low = (mid - base) * 1024 / (SIZES[1] - SIZES[0]);
    let high = (deep - mid) * 1024 / (SIZES[2] - SIZES[1]);
    eprintln!(
        "{what}: peak RSS at {SIZES:?}: {base}KB {mid}KB {deep}KB; {low}/{high} bytes per {what}"
    );
    assert!(high <= ceiling, "a {what} costs {high} bytes, over {ceiling}");
    assert!(
        high <= 2 * low.max(1024) && low <= 2 * high.max(1024),
        "{what} cost {low} bytes below {} deep, {high} bytes above",
        SIZES[1]
    );
}

#[test]
fn deep_recursion_memory_is_linear() {
    assert_linear_cost("activation", MAX_BYTES_PER_ACTIVATION, |depth| {
        let expected = depth * (depth + 1) / 2;
        format_compact!(
            "let rec f = |n: i64| -> i64 select n {{\n  0 => 0,\n  _ => n + f(n - 1)\n}};\n\
             sys::exit(select f({depth}) {{ {expected} => 0, _ => 1 }})\n"
        )
    });
}

const MAX_BYTES_PER_SLOT: i64 = 3_700;

#[test]
fn collection_slot_memory_is_linear() {
    assert_linear_cost("slot", MAX_BYTES_PER_SLOT, |n| {
        let last = n - 1;
        format_compact!(
            "let values = array::init({n}, |i| i);\n\
             sys::exit(select (array::len(values), values[0]$, values[-1]$) {{\n\
             ({n}, 0, {last}) => 0, _ => 1\n}})\n"
        )
    });
}
