//! A cycle that walks none of a module's statements does no work for
//! them: a module's lets are as cheap to sit beside as the module
//! statement alone, with an interface or without one.

#[cfg(target_os = "linux")]
mod common;

#[cfg(target_os = "linux")]
use common::{Instructions, graphix};

/// A module of `lets` lets, `r` the last.
#[cfg(target_os = "linux")]
fn module(lets: usize) -> String {
    let mut s: String = (0..lets).map(|i| format!("let x{i} = {i};\n")).collect();
    s.push_str(&format!("let r = x0 + x{}\n", lets - 1));
    s
}

/// The instructions a script retires that counts to `cycles` beside the
/// modules `a` (with an interface) and `b` (without), `lets` lets each,
/// and reads both once it is done.
#[cfg(target_os = "linux")]
fn work(lets: usize, cycles: usize) -> u64 {
    let dir = tempfile::tempdir().expect("a dir for the program");
    for m in ["a", "b"] {
        std::fs::write(dir.path().join(format!("{m}.gx")), module(lets))
            .expect("write a module");
    }
    std::fs::write(dir.path().join("a.gxi"), "val r: i64\n")
        .expect("write a's interface");
    let program = format!(
        "mod a;
mod b;
let n = 0;
n <- select n {{ k if k < {cycles} => k + 1, _ => never() }};
let s = a::r + b::r + n;
sys::exit(select n {{ {cycles} => s ~ 0, _ => never() }})
"
    );
    let count = Instructions::count()
        .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
    let ran = graphix(dir.path(), &program, &["--no-cache", "--no-fusion"], &[]);
    let work = count.read();
    assert!(ran.status.success(), "{}: {}", ran.status, ran.stderr);
    work
}

#[test]
#[cfg(target_os = "linux")]
fn idle_module_statements_cost_a_cycle_nothing() {
    const LETS: usize = 1500;
    const CYCLES: usize = 3000;
    let idle = |lets| work(lets, CYCLES).saturating_sub(work(lets, 1));
    let (big, small) = (idle(LETS), idle(1));
    let per = big.saturating_sub(small) as f64 / (2 * LETS * CYCLES) as f64;
    eprintln!(
        "{CYCLES} cycles beside 2 x {LETS} lets: {big}; beside 2 x 1: {small}; \
         {per:.1} per statement and cycle"
    );
    assert!(per < 20.0, "{per:.1} instructions per idle statement and cycle")
}
