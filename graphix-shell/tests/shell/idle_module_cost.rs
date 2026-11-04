//! A cycle that walks none of a module's statements does no work for
//! them: a module's lets are as cheap to sit beside as the module
//! statement alone, with an interface or without one. A cycle that walks
//! them pays the walk alone: a let that re-raised nothing costs what an
//! expression statement does.

#[cfg(target_os = "linux")]
use super::common;

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

/// The instructions a self-loop counting to `cycles` inside a module of
/// `stmts` retires, with an interface or without one.
#[cfg(target_os = "linux")]
fn walked(stmts: &str, interface: bool, cycles: usize) -> u64 {
    let dir = tempfile::tempdir().expect("a dir for the program");
    let body = format!(
        "{stmts}let n = 0;\nn <- select n {{ k if k < {cycles} => k + 1, _ => never() }}\n"
    );
    std::fs::write(dir.path().join("a.gx"), body).expect("write a module");
    if interface {
        std::fs::write(dir.path().join("a.gxi"), "val n: i64\n")
            .expect("write a's interface");
    }
    let program = format!(
        "mod a;\nsys::exit(select a::n {{ {cycles} => a::n ~ 0, _ => never() }})\n"
    );
    let count = Instructions::count()
        .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
    let ran = graphix(
        dir.path(),
        &program,
        &["--no-cache", "--no-fusion"],
        &[("GRAPHIX_PAR", "off")],
    );
    let work = count.read();
    assert!(ran.status.success(), "{}: {}", ran.status, ran.stderr);
    work
}

#[test]
#[cfg(target_os = "linux")]
fn walked_module_lets_cost_what_expression_statements_do() {
    const STMTS: usize = 1500;
    const CYCLES: usize = 2000;
    let lets: String = (0..STMTS).map(|i| format!("let x{i} = {i};\n")).collect();
    let exprs: String = (0..STMTS).map(|i| format!("{i};\n")).collect();
    let per = |stmts: &str, interface| {
        let walk =
            walked(stmts, interface, CYCLES).saturating_sub(walked(stmts, interface, 1));
        walk as f64 / ((CYCLES - 1) * STMTS) as f64
    };
    let base = per(&exprs, false);
    for (what, stmts, interface) in [
        ("lets", &lets, false),
        ("lets with an interface", &lets, true),
        ("expressions with an interface", &exprs, true),
    ] {
        let cost = per(stmts, interface);
        eprintln!(
            "{what}: {cost:.1} per walked statement and cycle; expressions {base:.1}"
        );
        assert!(
            cost - base < 15.0,
            "{what}: {cost:.1} instructions per walked statement and cycle, expressions \
             without an interface {base:.1}"
        )
    }
}
