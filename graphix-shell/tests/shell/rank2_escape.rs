//! A REPL input holds a value to a written quantified function type as a
//! script does: a lambda that writes a binding outside it is refused.

use super::common;
use common::repl;
#[cfg(target_os = "linux")]
use common::{Instructions, graphix};

#[test]
fn a_repl_input_holds_a_rank2_annotation_generally() {
    let dir = tempfile::tempdir().expect("tempdir");
    let input = "{ let y = never(); let p: fn<'b: Any>(x: 'b) -> 'b = |x| { y <- x; x }; \
                 (p(1), p(\"s\"), y + 1) }\n\
                 sys::exit(3)\n";
    let ran = repl(dir.path(), &["--no-cache"], input);
    assert_eq!(ran.status.code(), Some(3), "{}\n{}", ran.stdout, ran.stderr);
    let all = format!("{}\n{}", ran.stdout, ran.stderr);
    assert!(all.contains("the value is not polymorphic in 'b"), "{all}");
}

/// A call passing `n` rank-2 formals each a lambda general in it.
#[cfg(target_os = "linux")]
fn many_rank2_formals(n: usize) -> String {
    let formals: String =
        (0..n).map(|i| format!("#k{i}: fn<'a: Any>(x: 'a) -> 'a, ")).collect();
    let args: String = (0..n).map(|i| format!("#k{i}: |x| x, ")).collect();
    format!("let f = |{formals}n: i64| n;\nprintln(f({args}1))\n")
}

#[cfg(target_os = "linux")]
#[test]
fn a_call_judges_its_signature_once() {
    const BILLION: f64 = 1e9;
    let cost = |n| {
        let dir = tempfile::tempdir().expect("tempdir");
        let work = Instructions::count()
            .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
        let ran =
            graphix(dir.path(), &many_rank2_formals(n), &["--no-cache", "--check"], &[]);
        let done = work.read();
        assert!(ran.status.success(), "{}\n{}", ran.stdout, ran.stderr);
        done
    };
    let (small, big) = (cost(8), cost(512));
    let extra = big.saturating_sub(small) as f64 / BILLION;
    eprintln!(
        "8 formals: {:.2}G, 512: {:.2}G",
        small as f64 / BILLION,
        big as f64 / BILLION
    );
    assert!(extra < 1.0, "512 rank-2 formals cost {extra:.2}G more than 8");
}
