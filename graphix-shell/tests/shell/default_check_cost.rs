//! A labeled default's check costs what its tree holds, however much of
//! its types share one node.

#![cfg(target_os = "linux")]

use super::common;
use common::{Instructions, graphix};

/// A default whose block doubles a tuple `depth` times: its type is
/// `depth` nodes deep and `2^depth` paths wide.
fn doubling(depth: usize) -> String {
    let mut lets = String::from("let a0 = (1, 2); ");
    for i in 1..=depth {
        lets.push_str(&format!("let a{i} = (a{j}, a{j}); ", j = i - 1));
    }
    format!(
        "let f = |#d = {{ {lets}let z = a{depth}; 7 }}, y: i64| d + y;\n\
         println(f(1))\n"
    )
}

/// The instructions `graphix --check` retires over `program`.
fn check_cost(program: &str) -> u64 {
    let dir = tempfile::tempdir().expect("tempdir");
    let work = Instructions::count()
        .expect("count instructions (kernel.perf_event_paranoid must be 2 or lower)");
    let ran = graphix(dir.path(), program, &["--no-cache", "--check"], &[]);
    let done = work.read();
    assert!(ran.status.success(), "{}\n{}", ran.stdout, ran.stderr);
    done
}

#[test]
fn a_default_of_shared_types_checks_in_linear_time() {
    const BILLION: f64 = 1e9;
    let small = check_cost(&doubling(2));
    let big = check_cost(&doubling(20));
    let extra = big.saturating_sub(small) as f64 / BILLION;
    eprintln!(
        "depth 2: {:.2}G, depth 20: {:.2}G",
        small as f64 / BILLION,
        big as f64 / BILLION
    );
    assert!(
        extra < 0.3,
        "twenty doubling lets in a default cost {extra:.2}G more than two"
    );
}
