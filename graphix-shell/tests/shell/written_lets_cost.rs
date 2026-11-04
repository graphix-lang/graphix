//! Finding the lets a block writes costs what the block holds, however
//! many bindings stand between a write and the let it names.

#![cfg(target_os = "linux")]

use super::common;
use common::{Instructions, graphix};

/// A function body of `n` lets and `n` writes to the let at `target`.
fn writes(n: usize, target: usize) -> String {
    let mut body = String::new();
    for i in 0..n {
        body.push_str(&format!("let x{i} = {i}; "));
    }
    for _ in 0..n {
        body.push_str(&format!("x{target} <- v; "));
    }
    format!("let g = |v: i64| {{ {body}x{target} }};\nprintln(g(1))\n")
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
fn a_write_past_many_bindings_costs_what_a_near_one_does() {
    const N: usize = 4000;
    const MILLION: f64 = 1e6;
    let near = check_cost(&writes(N, N - 1));
    let far = check_cost(&writes(N, 0));
    let extra = far.abs_diff(near) as f64 / MILLION;
    eprintln!("near {:.0}M, far {:.0}M", near as f64 / MILLION, far as f64 / MILLION);
    assert!(
        extra < 100.,
        "{N} writes past {N} bindings cost {extra:.0}M more than writes past none"
    );
}
