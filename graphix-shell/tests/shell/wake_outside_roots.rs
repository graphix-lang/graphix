//! What a wake recomputes is a fire to no reader, in its own root or
//! another (`design/bind_driven_bodies.md`): a reader in a root after the
//! woken arm's takes the new value in the wake's cycle, one in a root
//! before it the cycle after, and neither counts an event.

use super::common;
use common::repl;

/// Each REPL input is a root. Arm 0 of the select over `sel` sleeps from
/// clk 2 while `n` goes 1, 2, 3 and wakes at clk 6, re-seeding the formal
/// `x` its closure captured; `d` reads the closure in a root before the
/// select's and `late` in one after it. The clock starts once every input
/// is in.
const INPUT: &str = "\
let go: i64 = never();
let clk = array::iter(go ~ array::init(10, |i| i));
let n = 0;
n <- filter(clk, |c| c % 2 == 0) ~ (n + 1);
let sel = 0;
sel <- filter(clk, |c| c % 4 == 1) ~ (1 - sel);
let k = 0;
let hh: fn(y: i64) -> i64 = never();
let d = select k { 0 => hh(1), _ => -1 };
let f = count(d);
let mk = |x: i64| |y: i64| { let z = y; x };
let g = select sel { 0 => mk(n), _ => mk(n + 100) };
hh <- once(g);
let late = select k { 0 => hh(1), _ => -1 };
let fl = count(late);
println(clk ~ (clk, n, sel, d, f, late, fl));
go <- 1;
sys::exit(sys::time::after_idle(duration:300.ms, 0))
";

#[test]
fn a_wake_fires_nothing_in_the_roots_around_it() {
    for args in [&["--no-cache"][..], &["--no-cache", "--no-fusion"]] {
        let dir = tempfile::tempdir().expect("tempdir");
        let ran = repl(dir.path(), args, INPUT);
        let rows: Vec<&str> = ran.stdout.lines().filter(|l| l.starts_with('(')).collect();
        assert_eq!(
            rows,
            [
                "(0, 0, 0, 0, 1, 0, 1)",
                "(1, 1, 0, 0, 1, 1, 2)",
                "(2, 1, 1, 1, 2, 1, 2)",
                "(3, 2, 1, 1, 2, 1, 2)",
                "(4, 2, 1, 1, 2, 1, 2)",
                "(5, 3, 1, 1, 2, 1, 2)",
                "(6, 3, 0, 1, 2, 3, 2)",
                "(7, 4, 0, 3, 2, 4, 3)",
                "(8, 4, 0, 4, 3, 4, 3)",
                "(9, 5, 0, 4, 3, 5, 4)",
            ],
            "{args:?}: {}{}",
            ran.stdout,
            ran.stderr
        );
    }
}
