#!/bin/bash
# x-perf-runtime: a node-walked recursion whose depth alternates rebuilds its activations every cycle
# fib's argument alternates 14, 13, 14, ...: a depth not reached this cycle
# is deleted (shrink = delete) and rebuilt next cycle as a fresh run-time
# bind (compile, instance typing, analysis), so each cycle costs ~11x the
# cycle that re-runs the same depth.
# Command: bash design/review-2026-10-26/repro/x-perf-runtime-depth-churn.sh
# Expected: alternating 14/13 costs about what fixed 14 costs (the update
#   of the activations reached).
# Observed (debug build, --no-fusion, GRAPHIX_PAR=off), ms per 100 cycles:
#   fixed fib(14): 80     alternating fib(14 - i % 2): 860-900
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
for arg in "14 + i * 0" "14 - i % 2"; do
cat > $d/p.gx <<GX
let k = 400;
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let rec fib = |x| select x { 0 => 0, 1 => 1, x => fib(x - 1) + fib(x - 2) };
let r = fib($arg);
let t2 = sys::time::now(select i { x if x == k - 100 => x, _ => never() });
let t3 = sys::time::now(select i { x if x == k => x, _ => never() });
let last = cast<f64>(sys::time::diff(t3, t2))\$ * 1000.0;
println("fib($arg): ms_per_100_cycles=[last]");
sys::exit(last ~ 0)
GX
GRAPHIX_PAR=off timeout -s KILL 120 $G --no-cache --no-fusion $d/p.gx
done
rm -rf $d
