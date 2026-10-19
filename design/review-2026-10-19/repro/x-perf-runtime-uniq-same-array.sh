#!/bin/bash
# x-perf-runtime: uniq of an unchanged collection costs O(n) per fire
# `uniq(i ~ big)` re-delivers the same array (one allocation) every cycle;
# Uniq::update compares it with the last one through Value's PartialEq,
# which for an array compares element by element (netidx-value
# ValArray::eq has no same-allocation shortcut).
# Command: bash design/review-2026-10-19/repro/x-perf-runtime-uniq-same-array.sh
# Expected: per-cycle cost independent of n (the value is the same allocation).
# Observed (debug build, --no-fusion, GRAPHIX_PAR=off, 1000 cycles):
#   n=1000 11.5 ms   n=10000 37 ms   n=100000 338 ms  (0.33 ms a cycle)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for n in 1000 10000 100000; do
cat > $d/p.gx <<GX
let k = 1010;
let big = array::init($n, |i| i);
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let u = count(uniq(i ~ big));
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000.0;
println("n=$n ms=[el] u=[u]");
sys::exit(el ~ 0)
GX
GRAPHIX_PAR=off timeout -s KILL 60 $G --no-cache --no-fusion $d/p.gx
done
rm -rf $d
