#!/bin/bash
# Title: uniq's same-allocation shortcut covers only a top-level array: an unchanged map,
#   or a fresh tuple around the same array, still compares element by element (x-perf-runtime-05 variant)
# Command: bash design/review-2026-10-26/repro/x-rerun-4-uniq-same-map.sh
# Expected: per-cycle cost independent of n, as for `uniq(i ~ big)` over an array
#   (array: n=1000 4.3 ms, n=100000 4.7 ms for 1000 cycles).
# Observed (debug build, --no-fusion, GRAPHIX_PAR=off, 1000 cycles):
#   map   n=1000 16 ms   n=100000 997 ms
#   tuple n=1000 7.6 ms  n=100000 326 ms
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
for kind in map tuple; do for n in 1000 100000; do
  if [ $kind = map ]; then init="cast<Map<i64, i64>>(array::init($n, |i| (i, i)))\$"; e="i ~ big";
  else init="array::init($n, |i| i)"; e="(i ~ big, 1)"; fi
cat > $d/p.gx <<GX
let k = 1010;
let big = $init;
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let u = count(uniq($e));
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000.0;
println("$kind n=$n ms=[el] u=[u]");
sys::exit(el ~ 0)
GX
GRAPHIX_PAR=off timeout -s KILL 60 $G --no-cache --no-fusion $d/p.gx 2>&1 | grep 'n='
done; done
rm -rf $d
