#!/bin/bash
# x-perf-runtime: a fused Map loop allocates a (k, v) tuple per entry at every run (graphix_cmap_to_pairs)
# The same fold, one entry changing per cycle, over a Map<i64, i64> and
# over an Array<(i64, i64)> holding the same pairs. Both fuse; the Map
# run flattens the map into a fresh ValArray of fresh pair tuples first.
# Command: bash design/review-2026-11-16/repro/x-perf-runtime-map-pair-flatten.sh
# Expected: the Map fold within a small factor of the pair-array fold.
# Observed (debug build, fusion on, GRAPHIX_PAR=off), us per cycle:
#   Array<(i64, i64)>: N=1000 33  N=4000 123  N=16000 471
#   Map<i64, i64>:     N=1000 119 N=4000 474  N=16000 3172
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for n in 1000 4000 16000; do
cat > $d/a.gx <<GX
let k = 210;
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let a = array::init($n, |j| (j, j));
let p = &mut a[7];
*p <- i ~ (7, i);
let r = array::fold(a, 0, |acc, (k, v)| acc + v);
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000000.0 / 200.0;
println("array of pairs N=$n us_per_cycle=[el] r=[r]");
sys::exit(el ~ 0)
GX
cat > $d/m.gx <<GX
let k = 210;
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let init: Map<i64, i64> = {};
let m0 = array::fold(array::init($n, |j| j), init, |acc, x| map::insert(acc, x, x));
let m = never();
m <- m0;
m <- i ~ map::insert(m, 7, i);
let r = map::fold(m, 0, |acc, (k, v)| acc + v);
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000000.0 / 200.0;
println("map N=$n us_per_cycle=[el] r=[r]");
sys::exit(el ~ 0)
GX
GRAPHIX_PAR=off timeout -s KILL 170 $G --no-cache $d/a.gx | grep us_per
GRAPHIX_PAR=off timeout -s KILL 170 $G --no-cache $d/m.gx | grep us_per
done
rm -rf $d
