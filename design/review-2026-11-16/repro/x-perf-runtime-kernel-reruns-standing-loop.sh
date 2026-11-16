#!/bin/bash
# x-perf-runtime: a fused function re-runs a collection loop over a standing argument whenever another argument fires
# f(mode, a): `mode` fires every cycle, `a` (N elements) never changes after
# birth. The node-walk's MapQ rides its standing slots (O(1) per cycle);
# the fused kernel is a pure function of its inputs, so each run maps all
# N elements again.
# Command: bash design/review-2026-11-16/repro/x-perf-runtime-kernel-reruns-standing-loop.sh
# Expected: fused no slower than the node-walk.
# Observed (debug build, GRAPHIX_PAR=off), us per cycle:
#   --no-fusion: N=1000 5.8  N=8000 5.2  N=32000 5.5
#   fused:       N=1000 16.9 N=8000 88.7 N=32000 333.6
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for mode in --no-fusion --fusion; do
for n in 1000 8000 32000; do
cat > $d/p.gx <<GX
let k = 510;
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let a = array::init($n, |j| j);
let mode = select i { x if x >= 0 => \`A, _ => \`B };
let f = |m: [\`A, \`B], xs: Array<i64>| select m { \`A => array::len(array::map(xs, |x| x + 1)), \`B => 0 };
let n = f(mode, a);
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000000.0 / 500.0;
println("$mode N=$n us_per_cycle=[el] n=[n]");
sys::exit(el ~ 0)
GX
flag=$mode; [ "$mode" = --fusion ] && flag=
GRAPHIX_PAR=off timeout -s KILL 170 $G --no-cache $flag $d/p.gx | grep us_per
done
done
rm -rf $d
