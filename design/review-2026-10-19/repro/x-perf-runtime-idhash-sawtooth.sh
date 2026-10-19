#!/bin/bash
# x-perf-runtime: per-cycle cost jumps up to 4.5x with tiny changes in program size
# A map over n+1 elements whose source changes every cycle (every slot reruns,
# so the work is linear in n). The cost per slot per cycle should be flat in n.
# It is a sawtooth: the BindId-keyed maps (store, by_ref, event overlay) use
# identity hashing, and their probe lengths depend on how the ids fall
# against the table capacity (see x-perf-runtime-idhash-bench.rs).
# Command: bash design/review-2026-10-19/repro/x-perf-runtime-idhash-sawtooth.sh
# Expected: ns/slot-cycle roughly constant (~750).
# Observed (debug build, --no-fusion, GRAPHIX_PAR=off, 400 cycles):
#   n=300 3610  n=342 3718  n=344 833  n=400 830  n=600 1006  n=800 1966  n=1000 769
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for n in 300 342 344 400 600 800 1000; do
cat > $d/p.gx <<GX
let k = 410;
let rest = array::init($n, |i| i);
let i = 0;
i <- select i { x if x < k => x + 1, _ => never() };
let src = array::concat([i], rest);
let m = array::map(src, |x| uniq(x) * 2);
let t0 = sys::time::now(select i { 10 => 0, _ => never() });
let done = select i { x if x == k => x, _ => never() };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000.0;
println("n=$n ms=[el] ns_per_slot_cycle=[el * 1000000.0 / 400.0 / $n.0] len=[array::len(m)]");
sys::exit(el ~ 0)
GX
GRAPHIX_PAR=off timeout -s KILL 60 $G --no-cache --no-fusion $d/p.gx
done
rm -rf $d
