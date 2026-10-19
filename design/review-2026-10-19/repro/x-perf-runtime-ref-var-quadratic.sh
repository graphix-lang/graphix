#!/bin/bash
# x-perf-runtime: building n slots that read one outer variable is quadratic
# Each slot's instance body is its own pruning boundary, so every slot
# registers a distinct Reader of `k`; GXRt::ref_var finds an existing Reader
# by a linear scan of by_ref[k] (unref_var too), so n slots cost O(n^2).
# The control reads a constant instead of `k`: the difference is the cost
# of the registrations alone.
# Command: bash design/review-2026-10-19/repro/x-perf-runtime-ref-var-quadratic.sh
# Expected: the shared-read cost grows linearly with n (2x per doubling).
# Observed (debug build, default flags; ms to build and run the map once):
#   n=5000  shared 281  const 137   (+144)
#   n=10000 shared 747  const 289   (+458)
#   n=20000 shared 2472 const 716   (+1756)   difference x3.2 then x3.8
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for n in 5000 10000 20000; do
  for kind in shared const; do
    if [ $kind = shared ]; then t=k; else t=3; fi
    cat > $d/p.gx <<GX
let k = 3;
let t0 = sys::time::now(0);
let m = array::map(array::init($n, |i| i), |x| uniq(x) + $t + $t + $t + $t + $t + $t + $t + $t);
let t1 = sys::time::now(m);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000.0;
println("n=$n $kind ms=[el] len=[array::len(m)]");
sys::exit(el ~ 0)
GX
    timeout -s KILL 120 $G --no-cache $d/p.gx
  done
done
rm -rf $d
