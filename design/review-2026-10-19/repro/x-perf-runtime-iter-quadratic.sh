#!/bin/bash
# x-perf-runtime: array::iter over n elements is quadratic in n
# The runtime's per-variable write FIFO (graphix-rt/src/rt.rs Waiting::pop)
# takes each cycle's write with SmallVec::remove(0), which shifts the whole
# queue; array::iter queues all n elements on one variable at once.
# Command: bash design/review-2026-10-19/repro/x-perf-runtime-iter-quadratic.sh
# Expected: cost per element roughly constant in n (iterq, which queues one
#   element at a time, stays at ~16 us per element at every n).
# Observed (debug build, GRAPHIX_PAR=off):
#   iter  n=5000  us_per=14.5   n=20000 us_per=29.1   n=80000 us_per=118.2 (9.5 s)
#   iterq n=5000  us_per=16.1   n=20000 us_per=15.9   n=80000 us_per=16.0  (1.3 s)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for kind in iter iterq; do
  for n in 5000 20000 80000; do
    if [ $kind = iter ]; then src="let x = array::iter(a);"; else src="let clk = 0;
let x = array::iterq(#clock: clk, a);
clk <- x;"; fi
    cat > $d/p.gx <<GX
let n = $n;
let t0 = sys::time::now(0);
let a = array::init(n, |i| i);
$src
let c = count(x);
let done = select c { p if p < n => never(), p => p };
let t1 = sys::time::now(done);
let el = cast<f64>(sys::time::diff(t1, t0))\$ * 1000.0;
println("$kind n=[n] ms=[el] us_per=[el * 1000.0 / cast<f64>(n)\$]");
sys::exit(el ~ 0)
GX
    GRAPHIX_PAR=off timeout -s KILL 120 $G --no-cache $d/p.gx
  done
done
rm -rf $d
