#!/bin/bash
# Title: by_ref keeps one entry per (root, boundary); each slot's reader of a
# captured variable is a distinct entry and GXRt::ref_var / unref_var scan the
# list linearly, so building (or deleting) N map slots that read one outer
# variable costs O(N^2).
# Command: bash design/review-2026-10-19/repro/x-prune-reader-scan-quadratic.sh
# Expected: both columns grow ~linearly with N (the callbacks differ only in
#   reading `y` vs a constant).
# Observed (debug build, --no-fusion, 2026-10-19):
#   N=32000  reads-y 1.12 s   const 0.57 s
#   N=64000  reads-y 3.13 s   const 1.10 s
#   N=128000 reads-y 12.2 s   const 2.53 s   (x3.9 per doubling vs x2.3)
B=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for n in 32000 64000 128000; do
  for k in y 1; do
    cat > $d/p.gx <<GX
let y = 1;
let xs = array::init($n, |i| i);
let r = array::map(xs, |x| x + $k);
sys::exit(sys::time::after_idle(duration:10.ms, array::len(r)));
array::len(r)
GX
    s=$(date +%s.%N)
    timeout -s KILL 170 $B --no-cache --no-fusion $d/p.gx > /dev/null
    e=$(date +%s.%N)
    echo "N=$n callback reads $k: $(echo "$e - $s" | bc) s"
  done
done
rm -rf $d
