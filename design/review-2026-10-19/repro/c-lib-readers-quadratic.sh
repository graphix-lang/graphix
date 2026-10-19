#!/bin/sh
# Title: a variable read by N reader boundaries costs O(N^2) to register
# (GXRt::ref_var scans the variable's reader list linearly per registration).
# Command: sh design/review-2026-10-19/repro/c-lib-readers-quadratic.sh
# Expected: time roughly linear in N for both programs (the no-capture
#   control doubles per doubling of N).
# Observed (debug binary, --no-fusion, 2026-10-19):
#   capture n:  N=40000 1.40s  N=80000 4.27s  N=160000 13.9s
#   no capture: N=40000 0.63s  N=80000 1.25s  N=160000 2.76s
# Fusion on (no --no-fusion), callback `|x| once(x) + n` (does not fuse):
#   N=40000 2.17s  N=80000 7.52s; even `once(x) + 1` reads a variable that
#   every slot instance registers a reader for (403 readers at N=400).
# With GRAPHIX_DBG_VARS=1 at N=400 the captured `n` has 402 distinct readers,
# one per slot instance boundary.
BIN=${BIN:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for N in 40000 80000 160000; do
  for body in 'x + n' 'x + 1'; do
    cat > $D/p.gx <<GX
let n = 1;
let xs = array::init($N, |i| i);
let ys = array::map(xs, |x| $body);
sys::exit(sys::time::after_idle(duration:10.ms, ys ~ 0));
array::len(ys)
GX
    s=$(date +%s.%N)
    timeout -s KILL 120 $BIN --no-cache --no-fusion $D/p.gx >/dev/null
    e=$(date +%s.%N)
    echo "N=$N body='$body' $(echo "$e - $s" | bc)s"
  done
done
rm -rf $D
