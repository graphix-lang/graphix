#!/bin/sh
# rt: a variable's waiting writes pop with SmallVec::remove(0), so draining
# n queued writes to one variable costs O(n^2) (graphix-rt/src/rt.rs Waiting::pop).
# range(0, n) queues n set_vars on one variable in one cycle.
# Command: sh design/review-2026-10-19/repro/rt-waiting-fifo-quadratic.sh
# Expected: wall time linear in n (one cycle per element, constant work each).
# Observed (debug build at 9445a1d21):
#   10000 0.34 s, 20000 0.76 s, 40000 2.17 s, 80000 7.8 s, 160000 32.2 s
#   (each doubling of n costs ~4x).
BIN=${BIN:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for n in 10000 20000 40000 80000; do
  cat > $D/r.gx <<G
let r = range(0, $n)\$;
sys::exit(select r { $((n-1)) => 0, _ => never() })
G
  s=$(date +%s.%N)
  timeout -s KILL 170 $BIN --no-cache $D/r.gx >/dev/null
  e=$(date +%s.%N)
  echo "$n: $(echo "$e - $s" | bc) s"
done
rm -rf $D
