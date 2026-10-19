#!/bin/sh
# Title: under the default GRAPHIX_PAR=auto, the SECOND update of a deep non-tail
# recursion (an argument fires through existing activations) grows ~8x per doubling
# of depth in the node-walk; the first run and GRAPHIX_PAR=off are linear.
# Command: sh design/review-2026-10-19/repro/tests-lib-b1-recursion-rerun-auto-cliff.sh
# Expected: the auto column close to the off column at every N.
# Observed (debug build, GRAPHIX_PAR_T fixed huge so nothing ever forks):
#   N=1000 off 0.20 s  auto 0.39 s
#   N=2000 off 0.28 s  auto 1.34 s
#   N=4000 off 0.42 s  auto 9.22 s
#   N=8000 off 0.79 s  auto >40 s (killed); force 2.5 s (force stops at MAX_FORK_DEPTH)
# The first run alone (no rerun) at N=4000 takes 0.54 s under auto.
BIN=${BIN:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for N in 1000 2000 4000; do
cat > $D/rr.gx <<GX
let rec f = |n: i64, s: string| -> i64 select n { 0 => 0, n => select str::contains(#part: "b[n]", s) { true => 2, false => 1 } + f(n - 1, s) };
let s = "aaaa";
let r = f($N, s);
s <- sys::time::timer(duration:10.ms, false) ~ str::concat(s, "b7");
sys::exit(skip(#n: 1, r) ~ 0);
r
GX
  for P in off auto; do
    s=$(date +%s.%N)
    GRAPHIX_PAR=$P GRAPHIX_PAR_T=1000000000000 timeout -s KILL 60 $BIN --no-cache --no-fusion $D/rr.gx >/dev/null
    e=$(date +%s.%N)
    echo "N=$N $P $(echo "$e - $s" | bc) s"
  done
done
rm -rf $D
