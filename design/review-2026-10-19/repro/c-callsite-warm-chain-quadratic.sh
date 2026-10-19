#!/bin/sh
# A warm image start of a chain of N statically bound functions is slower
# than compiling it, and quadratic in N. Every Callee::Imaged site flattens
# its whole subtree's reads (ImagedRefs::flat, per site) and registers them
# at decode; each materialize releases them and the decoded body's imaged
# site registers its own (one shorter) subtree again: sum = O(N^2).
# Command: sh c-callsite-warm-chain-quadratic.sh [N]   (default 2000)
# Expected: warm start at most the cold time (the image exists to skip the
#   compile; commit 966be26bf measured a 4000-chain warm first cycle 75 ms).
# Observed (debug build, --no-fusion): N=1000 cold 0.45 s / warm 0.81 s;
#   N=2000 cold 0.85 s / warm 3.36 s; N=4000 cold 2.66 s / warm 14.7 s.
#   A variant that defines the chain but never calls f0 starts warm in 0.12 s,
#   so the cost is in materializing the nested imaged sites.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
N=${1:-2000}
D=$(mktemp -d)
F=$D/chain.gx
i=0; while [ $i -lt $N ]; do echo "let v$i = i64:$i;" >> $F; i=$((i+1)); done
echo "let f$N = |x: i64| -> i64 x;" >> $F
i=$((N-1)); while [ $i -ge 0 ]; do echo "let f$i = |x: i64| -> i64 f$((i+1))(x) + v$i;" >> $F; i=$((i-1)); done
echo 'println("[f0(i64:1)]");' >> $F
echo 'sys::exit(sys::time::after_idle(duration:1.ms, 0))' >> $F
for run in cold warm; do
  s=$(date +%s.%N)
  XDG_CACHE_HOME=$D/cache timeout -s KILL 170 $G --no-fusion $F > $D/out 2>&1
  e=$(date +%s.%N)
  echo "$run: $(echo "$e - $s" | bc) s, prints $(head -c 20 $D/out)"
done
rm -rf $D
