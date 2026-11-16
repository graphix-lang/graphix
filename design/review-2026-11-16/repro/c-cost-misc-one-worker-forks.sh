#!/bin/sh
# Auto forks on a one-worker evaluation pool, where no two parts can run at once
#
# On a host (or container, or affinity mask) with one CPU, eval_threads() is 1
# and the runtime's pool has one worker. A fork from the runtime thread hands
# the whole fork to that worker (EvalPool::install) and parks; the parts run
# one after the other. Every fork is overhead: the install hop, branch views,
# the merge. ForkSite and LoopSite have no loss feedback, so they keep forking.
#
# Expected: GRAPHIX_PAR=auto (the default) runs no slower than GRAPHIX_PAR=off
#           on one CPU, and forks nothing.
# Observed (debug build, 2026-11-16): ~4000 forked kernel loops, and auto is
#           25-35% slower than off:
#   fused loops (p3):   off 0.29 s   auto 0.38 s
#   node-walk block (p2, --no-fusion, two folds of 400): off 1.37 s  auto 1.70 s
#   (GRAPHIX_EVAL_THREADS=1 without taskset: off 1.37 s, auto 1.85 s)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/p3.gx <<'GX'
let n = 0;
let f = |n: i64| array::fold(array::map(array::init(4000, |i| i), |x| x * n + 1), 0, |acc, x| acc + x);
let a = f(n);
n <- select n < 2000 { true => n + 1, false => never() };
sys::exit(select n { 2000 => 0, _ => never() });
a
GX
cat > $D/p2.gx <<'GX'
let n = 0;
let xs = array::init(400, |i| i);
let a = array::fold(xs, n, |acc, x| acc + x * 3);
let b = array::fold(xs, n, |acc, x| acc + x * 5);
n <- select n < 3000 { true => n + 1, false => never() };
sys::exit(select n { 3000 => 0, _ => never() });
a + b
GX
echo "forked kernel loops on one CPU:"
GRAPHIX_DBG_PAR=1 timeout -s KILL 60 taskset -c 0 $G --no-cache $D/p3.gx 2>&1 | grep -c 'PAR kernel loop'
for m in off auto off auto; do
  s=$(date +%s%N)
  GRAPHIX_PAR=$m timeout -s KILL 120 taskset -c 0 $G --no-cache $D/p3.gx >/dev/null 2>&1
  e=$(date +%s%N); echo "p3 $m $(( (e - s) / 1000000 )) ms"
done
for m in off auto off auto; do
  s=$(date +%s%N)
  GRAPHIX_PAR=$m timeout -s KILL 120 taskset -c 0 $G --no-cache --no-fusion $D/p2.gx >/dev/null 2>&1
  e=$(date +%s%N); echo "p2 $m $(( (e - s) / 1000000 )) ms"
done
rm -r $D
