#!/bin/bash
# x-image: one run where fusion is unavailable writes an unfused program entry that every
#          later run restores, so the program never fuses again until its sources change
#
# Run: bash x-image-unfused-entry-sticks.sh [path-to-graphix]
# Expected: runs 3 and 4 (fusion available again) run the program's kernels, as run 1
#   did: a restored entry is indistinguishable from a cold compile, which fuses.
# Observed (HEAD e6ffc6eed): run 2 (GRAPHIX_JIT_ARENA=1 makes Jit::new fail, "fusion
#   is unavailable") refuses the fused entry, compiles unfused and rewrites the entry
#   under the same key (the key has the requested flags, not whether fusion ran);
#   runs 3 and 4 restore it: 0 kernel invocations (run 1: 14). Any Jit::new failure
#   (arena reservation, ISA setup) does the same.
G=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
let clk = sys::time::timer(duration:10.ms, true);
let n = 0;
n <- clk ~ n + 1;
let g = |a: Array<i64>, k: i64| array::map(a, |x| x * k + 1);
let rec fact = |i: i64, acc: i64| -> i64 select i { 0 => acc, i => fact(i - 1, acc * i) };
sys::exit(select n { 6 => n, _ => never() } ~ 0);
(g([1, 2, 3, n], n), fact(n, 1))
GX
for e in "" "GRAPHIX_JIT_ARENA=1" "" ""; do
  env $e GRAPHIX_DBG_INVOKE=1 XDG_CACHE_HOME=$D/cache timeout -s KILL 30 $G $D/p.gx > /dev/null 2> $D/err
  echo "run [${e:-fusion available}]: kernel invocations $(grep -ci 'kernel' $D/err)"
done
rm -rf $D
