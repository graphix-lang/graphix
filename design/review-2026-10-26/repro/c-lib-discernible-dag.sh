#!/bin/sh
# c-lib: a Discernible/Ordered judgment walks a shared type as a tree (2^depth)
# Command: sh design/review-2026-10-26/repro/c-lib-discernible-dag.sh
# Builds `let x0 = 1; let x1 = (x0, x0); ... let xN = (x(N-1), x(N-1))` and
# checks `xN == xN` (PendingSettle::Discernible -> lib.rs discernible ->
# Type::indiscernible -> cast.rs rep_ambiguity) with `graphix --check`.
# Expected: check time flat in N, as for `[xN, xN]` (contains memoizes shared
# nodes) and for Type::holds since f7686465d.
# Observed (debug binary, 2026-10-26): N=22 0.26 s, N=24 0.92 s, N=26 3.4 s
# (x4 per +2 levels); `uniq(xN)` and `{xN => 1}` the same; `[xN, xN]` 0.03 s
# at every N. `select xN { v => 1 }` is exponential too (0.23 s / 0.80 s at
# 22 / 24) through a walk in the select's check.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
for n in 20 22 24 26; do
  f=$d/dag$n.gx
  echo "let x0 = 1;" > $f
  i=1; while [ $i -le $n ]; do echo "let x$i = (x$((i-1)), x$((i-1)));" >> $f; i=$((i+1)); done
  echo "x$n == x$n" >> $f
  s=$(date +%s%N); timeout -s KILL 120 $G --check $f >/dev/null 2>&1; e=$(date +%s%N)
  echo "N=$n $(( (e - s) / 1000000 )) ms"
done
rm -rf $d
