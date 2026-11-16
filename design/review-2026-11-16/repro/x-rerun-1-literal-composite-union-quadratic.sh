#!/bin/sh
# Title: an array literal of tuples or structs (and any map literal) still
#   checks in O(n^2): element_union's SameType keys a composite type by its
#   Arc address, so separately built {name, v} element types never dedupe
#   (x-perf-compile-08, fixed for primitives only).
# Command: sh design/review-2026-11-16/repro/x-rerun-1-literal-composite-union-quadratic.sh
# Expected: --check linear in n, as `[0, 1, ..]` now is (138/191/258 ms at 20k/40k/80k).
# Observed (debug binary, --no-cache --check, shared box):
#   [{name: "k0", v: 0}, ..]  n=2500 111 ms, 5000 184, 10000 389, 20000 1193 (Typecheck0 981 ms)
#   [("k0", 0), ..]           n=20000 1165 ms, 40000 4249, 80000 16117
#   {"k0" => 0, ..}           n=20000 258 ms, 40000 795, 80000 2298 (keys from one variable
#                             or values from one variable: the same)
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for n in 5000 10000 20000; do
  python3 -c "import sys; n=int(sys.argv[1]); print('let a = [' + ', '.join(f'{{name: \"k{i}\", v: {i}}}' for i in range(n)) + '];\narray::len(a)')" $n > $D/s.gx
  python3 -c "import sys; n=int(sys.argv[1]); print('let a = [' + ', '.join(str(i) for i in range(n)) + '];\narray::len(a)')" $n > $D/p.gx
  for k in s p; do
    t0=$(date +%s%N); timeout -s KILL 120 $G --no-cache --check $D/$k.gx; t1=$(date +%s%N)
    echo "$k n=$n $(( (t1 - t0) / 1000000 )) ms"
  done
done
rm -rf $D
