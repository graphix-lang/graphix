#!/bin/sh
# A large array or map literal typechecks in quadratic time: element_union
# dedupes element types with a linear scan whose `same` never equates two
# primitive types (or two separately built structs), so every element is
# "distinct" and the scan is O(n^2).
# Command: sh x-perf-compile-literal-element-union.sh
# Expected: --check linear in the literal's length.
# Observed (debug, HEAD e6ffc6eed, --no-cache --check):
#   [0, 1, ..]   n=20000 0.30 s, 40000 0.78 s, 80000 2.50 s (phase Typecheck0
#                90 / 412 / 1822 ms; Parse 164 / 334 / 655 ms, linear)
#   {"k0" => 0, ..} n=20000 0.42 s, 40000 1.06 s, 80000 3.13 s
#   gdb: every sample in node::array::element_union's own frame.
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 20000 40000 80000; do
  python3 -c "import sys; n=int(sys.argv[1]); print('let a = [' + ', '.join(str(i) for i in range(n)) + '];\narray::len(a)')" $n > "$D/a.gx"
  python3 -c "import sys; n=int(sys.argv[1]); print('let m = {' + ', '.join(f'\"k{i}\" => {i}' for i in range(n)) + '};\nmap::len(m)')" $n > "$D/m.gx"
  for k in a m; do
    t0=$(date +%s%N); timeout -s KILL 170 "$GX" --no-cache --check "$D/$k.gx"; rc=$?; t1=$(date +%s%N)
    echo "$k n=$n rc=$rc $(( (t1 - t0) / 1000000 )) ms"
  done
done
rm -rf "$D"
