#!/bin/sh
# Title: comparing or uniq-ing a value whose type is a shared pair DAG checks
#        in time exponential in its depth (Type::require_compared and
#        Type::indiscernible walk each path, no memo of shared nodes)
# Command: sh t-tvar-compared-dag-exponential.sh
# Expected: --check time linear in D, as for the bare value (`xD`: ~40 ms at
#   any depth).
# Observed (debug binary, --check): `xD == xD` 95 / 246 / 891 ms and
#   `uniq(xD)` 139 / 460 / 1713 ms at D = 20 / 22 / 24 (x2 per level);
#   `array::sort([xD])` like uniq.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
dir=$(mktemp -d)
for D in 20 22 24; do
  for E in "x$D" "x$D == x$D" "uniq(x$D)"; do
    {
      echo "let x0 = 1;"
      i=1
      while [ $i -le $D ]; do echo "let x$i = (x$((i-1)), x$((i-1)));"; i=$((i+1)); done
      echo "$E"
    } > "$dir/dag.gx"
    s=$(date +%s%N)
    timeout -s KILL 120 "$G" --check "$dir/dag.gx" > /dev/null 2>&1
    echo "D=$D [$E] rc=$? $(( ($(date +%s%N) - s) / 1000000 )) ms"
  done
done
rm -rf "$dir"
