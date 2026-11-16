#!/bin/bash
# Title: x-rerun-1-03 variant: a let chain of writable references
#   (`let x1 = &mut x0; let x2 = &mut x1; ..`) still checks in quadratic time.
# Command: bash design/review-2026-11-16/repro/x-rerun-2-mut-ref-chain-quadratic.sh
# Expected: linear, as the `&x` chain (0.10 / 0.12 / 0.16 s at 2500 / 5000 /
#   10000 lets) and a flat `let r_i = &mut x_i` program (0.13 / 0.16 / 0.24 s) are.
# Observed (debug build, --check): &mut chain 0.57 / 1.98 / 8.01 s, x4 per
#   doubling; GRAPHIX_PROFILE=1 at 10000: BuildGraph 4945 ms, Typecheck0 2767 ms
#   (318 / 1218 / 4945 ms and 183 / 699 / 2767 ms across the three sizes).
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for n in 2500 5000 10000; do
  { echo "let x0 = 1;"; for ((i = 1; i <= n; i++)); do echo "let x$i = &mut x$((i - 1));"; done; echo 1; } > $D/p.gx
  s=$(date +%s.%N); timeout -s KILL 120 $G --no-cache --check $D/p.gx > /dev/null; rc=$?
  echo "&mut $n rc=$rc $(python3 -c "print(round($(date +%s.%N)-$s,2))") s"
done
rm -rf $D
