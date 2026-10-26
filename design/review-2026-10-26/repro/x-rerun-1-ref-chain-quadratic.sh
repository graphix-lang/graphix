#!/bin/bash
# Title: a let chain of references (`let x1 = &x0; let x2 = &x1; ..`) still checks in
#   quadratic time (re-run of the 2026-10-19 x-stack-flat-chain-quadratic &X shape).
# Command: bash design/review-2026-10-26/repro/x-rerun-1-ref-chain-quadratic.sh
# Expected: time roughly linear in the number of lets, as `[X]`, `{a: X}`, `(X, 1)` and
#   `A(X) chains now are (0.16-0.26 s at 2500 and 5000 lets).
# Observed (debug build at e6ffc6eed, --check): &X 1250 0.2 s, 2500 0.46 s, 5000 1.51 s,
#   10000 5.64 s (x3.7 per doubling); GRAPHIX_PROFILE=1 puts 3.5 of 3.6 s at 8000 lets in
#   the Typecheck0 phase.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 2500 5000 10000; do
  { echo "let x0 = 1;"; for ((i = 1; i <= n; i++)); do echo "let x$i = &x$((i - 1));"; done; echo 1; } > $D/p.gx
  s=$(date +%s.%N); timeout -s KILL 120 $G --no-cache --check $D/p.gx > /dev/null; rc=$?
  echo "&X $n rc=$rc $(python3 -c "print(round($(date +%s.%N)-$s,2))") s"
done
rm -rf $D
