#!/bin/bash
# Title: the check of nested slice arms over an unannotated parameter grows about
# as arms^4.5, where the written parameter's check is near flat.
# Command: bash design/review-2026-11-16/repro/tests-types-c-nested-slice-ladder-cost.sh
# The ladder is slice_arms(n, "[[{names}, t..], u..] => t", "_ => []") of
# lang::types::slice_arms_over_an_unannotated_parameter_copy_no_chain_of_cells.
# Expected: cost near the annotated twin's (|x: Array<Array<'e>>|).
# Observed (HEAD e1be2fbf2, debug binary, ~72 ms process baseline):
#   n=24  unannotated ~210 ms   annotated ~80 ms
#   n=32  unannotated ~480 ms   annotated ~105 ms
#   n=48  unannotated ~2.5 s
#   n=64  unannotated ~9.5 s    annotated ~130 ms
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
gen() { echo "let f = |x$2| select x {"; for k in $(seq $1 -1 1); do
  names=$(seq -s ', ' 0 $((k-1)) | sed 's/\([0-9][0-9]*\)/a\1/g'); echo "[[$names, t..], u..] => t,"; done
  echo "_ => []"; echo "};"; echo "f"; }
for n in 24 32 48 64; do
  for ann in "" ": Array<Array<'e>>"; do
    [ $n = 48 ] && [ -n "$ann" ] && continue
    gen $n "$ann" > $d/p.gx
    s=$(date +%s%N); timeout -s KILL 120 $G --check --no-cache $d/p.gx > /dev/null 2>&1; rc=$?; e=$(date +%s%N)
    echo "n=$n param='x$ann' rc=$rc $(( (e - s) / 1000000 )) ms"
  done
done
rm -rf $d
