#!/bin/sh
# Title: collection slots whose callback is a builtin are never charged to the
#   activation budget (re-run of c-collection-budget-builtin-slots.gx, unchanged).
# Command: sh design/review-2026-11-16/repro/x-rerun-3-builtin-slots-uncharged.sh
# Expected (design/atomic_recursion.md, book functions/recursion.md: the budget
#   counts live activations, "calls, collection slots"): both runs stop with the
#   budget error (300000 live slots, budget 1000).
# Observed: the lambda callback stops ("a runaway recursion exceeded the
#   runtime's budget"); the builtin callback prints 300000 and exits normally.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for cb in 'count' '|i| count(i)'; do
  printf 'let n = array::iter([300000]);\nlet a = array::init(n, %s);\nsys::exit(sys::time::after_idle(duration:100.ms, array::len(a)));\narray::len(a)\n' "$cb" > $D/p.gx
  echo "== callback $cb"
  GRAPHIX_ACTIVATION_BUDGET=1000 timeout -s KILL 120 $G --no-cache $D/p.gx 2>&1 | tail -1
done
rm -rf $D
