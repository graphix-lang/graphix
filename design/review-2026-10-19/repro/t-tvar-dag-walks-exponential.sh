#!/bin/bash
# Title: type walks have no memo for shared composites: --check time doubles per `let`
# Command: bash t-tvar-dag-walks-exponential.sh   (GRAPHIX defaults to the review binary)
# Expected: --check time roughly linear in n (the types are DAGs of n nodes).
# Observed (debug build, 16-core box shared with other probes):
#   n=18 0.5 s, n=20 1.8 s, n=22 7.6 s  (x4 per 2 lets);
#   without the call: n=18 0.18 s, n=20 0.52 s, n=22 1.9 s; top level (no def) n=24 0.06 s.
#   gdb samples of the n=22 check sit in Type::named_defs (DefTable::own_typedefs),
#   settle::position_cells (DefTable::settle_open), FnType::inner_quantifiers::go,
#   ContainsHist::closed and would_cycle_seen_inner (TVar::copy).
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
dir=$(mktemp -d)
for n in 16 18 20 22; do
  {
    echo "let f = |a| {"
    echo "  let x0 = a;"
    for i in $(seq 1 $n); do echo "  let x$i = (x$((i-1)), x$((i-1)));"; done
    echo "  x$n"
    echo "};"
    echo "let r = f(1);"
    echo "0"
  } > $dir/e$n.gx
  s=$(date +%s.%N)
  timeout -s KILL 170 $GRAPHIX --check $dir/e$n.gx > /dev/null 2>&1
  echo "n=$n rc=$? $(echo "$(date +%s.%N) - $s" | bc) s"
done
rm -rf $dir
