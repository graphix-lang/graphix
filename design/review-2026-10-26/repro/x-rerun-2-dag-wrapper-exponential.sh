#!/bin/bash
# Title: t-tvar-02 variant: a call over a shared type DAG made from inside another
#   definition (`let g = |b| f(b)`) still elaborates in time doubling per level.
# Command: bash x-rerun-2-dag-wrapper-exponential.sh [graphix]
# Expected: roughly flat, as `let r = f(1)` alone is (~0.1 s at n=24, the pin
#   a_generic_call_over_a_shared_type_dag_builds_in_linear_time).
# Observed (debug review binary): n=18 0.17 s, 20 0.23 s, 22 0.52 s, 24 1.8 s, 25 3.4 s.
#   --check stays at 0.04 s; GRAPHIX_NO_SUBST=1 at n=24 takes 0.1 s, so the cost is the
#   instance-by-substitution path; GRAPHIX_PROFILE_INSTANCES puts it in g's instance's
#   elaboration_ns and in Typecheck1.
G=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 18 20 22 24 25; do
  { echo "let f = |a| {"; echo "  let x0 = a;"
    for i in $(seq 1 $n); do echo "  let x$i = (x$((i-1)), x$((i-1)));"; done
    echo "  x$n"; echo "};"
    echo "let g = |b| f(b);"; echo "let r = g(1);"
    echo "sys::exit(sys::time::after_idle(duration:10.ms, 0));"; echo "0"; } > $D/v$n.gx
  s=$(date +%s%N)
  timeout -s KILL 120 $G --no-cache $D/v$n.gx > /dev/null 2>&1
  echo "n=$n rc=$? $(( ($(date +%s%N)-s)/1000000 )) ms"
done
rm -rf $D
