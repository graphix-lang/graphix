#!/bin/bash
# Title: a pair DAG built through a generic constructor still elaborates in exponential time
# Command: bash design/review-2026-10-26/repro/t-typ-mod-generic-pair-dag-exponential.sh
# Expected: build time roughly linear in n, as for the inline-tuple shape that
#   graphix-shell/tests/deep_nesting.rs::a_generic_call_over_a_shared_type_dag_builds_in_linear_time pins.
# Observed (debug build, shared box): --check 40 ms at every n; the run (elaboration)
#   n=18 307 ms, n=20 919 ms, n=22 3201 ms, n=24 12533 ms (x4 per 2 levels);
#   tuple form `|x: 'a| -> ('a, 'a) (x, x)` n=24 10409 ms.
#   gdb samples sit in Type::collect_position_tvars (tvar.rs), under
#   FnType::collect_tvars / position_cells, from FnType::settle_terminal (drain_pending_settles).
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
dir=$(mktemp -d)
for n in 18 20 22 24; do
  {
    echo "let mk = |x: 'a| -> ('a, 'a) (x, x);"
    echo "let f = |a| {"
    echo "  let x0 = a;"
    for i in $(seq 1 $n); do echo "  let x$i = mk(x$((i-1)));"; done
    echo "  x$n"
    echo "};"
    echo "let r = f(1);"
    echo "sys::exit(0)"
  } > $dir/p$n.gx
  s=$(date +%s%N)
  timeout -s KILL 170 $GRAPHIX --no-cache $dir/p$n.gx > /dev/null 2>&1
  rc=$?
  echo "n=$n rc=$rc $(( ($(date +%s%N) - s) / 1000000 )) ms"
done
rm -rf $dir
