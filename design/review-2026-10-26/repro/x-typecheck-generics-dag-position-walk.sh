#!/bin/bash
# Title: elaborating a generic call chain over a shared type DAG is still
# exponential in the DAG's depth: FnType::for_each_part walks every
# position of the signature with Type::collect_position_tvars, which has
# no shared-node memo, and settle_terminal, inner_quantifiers and
# site_bounds (FnType::collect_tvars) all go through for_each_part.
# The pinned shape (deep_nesting.rs a_generic_call_over_a_shared_type_dag_
# builds_in_linear_time: f called directly) is linear; the same f reached
# through two generic callers (g, h) is not.
# Command: bash x-typecheck-generics-dag-position-walk.sh
# Expected: each depth runs in well under a second (the check alone takes
#   0.08 s at every depth).
# Observed (debug build, --no-fusion, 2026-10-26): depth 20 0.62 s,
#   22 1.93 s, 24 7.85 s (x4 per two levels); --check stays at 0.08 s.
#   gdb samples sit in collect_position_tvars under FnType::for_each_part,
#   called from settle_terminal, inner_quantifiers and site_bounds.
G=${GRAPHIX:-graphix}
for n in 20 22 24; do
  f=$(mktemp --suffix=.gx)
  {
    echo "let f = |a| {"
    echo "  let x0 = a;"
    for i in $(seq 1 $n); do echo "  let x$i = (x$((i-1)), x$((i-1)));"; done
    echo "  x$n"
    echo "};"
    echo "let g = |b| f(b);"
    echo "let h = |c| g(c);"
    echo "let r = h(1);"
    echo "let q = h(\"s\");"
    echo "sys::exit(0)"
  } > $f
  s=$(date +%s%N)
  timeout -s KILL 120 $G --no-cache --no-fusion $f
  e=$(date +%s%N)
  echo "depth $n: $(( (e - s) / 1000000 )) ms"
  rm -f $f
done
