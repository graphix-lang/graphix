#!/bin/bash
# Title: union_identical (setops::Identity::same) walks a type DAG as a tree:
#   a pair DAG named through a typedef builds in exponential time
# Command: bash design/review-2026-10-26/repro/t-typ-mod-ref-dag-identity-exponential.sh
# Expected: build time roughly linear in n (the type is a DAG of n nodes).
# Observed (debug build, shared box): --check ~40 ms at every n; the run
#   n=18 307 ms, n=20 919 ms, n=22 3201 ms, n=24 12533 ms (x4 per 2 levels).
#   gdb samples: setops::Identity::same/all, thousands of frames deep, under
#   contains::identical_linked <- Type::check_contains <- CallSite::setup_static_bind
#   <- bind_instance <- resolve_static (typecheck1).
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
dir=$(mktemp -d)
for n in 18 20 22 24; do
  {
    echo "type R<'a, 'b> = ('a, 'b);"
    echo "let mk = |x: 'a| -> R<'a, 'a> (x, x);"
    echo "let f = |a| {"
    echo "  let x0 = a;"
    for i in $(seq 1 $n); do echo "  let x$i = mk(x$((i-1)));"; done
    echo "  x$n"
    echo "};"
    echo "let r = f(1);"
    echo "sys::exit(0)"
  } > $dir/r$n.gx
  s=$(date +%s%N)
  timeout -s KILL 170 $GRAPHIX --no-cache $dir/r$n.gx > /dev/null 2>&1
  rc=$?
  echo "n=$n rc=$rc $(( ($(date +%s%N) - s) / 1000000 )) ms"
done
rm -rf $dir
