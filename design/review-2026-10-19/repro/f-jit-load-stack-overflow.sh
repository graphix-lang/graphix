#!/bin/bash
# Title: Generation::load recurses once per callee level: a 6000-deep lambda chain aborts the JIT link
# Command: bash design/review-2026-10-19/repro/f-jit-load-stack-overflow.sh [graphix binary]
# Expected: (12001, 2) / (12002, 3), as `--no-fusion` prints (the node-walk runs it fine).
# Observed (debug build at 9445a1d21): "thread 'tokio-rt-worker' has overflowed its stack /
# fatal runtime error: stack overflow, aborting"; under gdb every frame is
# Generation::load -> SmallVec::extend -> Map::try_fold -> Generation::load ...
# A chain of 2000 or 3500 runs (cold and warm image); 6000 aborts.
GX=${1:-graphix}
N=6000
P=$(mktemp --suffix=.gx)
{
  echo "let f0 = |x: i64| x + 1;"
  for ((i = 1; i < N; i++)); do echo "let f$i = |x: i64| f$((i - 1))(x) + $((i % 3 + 1));"; done
  echo "let g = |a: i64| {"
  echo "  let r = f$((N - 1))(a);"
  echo "  let c = count(a);"
  echo "  (r, c)"
  echo "};"
  echo "let n = 0;"
  echo "let t = sys::time::timer(duration:5.ms, 2);"
  echo "n <- t ~ n + 1;"
  echo "sys::exit(sys::time::after_idle(duration:300.ms, n ~ 0));"
  echo "g(n)"
} > "$P"
timeout -s KILL 170 "$GX" --no-cache "$P"
echo "exit $?"
rm -f "$P"
