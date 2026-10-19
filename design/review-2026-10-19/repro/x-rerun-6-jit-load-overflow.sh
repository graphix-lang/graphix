#!/usr/bin/env bash
# x-rerun-6 (variant of x-perf-compile-07): Generation::load
# (graphix-compiler/src/fusion/emit/jit.rs) recurses through a BodyRecord's
# callees with no stack::ensure_sufficient, so a chain of 4800 lambdas, each
# calling the one before, aborts the process at link time when it fuses.
# command: GRAPHIX=/path/to/graphix bash x-rerun-6-jit-load-overflow.sh
# expected: 2 (what --no-fusion prints, 3200 lambdas fuse and print 2)
# observed (HEAD 9445a1d21, debug build): after ~8 s "thread 'tokio-rt-worker'
#   has overflowed its stack / fatal runtime error: stack overflow, aborting",
#   rc 134; gdb (at 6400): ~14000 frames of Generation::load <- Generation::install's map
#   <- Jit::install <- Jit::link <- compile_script.
G=${GRAPHIX:-graphix}
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
n=${1:-4800}
{
  echo "let f0 = |x| x;"
  for i in $(seq 1 $((n - 1))); do echo "let f$i = |x| f$((i - 1))(x);"; done
  echo "let g = |x: i64| f$((n - 1))(x) + 1;"
  echo "sys::exit(sys::time::after_idle(duration:1.ms, 0));"
  echo "g(1)"
} > "$D/p.gx"
echo "== fused";     timeout -s KILL 170 "$G" --no-cache "$D/p.gx" 2>&1 | tail -2; echo "rc=${PIPESTATUS[0]}"
echo "== node-walk"; timeout -s KILL 170 "$G" --no-cache --no-fusion "$D/p.gx" 2>&1 | tail -2; echo "rc=${PIPESTATUS[0]}"
