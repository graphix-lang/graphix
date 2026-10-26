#!/bin/sh
# A chain of array-accumulator folds in one function aborts the process: the
# fused kernel's frame outgrows the runtime thread's stack.
# Command: sh x-perf-compile-fold-chain-stack-overflow.sh 800   (and 400, --no-fusion)
# Expected: prints [1, 2, 3] (as --no-fusion does at n=800 in 0.36 s).
# Observed (debug, HEAD e6ffc6eed): n=400 first value 0.97 s; n=700 and n=800:
#   "thread 'tokio-rt-worker' has overflowed its stack / fatal runtime error:
#   stack overflow, aborting" (core dumped); gdb puts the fault in JIT code at
#   the first kernel's prologue (inline stack probe), which reserves
#   0x2b1f90 = 2.83 MB. The kernel frame (rbp - rsp at its first
#   graphix_fastcall) is 226 KB at n=200 and 771 KB at n=400: quadratic.
# The scalar chain (`array::fold(a, a{i-1}, |acc, x| acc + x)`) runs at 1600.
N=${1:-800}
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
F=$(mktemp --suffix=.gx)
{
  echo "let f = |a: Array<i64>| {"
  echo "  let a0 = a;"
  i=1
  while [ $i -lt $N ]; do
    echo "  let a$i = array::fold(a$((i-1)), [], |acc, x| array::push(acc, x));"
    i=$((i+1))
  done
  echo "  a$((N-1))"
  echo "};"
  echo "f([1, 2, 3])"
} > "$F"
timeout -s KILL 170 "$GX" --no-cache $2 "$F"
