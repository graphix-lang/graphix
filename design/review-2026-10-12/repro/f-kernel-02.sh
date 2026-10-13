#!/bin/sh
# f-kernel-02: a chain of N non-tail recursive functions, each calling the
# next, writes a program image that doubles per link (SelfBlock::encode in
# fusion/kernel_abi.rs writes each ActivationLayout inline, so the DAG of
# shared layouts is written as a tree). Usage: f-kernel-02.sh /path/to/graphix
# Observed (debug build): N=14 0.29 MB, N=16 1.0 MB, N=18 4.0 MB of program
# entry beyond the empty program; with two self-calls per function
# (fib-like) N=15 gives a 274 MB image and a warm start 10x slower than cold.
G=${1:-graphix}
D=$(mktemp -d)
for n in 12 14 16 18; do
  {
    echo "let rec f0 = |n: i64| -> i64 select n < 1 { true => 0, false => f0(n - 1) + 1 };"
    k=1
    while [ $k -lt $n ]; do
      echo "let rec f$k = |n: i64| -> i64 select n < 1 { true => f$((k-1))(n), false => f$k(n - 1) + 1 };"
      k=$((k+1))
    done
    echo "let x = 3;"
    echo "sys::exit(sys::time::after_idle(duration:100.ms, 0));"
    echo "f$((n-1))(x)"
  } > $D/chain$n.gx
  rm -rf $D/cache; mkdir $D/cache
  XDG_CACHE_HOME=$D/cache timeout -s KILL 170 $G $D/chain$n.gx > /dev/null 2>&1
  echo "N=$n image bytes: $(find $D/cache -name '*.img' -printf '%s ')"
done
rm -rf $D
