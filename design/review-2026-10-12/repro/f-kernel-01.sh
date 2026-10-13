#!/bin/sh
# f-kernel-01: SelfBlock's image codec (fusion/kernel_abi.rs) writes each
# ActivationLayout inline, so the DAG of shared layouts a chain of recursive
# kernels builds is written as a tree: the program image grows 2^N (one
# self-call per function) or 3^N (two self-calls) in the chain length.
# Usage: f-kernel-01.sh /path/to/graphix
# Expected: image size linear in N (a non-recursive chain of the same
#   shape grows ~1.5 KB per link), warm start faster than cold.
# Observed (debug build, HEAD 7324207f), program entry bytes:
#   one self-call:  N=10 1617723, N=12 1670150, N=14 1860318, N=16 2616162
#                   (beyond the ~1574400-byte empty entry: 43K, 96K, 286K, 1.04M)
#   two self-calls: N=10 2.75 MB, N=12 11.8 MB, N=13 32 MB; warm start at
#                   N=13 about 1.0 s against 0.35 s cold (--no-cache), wall
#                   time including the 100 ms idle exit
#   control: the two-self-call N=12 program under --no-fusion writes 1.61 MB,
#   so the growth is the fused kernels' records (SelfBlock layouts).
G=${1:-graphix}
D=$(mktemp -d)
gen() { n=$1; rhs=$2; {
  echo "let rec f0 = |n: i64| -> i64 select n < 1 { true => 0, false => $(echo "$rhs" | sed s/F/f0/g) };"
  k=1
  while [ $k -lt $n ]; do
    echo "let rec f$k = |n: i64| -> i64 select n < 1 { true => f$((k-1))(n), false => $(echo "$rhs" | sed s/F/f$k/g) };"
    k=$((k+1))
  done
  echo "let x = 3;"
  echo "sys::exit(sys::time::after_idle(duration:100.ms, 0));"
  echo "f$((n-1))(x)"; } > $D/p.gx; }
run() { rm -rf $D/cache; mkdir $D/cache
  XDG_CACHE_HOME=$D/cache timeout -s KILL 170 $G $D/p.gx > /dev/null 2>&1
  echo "$1 N=$2 image bytes: $(find $D/cache -name '*.img' -printf '%s ')"; }
for n in 10 12 14 16; do gen $n 'F(n - 1) + 1'; run one $n; done
for n in 10 12 13; do gen $n 'F(n - 1) + F(n - 2)'; run two $n; done
rm -rf $D
