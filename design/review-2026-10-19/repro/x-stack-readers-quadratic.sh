#!/bin/bash
# Registering a variable's readers is quadratic in how many boundaries read it.
# GXRt::ref_var (graphix-rt/src/rt.rs) scans the variable's reader list linearly, and every
# collection slot and every recursive activation is a boundary of its own, so a callback that
# reads an outer variable over N slots costs O(N^2) to build (and unref_var the same to delete).
# run: bash x-stack-readers-quadratic.sh
# expected: the outer `k` costs about what `+ 1` costs (linear in N)
# observed (debug build, --no-fusion, wall incl. ~0.8 s startup+exit):
#   with k:    25k 1.1 s, 50k 4.2 s, 100k 10.3 s, 200k 29.2 s
#   without k: 100k 2.4 s
#   default mode, callback |x| uniq(x + k) (de-fuses): 50k 3.5 s, 100k 12.2 s (6.6 s without k)
# gdb samples land in GXRt::ref_var -> BoundaryRef::eq under ExecCtx::apply_deferred.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for n in 25000 50000 100000; do
  printf 'let k = 1;\nlet xs = array::init(%s, |i| i);\nsys::exit(sys::time::after_idle(duration:300.ms, 0));\narray::len(array::map(xs, |x| x + k))\n' $n > $D/k.gx
  printf 'let xs = array::init(%s, |i| i);\nsys::exit(sys::time::after_idle(duration:300.ms, 0));\narray::len(array::map(xs, |x| x + 1))\n' $n > $D/nok.gx
  for f in k nok; do
    s=$(date +%s.%N); timeout -s KILL 150 $G --no-cache --no-fusion $D/$f.gx > /dev/null
    echo "$n $f $(echo "$(date +%s.%N) - $s" | bc) s"
  done
done
rm -rf $D
