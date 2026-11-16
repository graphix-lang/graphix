#!/bin/zsh
# Title: a fire N readers take late costs O(N^2) a cycle (GX::deliver_late, node::late_fire)
#
# Command: zsh design/review-2026-11-16/repro/x-prune-late-readers-quadratic.sh
#   (GRAPHIX=/path/to/graphix overrides the binary; debug build, --no-fusion, GRAPHIX_PAR=off)
#
# The program maps N slots through a function value written after the map
# whose closure captures `k`, a binding made later in the text that fires
# every cycle. Each slot's instance reads `k` after the walk passed it, so
# each fire of `k` is delivered late to N readers of one variable.
#
# Expected: the 18 extra cycles cost time linear in N, as the forward
# twin does (the same map over a `k` bound above it: 80 / 198 / 614 ms
# at N = 8000 / 16000 / 32000).
# Observed (review box, debug build): the 18 extra cycles cost
#   N=8000: 1.4 s   N=16000: 5.4 s   N=32000: 21.3 s
# i.e. 4x for each doubling: `deliver_late` scans a variable's late fires
# once per reader to dedup, and `late_fire` scans them once per read.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
mk() { # N cycles back|fwd
  local k='let k = n * 2;'
  {
    print "let n = 0;"
    print "n <- select n { k if k < $2 => k + 1, _ => never() };"
    print "let xs = array::init($1, |i| i);"
    [[ $3 == fwd ]] && print $k
    print "let f: fn(x: i64) -> i64 = never();"
    print "let ys = array::map(xs, |x| f(x));"
    [[ $3 == back ]] && print $k
    print "f <- once(|x: i64| x + k);"
    print "sys::exit(sys::time::after_idle(duration:50.ms, select n { $2 => 0, _ => never() }));"
    print "array::len(ys)"
  } > $D/p_$3_$1_$2.gx
}
for dir in fwd back; do
  for N in 8000 16000 32000; do
    typeset -A ms
    for C in 2 20; do
      mk $N $C $dir
      s=$(date +%s%N)
      GRAPHIX_PAR=off timeout -s KILL 170 $G --no-cache --no-fusion $D/p_${dir}_${N}_$C.gx > /dev/null 2>&1
      ms[$C]=$(( ($(date +%s%N) - s) / 1000000 ))
    done
    print "$dir N=$N: 2 cycles ${ms[2]} ms, 20 cycles ${ms[20]} ms, 18 extra cycles $(( ms[20] - ms[2] )) ms"
  done
done
rm -rf $D
