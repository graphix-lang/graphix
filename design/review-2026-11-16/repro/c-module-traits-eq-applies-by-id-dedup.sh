#!/bin/sh
# An `Eq` impl for ONE instantiation of an abstract type makes `array::dedup`
# quadratic over every OTHER instantiation of it.
#
# command:  sh design/review-2026-11-16/repro/c-module-traits-eq-applies-by-id-dedup.sh
# expected: both programs take about the same time at every size: no impl
#           applies to W<string>, so its values compare and hash by payload
# observed (debug build, HEAD e1be2fbf2):
#           plain  N=2000 0.10s   N=4000 0.10s   N=8000 0.09s
#           impl   N=2000 0.70s   N=4000 2.43s   N=8000 9.56s   (x4 a doubling;
#           N=30000 does not finish in 150 s)
# cause:    `coretraits::dispatch_eq_applies` answers by the abstract id alone
#           (`impls_on(..).next().is_some()`), so `GxAbstract::hash` takes the
#           `hash` hook's answer, which for W<string> is "no implementation"
#           (None): every W<string> hashes alike, while equality is the
#           payload's. `dispatch_cmp_applies` asks `impl_for` of the value's
#           own instantiation.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
prog() {
cat > "$D/$1.gx" <<GX
type W<'a> = Abstract<'a>;
$3
let xs = array::map(array::init($2, |i| i), |i| W("s[i]"));
let d = array::dedup(xs);
sys::exit(d ~ 0);
array::len(d)
GX
}
for n in 2000 4000 8000; do
  prog plain_$n $n ''
  prog impl_$n $n 'impl Eq for W<i64> { let eq = |a, b| a.0 == b.0; let hash = |a| a.0 };'
  for v in plain impl; do
    s=$(date +%s%N)
    timeout -s KILL 100 "$G" --no-cache "$D/${v}_$n.gx" >/dev/null 2>&1
    e=$(date +%s%N)
    echo "$v N=$n $(( (e - s) / 1000000 )) ms"
  done
done
rm -rf "$D"
