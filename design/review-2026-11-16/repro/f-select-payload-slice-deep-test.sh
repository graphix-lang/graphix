#!/bin/bash
# f-select: a fused slice pattern inside a variant payload deep-tests the whole
# payload on every evaluation, so a tail loop over it is quadratic.
#
# emit_variant_cond reads every payload leaf with exact=false, and
# LeafAt::array then calls value_array(.., exact=false), whose test is
# graphix_inferred_type_matches over the payload's type: is_a walks every
# element of the array, per evaluation. The node-walk's shape test is shallow.
#
# Command: bash design/review-2026-11-16/repro/f-select-payload-slice-deep-test.sh
# Expected: the fused loop is linear in n, as the control (the payload bound
#           first, then matched as an array) is, and no slower than --no-fusion.
# Observed (debug build, this box): pattern fused 282 ms / 1352 / 4954 / 18876 ms
#           at n = 5k / 20k / 40k / 80k (x4 per doubling); --no-fusion 4400 ms
#           at 80k; the control fused ~220 ms at every n up to 80k.
B=/home/eric/tmp/review-2026-11-16/bin
D=$(mktemp -d)
run() { # name mode file
  s=$(date +%s%N); out=$(timeout -s KILL 170 $B/graphix --no-cache $2 $3 2>&1 | tail -1); e=$(date +%s%N)
  echo "$1 ${2:-fused}: out=$out ms=$(( (e - s) / 1000000 ))"
}
for n in 20000 40000 80000; do
  cat > $D/p.gx <<EOF
let rec f = |v: [\`A(Array<i64>), \`B], acc: i64| -> i64 select v {
  \`A([h, rest..]) => f(\`A(rest), acc + h),
  \`A([]) => acc,
  \`B => acc
};
let r = f(\`A(array::init($n, |i| i)), 0);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
EOF
  cat > $D/c.gx <<EOF
let rec f = |v: [\`A(Array<i64>), \`B], acc: i64| -> i64 select v {
  \`A(a) => select a { [h, rest..] => f(\`A(rest), acc + h), [] => acc },
  \`B => acc
};
let r = f(\`A(array::init($n, |i| i)), 0);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
EOF
  run "pattern n=$n" "" $D/p.gx
  run "control n=$n" "" $D/c.gx
done
run "pattern n=80000" --no-fusion $D/p.gx
rm -rf $D
