#!/bin/sh
# Title: in a seq inside a function, an unannotated function parameter passed to a call
#   is put in the issue snapshot tuple, so the call (here array::map) cannot resolve its
#   callback statically and node-walks; annotating the parameter's type fuses it.
# cmd: sh t-seq-fn-param-defuses.sh [path/to/graphix-fuzz]
# expected: the two programs fuse alike (they differ only by the annotation on `f`)
# observed: annotated: fused=4; unannotated: fused=2, with
#   "lambda call site `array::map(seqissued….1, seqissued….2)` not discovered — subtree node-walks"
# Cause: fn_names asks is_function(env, bind) at lowering time, before the check, when an
#   unannotated parameter's type is an unbound variable.
F=${1:-/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz}
D=$(mktemp -d)
cat > $D/annotated.gx <<'GX'
let apply = |f: fn(x: i64) -> i64, t| seq t {
    let y = array::map([1, 2, 3], f);
    y
};
let res = apply(|x| x + 1, 1);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
res
GX
sed 's/|f: fn(x: i64) -> i64, t|/|f, t|/' $D/annotated.gx > $D/unannotated.gx
for p in annotated unannotated; do
  echo "== $p"
  timeout -s KILL 120 $F run $D/$p.gx 2>&1 | grep -E "fusion:|array::map\(seqissued" | head -3
done
rm -rf $D
