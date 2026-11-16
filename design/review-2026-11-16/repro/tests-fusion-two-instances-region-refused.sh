#!/bin/sh
# Title: a region holding two instances of one generic callback whose body
#   calls another generic lambda (lang/fusion.rs
#   callback_instances_call_their_own_kernels / _at_float_and_int) is
#   refused ("two instances in one region call this site's lambda apart",
#   fusion::build_region), so those fixtures never run the in-region
#   per-instance dispatch their comment describes; and #[native] on such a
#   region does not name the blocker.
# Command: sh tests-fusion-two-instances-region-refused.sh
# Expected (per the fixtures' comment): a fuses, printing
#   ([2, 8, 0], [4.5, 12.5, 0.0]); or, if refused, the #[native] message
#   names the collision.
# Observed (HEAD e1be2fbf2):
#   a: refused, "#[native] expression did not fully fuse to native code:
#        - the node-walk computes `#[native] (array::map(xs, g), array::map(ys, g))`"
#      (graphix-fuzz run on the i64/u64 fixture lists
#       "two instances in one region call this site's lambda apart")
#   b: each map under its own #[native] fuses: ([2, 8, 0], [4.5, 12.5, 0.0])
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/a.gx <<'EOF'
let h = |x| x + x;
let g = |x| h(x) * x;
let xs: Array<i64> = [1, 2, 0];
let ys: Array<f64> = [1.5, 2.5, 0.0];
#[native] (array::map(xs, g), array::map(ys, g))
EOF
cat > $D/b.gx <<'EOF'
let h = |x| x + x;
let g = |x| h(x) * x;
let xs: Array<i64> = [1, 2, 0];
let ys: Array<f64> = [1.5, 2.5, 0.0];
(#[native] array::map(xs, g), #[native] array::map(ys, g))
EOF
for f in a b; do
  echo "== $f"
  timeout -s KILL 10 $G --no-cache $D/$f.gx 2>&1 | head -8
done
rm -rf $D
