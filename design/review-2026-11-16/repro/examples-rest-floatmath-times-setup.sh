#!/usr/bin/env bash
# fold_floatmath times its input array's construction, not just the fold.
#
# bench/fold_floatmath.gx starts the clock with `sys::time::now(0)` and
# builds `xs = array::init(n, |i| cast<f64>(i)$)` after it, so the timed
# region holds the build. The variant below builds xs first and starts the
# clock with `now(xs)` (bench/README.md's own rule: start the clock after
# the setup, by dataflow), leaving only the float fold in the timed region.
#
# Run: bash design/review-2026-11-16/repro/examples-rest-floatmath-times-setup.sh [graphix]
# Expected: both report the fold alone, about the same elapsed_s.
# Observed (debug binary at e1be2fbf2, GRAPHIX_PAR=off):
#   bench   elapsed_s=0.0016   fixed elapsed_s=0.00038   (JIT: ~75% of the bench is the build)
#   with --no-fusion: bench 2.57 s, fixed 1.54 s
set -u
B=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
here="$(cd "$(dirname "$0")" && pwd)"
root="$here/../../.."
tmp=$(mktemp -d)
cat > "$tmp/fixed.gx" <<'EOF'
let n = 100000;
let xs = array::init(n, |i| cast<f64>(i)$);
let t0 = sys::time::now(xs);
let seed = cast<f64>(t0)$;
let result = #[native] array::fold(xs, seed, |a, x| a + x * x - x / 2.0 + x);
let t1 = sys::time::now(result);
let elapsed = cast<f64>(t1)$ - cast<f64>(t0)$;
println("fold_floatmath(fixed) n=[n] result=[result] elapsed_s=[elapsed]");
sys::exit(elapsed ~ 0)
EOF
for i in 1 2 3; do
    GRAPHIX_PAR=off timeout -s KILL 60 "$B" --no-cache "$root/bench/fold_floatmath.gx" | tail -1
    GRAPHIX_PAR=off timeout -s KILL 60 "$B" --no-cache "$tmp/fixed.gx" | tail -1
done
rm -rf "$tmp"
