#!/bin/sh
# Title: a fused kernel whose values merely carry an abstract type never
# forks its loops, even under GRAPHIX_PAR=force, though nothing in it
# compares, orders, hashes, prints or casts the abstract value.
# Command: sh f-kernel-abstract-blocks-fork.sh
# Expected: both programs print "PAR kernel loop" lines (the 200-slot map
#   and each callee's 20-slot init fork as chunks), as the second does.
# Observed: the program over (Tok, i64) prints none; the same program over
#   (i64, i64) prints 2 + 200 of them. Same result value (241861) in both.
#   lowering::meets_abstract is true for any node whose type holds an
#   abstract, so FusedKernel::update runs the kernel under
#   with_display_hooks; core's `impl<'a> Cast for Transparent<'a>` makes
#   coretraits::hooks_live true in every program, so the loan is taken
#   (and the Env cloned) on every invocation, and par_loop::run sees
#   value_hooks_loaned() and runs every loop in order.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
cat > "$D/abs.gx" <<'EOF'
type Tok = Abstract<i64>;
let heavy = |x: i64| array::fold(array::init(20, |i| i), x, |acc, i| (acc * 31 + i) % 1000003);
let rows = array::init(200, |i| (Tok(i), i));
let f = |rows: Array<(Tok, i64)>| array::map(rows, |r| heavy(r.1));
array::fold(f(rows), 0, |a, b| (a + b) % 1000007)
EOF
sed 's/type Tok = Abstract<i64>;/type Tok = i64;/; s/Tok(i)/i/' "$D/abs.gx" > "$D/plain.gx"
for p in abs plain; do
  echo "== $p"
  GRAPHIX_PAR=force GRAPHIX_DBG_PAR=1 timeout -s KILL 20 "$G" --no-cache "$D/$p.gx" 2>&1 \
    | grep -v calibrated | cut -c1-60 | sort | uniq -c
done
rm -rf "$D"
