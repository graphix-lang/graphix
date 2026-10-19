#!/bin/sh
# Title: a reference held inside a nominal abstract, or under a result typed
#   Any, is not canonicalized in the trace, so every comparison of such a
#   program drops as NONDETERMINISTIC.
# Command: sh design/review-2026-10-19/repro/fuzz-lib-a-abstract-ref-nondet.sh
# Expected: both programs AGREE (a reference prints as &ref0, as it does for
#   `[i64, `W(&i64)]` results).
# Observed: each run renders a different abstract:<bytes> (the raw cell id);
#   check prints NONDETERMINISTIC ... not recorded, then UNSURE.
BIN=/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz
D=$(mktemp -d)
cat > "$D/absref.gx" <<'GX'
{
  type T = Abstract<&i64>;
  let x = 1;
  T(&x)
}
GX
cat > "$D/anyref.gx" <<'GX'
{
  let x = 1;
  let f = |a: Any| -> Any a;
  f(&x)
}
GX
for f in absref anyref; do
  echo "== $f"
  timeout -s KILL 170 "$BIN" check "$D/$f.gx" 2>&1 | grep -E 'NONDETERMINISTIC|first:|rerun:|UNSURE|AGREE|DIVERGENCE'
done
rm -rf "$D"
