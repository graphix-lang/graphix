#!/bin/sh
# Title: a node-walk budget abort beside a JIT deadline agrees as "contained",
#   so a JIT loop that fails to terminate on a deep but bounded recursion is
#   indistinguishable from a genuinely unbounded one.
# Command: sh design/review-2026-10-19/repro/fuzz-lib-a-budget-vs-deadline.sh
# deep.gx terminates: Interp Timeout(ActivationBudget), Jit Trace (CONTAINED drop).
# unb.gx does not:    Interp Timeout(ActivationBudget), Jit Timeout(Deadline) -> AGREE (contained).
# A JIT bug that kept deep.gx's native loop from exiting would turn its pair
# into unb.gx's, which the oracle and outcome.manifest vouch for as contained.
BIN=/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz
D=$(mktemp -d)
cat > "$D/deep.gx" <<'GX'
let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, n => f(n - 1, acc + 1) };
f(300000, 0)
GX
cat > "$D/unb.gx" <<'GX'
let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, n => f(n + 1, acc + 1) };
f(1, 0)
GX
for f in deep unb; do
  echo "== $f"
  timeout -s KILL 120 "$BIN" run "$D/$f.gx" 2>&1 | grep -E '^(Interp|Jit)/InLanguage:'
  timeout -s KILL 170 "$BIN" check "$D/$f.gx" 2>&1 | grep -E 'CONTAINED|UNSURE|AGREE|DIVERGENCE'
done
rm -rf "$D"
