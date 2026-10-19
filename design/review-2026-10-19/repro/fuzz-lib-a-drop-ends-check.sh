#!/bin/sh
# Title: one Drop in Oracle::settle ends check_verdict: the JIT's image
#   sessions, the forked runs and the prune pair are never run.
# Command: sh design/review-2026-10-19/repro/fuzz-lib-a-drop-ends-check.sh
# Expected: after the Engine pair drops as CONTAINED (node-walk activation
#   budget beside a JIT trace), the remaining comparisons still run: RUN lines
#   for "Jit InLanguage Cold/Warm", "JitPar ..." and "Par ...".
# Observed: only the group-0 runs (Interp, Jit, Unpruned) are made; the check
#   prints CONTAINED and UNSURE and returns. Jit cold/warm and JitPar never run.
BIN=/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz
D=$(mktemp -d)
cat > "$D/deep.gx" <<'GX'
let rec f = |n: i64, acc: i64| -> i64 select n { 0 => acc, n => f(n - 1, acc + 1) };
f(300000, 0)
GX
GRAPHIX_FUZZ_RUN_LOG=1 timeout -s KILL 180 "$BIN" check "$D/deep.gx" 2>&1 \
  | grep -E '^(RUN|DONE|CONTAINED|UNSURE|AGREE)'
rm -rf "$D"
