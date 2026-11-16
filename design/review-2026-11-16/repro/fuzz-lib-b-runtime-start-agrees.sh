#!/bin/bash
# Title: runs whose runtime could not start agree (RuntimeErr == RuntimeErr):
#        a check that ran nothing answers "AGREE (reject)".
# Command: bash design/review-2026-11-16/repro/fuzz-lib-b-runtime-start-agrees.sh
#   (GRAPHIX_FUZZ=<a graphix-fuzz binary>, default the review's probe wrapper)
# Expected: a check none of whose runs started reports a harness failure,
#   never an agreement; this program is a twin DIVERGENCE when it runs.
# Observed: under a check child's environment and a 128 MB address-space
#   cap every run is "RuntimeErr(runtime init failed: starting the runtime's
#   thread)" and `check` prints "AGREE (reject) — every comparison the
#   program owes holds"; the isolated check (campaign derive, `minimize`)
#   prints "AGREE". Partial start failures give false findings instead
#   (see fuzz-lib-b-cap-death-agrees.sh [4]).
set -u
FUZZ=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz}
CAP=134217728
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
cat > "$D/twin.gx" <<'GX'
{ let n = array::len([1, 2, 3]); select n > 0 { true => `TwinDiverged, false => `Ok } }
GX
cd "$D"
child_env=(GRAPHIX_EVAL_THREADS=2 MALLOC_ARENA_MAX=2 GRAPHIX_ABORT_ON_NO_MATCH=1
  GRAPHIX_FUZZ_SANDBOXED=1 GRAPHIX_FUZZ_MEM_LIMIT=$CAP)
echo "== uncapped check"
timeout -s KILL 120 "$FUZZ" check twin.gx 2>/dev/null | grep -E '^(DIVERGENCE|AGREE|UNSURE)'
echo "== capped runs"
env "${child_env[@]}" timeout -s KILL 120 "$FUZZ" run twin.gx 2>/dev/null \
  | grep -E '^(Interp|Jit)/' | cut -c1-110
echo "== capped check"
env "${child_env[@]}" timeout -s KILL 120 "$FUZZ" check twin.gx 2>/dev/null \
  | grep -E '^(DIVERGENCE|AGREE|UNSURE)'
echo "== isolated check, children capped"
GRAPHIX_FUZZ_MEM_LIMIT=$CAP timeout -s KILL 170 "$FUZZ" minimize twin.gx 1 2>&1 \
  | grep -E '^(DIVERGENCE|AGREE|CRASH)'
