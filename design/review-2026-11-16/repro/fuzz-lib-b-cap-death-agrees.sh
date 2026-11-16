#!/bin/bash
# Title: a check child the address-space cap stops answers AGREE when `died`
#        cannot pin the death on a JIT run beside a node-walk trace.
# Command: bash design/review-2026-11-16/repro/fuzz-lib-b-cap-death-agrees.sh
#   (GRAPHIX_FUZZ=<a graphix-fuzz binary>, default the review's probe wrapper)
# Expected: the isolated check (what a campaign's derive and `minimize` use)
#   reports the twin DIVERGENCE the in-process check reports, or names the
#   cap death it could not attribute.
# Observed (near the cap the runs race, so several tries):
#   [1] in-process check: "DIVERGENCE — twin invariant violated ...".
#   [2] a replica of the check-one child (its env, 200 MB cap) dies at the
#       cap, "memory allocation of N bytes failed", with RUN Interp and
#       RUN Jit in flight (exit 134), or ends in a false finding/agreement.
#   [3] a replica of the serial re-check child dies with only
#       "RUN Interp InLanguage NoCache" in flight: `died` filters node-walk
#       runs out, so nothing is re-run and it answers Agree.
#   [4] the isolated check at 200 MB: "AGREE" (most tries; once a false
#       "parallel evaluation bug" naming RuntimeErr(runtime init failed)).
set -u
FUZZ=${GRAPHIX_FUZZ:-/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz}
CAP=201326592
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
cat > "$D/twin.gx" <<'GX'
{ let n = array::len([1, 2, 3]); select n > 0 { true => `TwinDiverged, false => `Ok } }
GX
cd "$D"
child_env=(GRAPHIX_EVAL_THREADS=2 MALLOC_ARENA_MAX=2 GRAPHIX_ABORT_ON_NO_MATCH=1
  GRAPHIX_FUZZ_RUN_LOG=1 GRAPHIX_FUZZ_SANDBOXED=1 GRAPHIX_FUZZ_MEM_LIMIT=$CAP)
echo "[1] in-process check"
timeout -s KILL 120 "$FUZZ" check twin.gx 2>/dev/null | grep -E '^(DIVERGENCE|AGREE|UNSURE)'
for i in 1 2 3; do
  echo "[2] check-one replica, try $i"
  env "${child_env[@]}" timeout -s KILL 120 "$FUZZ" check twin.gx >o 2>e
  echo "  exit $?; $(grep -E '^(DIVERGENCE|AGREE|UNSURE)' o | cut -c1-60)"
  grep -E '^(RUN|DONE) |memory allocation' e | tail -3 | sed 's/^/    /'
done
for i in 1 2; do
  echo "[3] serial re-check replica, try $i"
  env "${child_env[@]}" GRAPHIX_FUZZ_SERIAL_RUNS=1 timeout -s KILL 120 "$FUZZ" check twin.gx >o 2>e
  echo "  exit $?; $(grep -E '^(DIVERGENCE|AGREE|UNSURE)' o | cut -c1-60)"
  grep -E '^(RUN|DONE) |memory allocation' e | tail -2 | sed 's/^/    /'
done
for i in 1 2 3; do
  echo "[4] isolated check at the cap, try $i"
  GRAPHIX_FUZZ_MEM_LIMIT=$CAP timeout -s KILL 170 "$FUZZ" minimize twin.gx 1 2>&1 \
    | grep -E '^(DIVERGENCE|AGREE|CRASH)' | cut -c1-90
done
