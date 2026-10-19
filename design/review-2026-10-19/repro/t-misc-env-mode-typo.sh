#!/bin/sh
# Title: GRAPHIX_PAR / GRAPHIX_PRUNE silently fall back to the default on an
#        unrecognised value (stack.rs ParMode::from_env, PruneMode::from_env).
# Command: sh design/review-2026-10-19/repro/t-misc-env-mode-typo.sh
# Expected: GRAPHIX_PAR=Force either forces forking (as `force` does) or
#           prints that the value is not a mode, as GRAPHIX_STACK_BUDGET does.
# Observed: `force` prints "PAR kernel loop: 8 slots, 0 probed, 8 ranges";
#           `Force` prints nothing: the run is silently Auto. GRAPHIX_PRUNE=Audit
#           likewise runs pruned without the audit.
BIN=${BIN:-/home/eric/tmp/review-2026-10-19/bin/graphix}
T=$(mktemp -d)
cat > "$T/p.gx" <<'GX'
let f = |a: Array<i64>| array::map(a, |x| x * 2);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f([1, 2, 3, 4, 5, 6, 7, 8])
GX
for v in force Force; do
  echo "== GRAPHIX_PAR=$v"
  GRAPHIX_PAR=$v GRAPHIX_DBG_PAR=1 timeout -s KILL 60 "$BIN" --no-cache "$T/p.gx" 2>&1 | grep -v calibrat
done
rm -rf "$T"
