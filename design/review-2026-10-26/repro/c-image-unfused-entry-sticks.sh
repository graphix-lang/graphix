#!/bin/sh
# Title: a run where the JIT is unavailable writes an unfused program entry under the
# fusion-on key, and every later run restores it and runs unfused.
# Run: sh c-image-unfused-entry-sticks.sh
# GRAPHIX_JIT_ARENA=1 makes the arena reservation fail (as a ulimit -v or a full
# address space would); GRAPHIX_DBG_INVOKE=1 prints one line per kernel invocation.
# Expected: every run without GRAPHIX_JIT_ARENA invokes kernels (17 lines).
# Observed:
#   cold normal 17 / warm normal 17 / arena-less run 0 / normal run after it 0
#   (and from an empty cache: arena-less cold run 0, then every normal run 0).
G=/home/eric/tmp/review-2026-10-26/bin/graphix
D=$(mktemp -d)
cat > $D/fz.gx <<'GX'
let f = |a: Array<i64>| array::fold(array::map(a, |x| x * 2 + 1), 0, |acc, x| acc + x);
let step = 0;
step <- select step { s if s < 5 => s + 1, _ => never() };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f([step, step, 3])
GX
run() { GRAPHIX_DBG_INVOKE=1 XDG_CACHE_HOME=$D/cache timeout -s KILL 60 env "$@" $G $D/fz.gx 2>&1 | grep -c "KERNEL INVOKE"; }
echo "cold normal: $(run X=1)"
echo "warm normal: $(run X=1)"
echo "arena-less:  $(run GRAPHIX_JIT_ARENA=1)"
echo "normal after: $(run X=1)"
