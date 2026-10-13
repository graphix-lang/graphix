#!/bin/sh
# rt-03 (review 2026-10-12): every cancelled timer logs an ERROR.
# Rt::cancel_timer aborts the timer task (graphix-rt/src/rt.rs:252-256);
# the aborted task's JoinError::Cancelled reaches `joined`
# (graphix-rt/src/gx.rs:241-243), which logs it as a task that died.
# after_idle re-arms (cancel + set) on every input, so a debounce logs
# one ERROR per keystroke.
#
# command: GRAPHIX=/path/to/graphix sh rt-03.sh
# expected: no error log (a cancelled timer is the requested outcome)
# observed (HEAD 7324207f): 21 lines of
#   ERROR [graphix_rt::gx] a runtime task ended without replying: task N was cancelled
D=$(mktemp -d)
cat > "$D/ai.gx" <<'GX'
let t = sys::time::timer(duration:10.ms, 20);
let d = sys::time::after_idle(duration:50.ms, t);
sys::exit(sys::time::after_idle(duration:300.ms, d ~ 0));
d
GX
RUST_LOG=error timeout -s KILL 60 "${GRAPHIX:-graphix}" --no-cache --log-dir "$D/logs" "$D/ai.gx" 2>/dev/null
grep -c 'without replying' "$D/logs/graphix.log"
