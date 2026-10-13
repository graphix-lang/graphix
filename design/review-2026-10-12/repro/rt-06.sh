#!/bin/sh
# rt-06 (review 2026-10-12): every cancelled timer is logged as an ERROR
# "a runtime task ended without replying".
# Rt::cancel_timer aborts the timer's task in the `tasks` JoinSet; the
# JoinSet still yields it as Err(JoinError::Cancelled), and `joined`
# (graphix-rt/src/gx.rs) logs every JoinError at error level. after_idle
# re-arms (cancel + set) on every input, so a debounce logs one ERROR per
# event. (Same defect as rt-03.sh.)
#
# command: GRAPHIX=/path/to/graphix sh rt-06.sh
# expected: 0 (a cancelled timer is the requested outcome, not a failure)
# observed (HEAD 7324207f): 21, each line
#   ERROR [graphix_rt::gx] a runtime task ended without replying: task N was cancelled
D=$(mktemp -d)
cat > "$D/ai.gx" <<'GX'
let t = sys::time::timer(duration:10.ms, 20);
let d = sys::time::after_idle(duration:50.ms, t);
sys::exit(sys::time::after_idle(duration:300.ms, d ~ 0));
d
GX
RUST_LOG=error timeout -s KILL 60 "${GRAPHIX:-graphix}" --no-cache --log-dir "$D/logs" "$D/ai.gx" >/dev/null 2>&1
grep 'without replying' "$D/logs/graphix.log" | head -2
grep -c 'without replying' "$D/logs/graphix.log"
