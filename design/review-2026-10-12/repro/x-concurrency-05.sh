#!/bin/sh
# x-concurrency-05: every cancelled timer logs ERROR "a runtime task ended
# without replying: task N was cancelled" (graphix-rt/src/gx.rs `joined`).
# An after_idle debounce re-armed by each input cancels its timer each time.
# Command: sh design/review-2026-10-12/repro/x-concurrency-05.sh
# Expected: no ERROR lines (a cancelled timer is ordinary operation).
# Observed: one ERROR per re-arm on stderr (21 at HEAD 7324207f), e.g.
#   ERROR [graphix_rt::gx] a runtime task ended without replying: task 209 was cancelled
# The same happens for a sys::time::timer whose select arm sleeps
# (repro/x-concurrency-01.gx).
d=$(mktemp -d)
cat > "$d/p.gx" <<'GX'
let clk = sys::time::timer(duration:5.ms, true);
let n = 0;
n <- clk ~ n + 1;
let d = sys::time::after_idle(duration:50.ms, n);
sys::exit(n ~ select n > 20 { true => 0, false => never() });
d
GX
RUST_LOG=error timeout -s KILL 30 graphix --no-cache --log-dir "$d" "$d/p.gx" 2>&1 >/dev/null | grep -c 'was cancelled'
