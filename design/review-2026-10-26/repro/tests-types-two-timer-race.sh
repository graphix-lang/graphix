#!/bin/sh
# Title: written_binding_takes_its_type orders its sample by two independent timers
# Command: sh tests-types-two-timer-race.sh
# The fixture (stdlib/graphix-tests/src/lang/types.rs WRITTEN_BINDING_TAKES_ITS_TYPE) samples r on a
# 200 ms timer and relies on the 50 ms timer's write having landed first. When both timers' fires
# reach one cycle (a runtime stalled 150 ms under a loaded test run), r is sampled before the write:
# this script makes them coincide by giving both timers 50 ms.
# Expected (the fixture's predicate): 42.  Observed with coinciding timers: 41.
# With the second event derived from the write instead (skip(#n: 1, r)) the result is 42 whatever the timing.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
T=$(mktemp -d)
cat > $T/race.gx <<'GX'
{
    let f = |x| x;
    let t = sys::time::timer(duration:50.ms, false);
    f <- t ~ (|x: i64| x + 1);
    let r = f(41);
    sys::time::timer(duration:50.ms, false) ~ r
}
GX
timeout -s KILL 5 $G --no-cache $T/race.gx
rm -rf $T
