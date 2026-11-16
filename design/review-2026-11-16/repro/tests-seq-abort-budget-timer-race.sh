#!/bin/sh
# Title: seq_abort.rs abort_timer_starts_with_the_run (and abort_timer_budgets_every_run's run 22)
#        race a 40 ms tick interval against a 140 ms abort budget with 60 ms to spare.
# Command: sh design/review-2026-11-16/repro/tests-seq-abort-budget-timer-race.sh
# The fixture's program, plus a computation at tick 2 that holds the runtime thread for a while
# (standing in for a loaded box). Run 1 needs tick 3 before its budget ends; a stall of about
# 100 ms between tick 2 and tick 3 lets the budget win.
# Expected (by the test): 1 whatever the load. Observed: burn 0 / 10000 -> 1; burn 40000 (and
# on some runs 20000) -> no value: run 1 aborted, so the test fails under a ~100 ms stall.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for burn in 0 10000 20000 40000; do
cat > $D/p.gx <<EOF
let tick = count(sys::time::timer(duration:40.ms, 16)?);
let burn = select tick { 2 => array::len(array::init($burn, |i| i * 2)), _ => never() };
let go = select tick { 1 | 9 => tick, _ => never() };
let r = seq go; abort(sys::time::timer(duration:140.ms, false)) {
    until live(tick) > go + select go { 1 => 1, _ => 6 };
    go
};
sys::exit(sys::time::after_idle(duration:900.ms, tick));
r
EOF
echo "burn $burn: $(timeout -s KILL 60 $G --no-cache --no-fusion $D/p.gx 2>/dev/null | tail -1)"
done
rm -rf $D
