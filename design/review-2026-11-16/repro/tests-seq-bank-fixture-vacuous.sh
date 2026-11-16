#!/bin/sh
# Title: seq.rs SEQ_BARE_SAMPLE_BANK_OUTLIVES_A_PASSING_VALUE cannot tell `~` from `~!`,
#        and the bank it describes IS paid after the wake.
# Command: sh design/review-2026-11-16/repro/tests-seq-bank-fixture-vacuous.sh
# Expected (if the fixture pinned banking): the fixture's program gives 0 with `~` and
#   something else with `~!`.
# Observed: fixture with `~` -> 0, fixture with `~!` -> 0 (same answer: the fixture pins nothing
#   about the bank). A variant where `x` arrives while the arm is awake after the wake:
#   `~` -> 1 (the bank made before the sleep survives it and is paid), `~!` -> 0.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for op in '~' '~!'; do
cat > $D/fixture.gx <<EOF
let step = 0;
step <- select step { s if s < 16 => s + 1, _ => never() };
type Pc = [\`Idle, \`Wait];
let pc: Pc = \`Idle;
pc <- select step { 1 => \`Wait, 7 => \`Idle, 8 => \`Wait, _ => never() };
let x = select step { s if s < 4 => never(), s if s < 6 => 1, s if s < 12 => never(), _ => 2 };
let n = 0;
select pc { \`Idle => never(), \`Wait => { n <- (pc $op x) ~ (n + 1); pc <- pc ~ \`Idle } };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
select step { 16 => n, _ => never() }
EOF
echo "fixture with $op: $(timeout -s KILL 30 $G --no-cache $D/fixture.gx 2>&1 | tail -1)"
cat > $D/variant.gx <<EOF
let step = 0;
step <- select step { s if s < 16 => s + 1, _ => never() };
let on = true;
on <- select step { 3 => false, 9 => true, _ => never() };
let trig = select step { 1 => step, _ => never() };
let x = select step { s if s < 4 => never(), s if s < 6 => 1, s if s < 12 => never(), _ => 2 };
let n = 0;
select on { false => never(), true => n <- (trig $op x) ~ (n + 1) };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
select step { 16 => n, _ => never() }
EOF
echo "variant with $op: $(timeout -s KILL 30 $G --no-cache $D/variant.gx 2>&1 | tail -1)"
done
rm -rf $D
