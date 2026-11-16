#!/bin/sh
# Title: seq_steps.rs let_fire_read_live_is_not_raised_again cannot see a re-raise of `a`.
# Command: sh design/review-2026-11-16/repro/tests-seq-until-count-vacuous.sh
# The fixture's `until count(a) >= 2` takes `a` through the step's entry snapshot
# (`any(pc, once(sin)) ~! sin`, printed by --expand below), which fires only at the entry:
# count sees one fire whatever the fire tracker does with `a` afterwards, so a re-raise of
# `a` in a later cycle could not make the run complete. `live(count(a))` counts `a`'s raw fires.
# Expected/observed: snapshot form -> (no value); `live(count(a)) >= 2` -> (no value, the rule
# holds); `live(count(a)) >= 1` -> 2 (control: the live count sees a's one fire).
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
for v in 'until count(a) >= 2' 'until live(count(a)) >= 2' 'until live(count(a)) >= 1'; do
cat > $D/p.gx <<EOF
let step = 0;
step <- select step { n if n < 8 => n + 1, _ => never() };
let go = select step { 2 => step, _ => never() };
let r = seq go { let a = go; $v; a };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
EOF
echo "$v: $(timeout -s KILL 30 $G --no-cache $D/p.gx 2>&1 | tail -1)"
done
sed -i 's/until live(count(a)) >= 1/until count(a) >= 2/' $D/p.gx
timeout -s KILL 30 $G --no-cache --expand $D/p.gx 2>&1 | grep -A12 'S1 until'
rm -rf $D
