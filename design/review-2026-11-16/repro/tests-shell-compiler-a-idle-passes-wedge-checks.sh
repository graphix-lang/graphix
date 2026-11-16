#!/bin/bash
# interrupt_wedge.rs::signal_frees_process cannot tell a wedged run from an
# idle one.
#
# Its evidence of a wedge is: the child is alive after `wedged_after`, its
# output did not grow over 500 ms, and SIGINT ends it with 130 (RSS growth
# is waited for, never required). A program that printed its value and sits
# idle meets every one of those, so if LATER_CYCLE_WEDGE's timer write or
# f's loop stopped wedging, the JIT cases (whose native loop never grows
# RSS) would still pass. repl_interrupt.rs::Repl::wedge (500 ms after the
# type line) has the same gap.
#
# Run:  bash design/review-2026-11-16/repro/tests-shell-compiler-a-idle-passes-wedge-checks.sh
# Expected (if the checks proved a wedge): the idle program would fail them.
# Observed: idle program: output "0" and nothing after, alive at 2 s, exit 130
#           on SIGINT -- the same status as the wedged program.
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
T=$(mktemp -d)
export XDG_CACHE_HOME=$T/cache
printf '{ let x = i64:0; x }\n' > $T/idle.gx
printf '{ let rec f = |v: i64| -> i64 f(v + i64:1); f(i64:0) }\n' > $T/wedge.gx
for p in idle wedge; do
  timeout -s KILL 30 timeout --preserve-status -s INT 2 "$GX" --no-cache --no-netidx $T/$p.gx > $T/$p.out 2>/dev/null
  echo "$p: exit $? after SIGINT at 2 s; printed: $(tr '\n' ' ' < $T/$p.out)"
done
rm -rf "$T"
