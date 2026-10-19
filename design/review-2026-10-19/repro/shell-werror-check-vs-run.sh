#!/bin/sh
# Title: under -W error, --check refuses an unused import that the run accepts
# Command: sh shell-werror-check-vs-run.sh
# Expected: `graphix -W error --check f.gx` and `graphix -W error f.gx` agree
#   (both refuse, or both accept), and a plain run warns as --check does.
# Observed: --check prints "unused import `map`" and fails (exit 1) under
#   -W error; the run prints no warning and runs (prints 1, exit 0).
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
printf 'use array::map;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\n1\n' > $D/warn.gx
timeout -s KILL 60 $G -W error --check $D/warn.gx; echo "check exit $?"
timeout -s KILL 60 $G -W error --no-cache $D/warn.gx; echo "run exit $?"
