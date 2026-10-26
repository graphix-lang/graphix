#!/bin/sh
# Title: a mistyped GRAPHIX_EVAL_THREADS is ignored silently, unlike every other knob
# Command: sh design/review-2026-10-26/repro/t-misc-eval-threads-silent.sh
# Expected: a message for each malformed knob, as GRAPHIX_PAR gets
# Observed: only "GRAPHIX_PAR="fast" is not one of off, auto, force; using the default";
#           GRAPHIX_EVAL_THREADS=four (and =0) runs with one worker per core, saying nothing
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf 'sys::exit(sys::time::after_idle(duration:50.ms, 0));\n1\n' > $D/one.gx
GRAPHIX_EVAL_THREADS=four GRAPHIX_PAR=fast timeout -s KILL 30 $GX --no-cache $D/one.gx
rm -rf $D
