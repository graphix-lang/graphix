#!/bin/bash
# x-image-02: program entry not keyed on the data-dir search path; a warm
# run executes a root module from the OLD $XDG_DATA_HOME/graphix.
# Command: GRAPHIX=path/to/graphix bash design/review-2026-10-12/repro/x-image-02.sh
# Expected: 1, 1, 2, 2 (the third run resolves m from d2, as the cold run does).
# Observed: 1, 1, 1, 2 (the third run restores the program entry, whose
# source record lists +.../d1/graphix/m.gx, still present and unchanged).
G=${GRAPHIX:-graphix}
t=$(mktemp -d)
mkdir -p $t/proj $t/d1/graphix $t/d2/graphix
printf 'mod m;\nsys::exit(sys::time::after_idle(duration:1.s, 0));\nm::x\n' > $t/proj/main.gx
echo 'let x = 1' > $t/d1/graphix/m.gx
echo 'let x = 2' > $t/d2/graphix/m.gx
export XDG_CACHE_HOME=$t/cache
unset GRAPHIX_MODPATH
XDG_DATA_HOME=$t/d1 timeout -s KILL 30 $G $t/proj/main.gx
XDG_DATA_HOME=$t/d1 timeout -s KILL 30 $G $t/proj/main.gx
XDG_DATA_HOME=$t/d2 timeout -s KILL 30 $G $t/proj/main.gx
XDG_DATA_HOME=$t/d2 timeout -s KILL 30 $G --no-cache $t/proj/main.gx
