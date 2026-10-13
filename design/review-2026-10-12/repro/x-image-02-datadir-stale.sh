#!/bin/bash
# A program entry is keyed on GRAPHIX_MODPATH but not on the data dir
# (XDG_DATA_HOME/graphix), which search_dirs() also searches: after the
# data dir changes, the warm run executes the module from the OLD data
# dir. Expected: 1, 1, 2, 2. Observed: 1, 1, 1 (RESTORED), 2.
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
