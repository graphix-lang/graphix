#!/bin/bash
# The program entry's source record lists, for a module read from m.gx,
# only `-m.gxi`: a later m/mod.gx (both layouts define m) or a stray
# m/mod.gxi makes the cold compile refuse the program, but the warm
# start restores the image and runs it.
G=${GRAPHIX:-graphix}
t=$(mktemp -d)
printf 'mod m;\nsys::exit(sys::time::after_idle(duration:1.s, 0));\nm::x\n' > $t/main.gx
echo 'let x = 1' > $t/m.gx
export XDG_CACHE_HOME=$t/cache
timeout -s KILL 30 $G $t/main.gx               # 1 (cold, writes the entry)
mkdir $t/m; echo 'let x = 3' > $t/m/mod.gx
echo "--- m/mod.gx beside m.gx"
timeout -s KILL 30 $G $t/main.gx; echo "warm exit $?"          # prints 1, exit 0
timeout -s KILL 30 $G --no-cache $t/main.gx; echo "cold exit $?" # "both define module m", exit 1
rm -r $t/m; mkdir $t/m; echo 'val x: i64' > $t/m/mod.gxi
echo "--- stray m/mod.gxi beside m.gx"
timeout -s KILL 30 $G $t/main.gx; echo "warm exit $?"          # prints 1, exit 0
timeout -s KILL 30 $G --no-cache $t/main.gx; echo "cold exit $?" # "is no interface", exit 1
