#!/bin/bash
# shell-04: compile warnings print on the cold run only; a warm start is silent.
G=${1:-graphix}
D=$(mktemp -d); export XDG_CACHE_HOME=$D/cache; cd $D
printf 'let r: [i64, Error<`E>] = 1;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nr?\n' > main.gx
echo "--- cold (WARNING ... will not be caught)"; $G main.gx
echo "--- warm (no warning)"; $G main.gx
rm -rf "$D"
