#!/bin/bash
# shell-06: the program cache key hashes the program id and GRAPHIX_MODPATH
# unframed, so (p/prog, MODPATH=.gx) and (p/prog.gx, no MODPATH) share a key
# and the second run restores the first program.
# Usage: shell-06-key-unframed.sh /path/to/graphix
# expected: the last run prints "I am prog.gx"; observed: "I am prog"
set -u
G=${1:-graphix}
D=$(mktemp -d); export XDG_CACHE_HOME=$D/cache; mkdir $D/p
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\n"I am prog"\n' > $D/p/prog
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 0));\n"I am prog.gx"\n' > $D/p/prog.gx
GRAPHIX_MODPATH=.gx $G $D/p/prog
env -u GRAPHIX_MODPATH $G $D/p/prog.gx
rm -rf "$D"
