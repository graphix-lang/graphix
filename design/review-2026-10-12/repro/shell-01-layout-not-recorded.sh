#!/bin/bash
# shell-01: a program entry is restored after a file appears that makes the
# cold compile refuse the program (the other module layout, or a stray
# interface). Usage: shell-01-layout-not-recorded.sh /path/to/graphix
set -u
G=${1:-graphix}
D=$(mktemp -d); export XDG_CACHE_HOME=$D/cache; cd $D
printf 'mod m;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nm::x\n' > main.gx
printf 'let x = 1\n' > m.gx
$G main.gx                      # cold: prints 1, writes the program entry
mkdir m; printf 'let x = 2\n' > m/mod.gx
echo "--- --no-cache (expect: both define module m)"; $G --no-cache main.gx
echo "--- cached (bug: prints 1)"; $G main.gx
# same with a stray m/mod.gxi beside m.gx, or m.gxi beside m/mod.gx
rm -rf "$D"
