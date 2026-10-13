#!/bin/bash
# shell-02: a script with a .gxi (or a module with an empty .gxi) never
# restores its program entry: the record lists the interface as absent.
# Usage: shell-02-script-interface-never-warm.sh /path/to/graphix
set -u
G=${1:-graphix}
D=$(mktemp -d); export XDG_CACHE_HOME=$D/cache; cd $D
printf 'let x = 1;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nx\n' > main.gx
printf 'val x: i64\n' > main.gxi
for i in 1 2; do RUST_LOG=info $G --log-dir $D/log main.gx; grep -h "Program image" $D/log/*.log; done
strings $D/cache/graphix/registration/*/*.img | grep '^[-+]/\|^[-+][0-9a-f]* /'
# expect: run 2 says "is stale", the record holds "-.../main.gxi" (twice) and main.gx twice
rm -rf "$D"
