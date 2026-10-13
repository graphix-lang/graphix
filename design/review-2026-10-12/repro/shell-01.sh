#!/bin/bash
# shell-01: Program entry ignores the other module layout: warm start runs a
# program the cold compile refuses.
# Command: bash design/review-2026-10-12/repro/shell-01.sh /path/to/graphix
# Expected: after each change, the cached run fails like the --no-cache run.
# Observed (HEAD 7324207f): every --no-cache run exits 1 ("both define module
# m" / "is no interface"), every cached run restores the stale program entry
# and prints the old value (exit 0).
set -u
G=${1:-graphix}
run() { # $1 = setup before the cold run, $2 = change after it
    D=$(mktemp -d); export XDG_CACHE_HOME=$D/cache; cd "$D"
    printf 'mod m;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nm::x\n' > main.gx
    eval "$1"
    echo "=== $2"
    echo "-- cold";     timeout -s KILL 60 "$G" main.gx; echo "exit $?"
    eval "$2"
    echo "-- no-cache"; timeout -s KILL 60 "$G" --no-cache main.gx; echo "exit $?"
    echo "-- cached";   timeout -s KILL 60 "$G" main.gx; echo "exit $?"
    cd /; rm -rf "$D"
}
run "printf 'let x = 1\n' > m.gx" "mkdir m; printf 'let x = 2\n' > m/mod.gx"
run "printf 'let x = 1\n' > m.gx" "mkdir m; printf 'val x: i64;\n' > m/mod.gxi"
run "mkdir m; printf 'let x = 2\n' > m/mod.gx" "printf 'val x: i64;\n' > m.gxi"
