#!/bin/sh
# Title: a catch as a module's last statement covers nothing and is accepted,
# while the same catch ending a block or a script is refused.
# Command:
#   sh design/review-2026-10-19/repro/c-node-mod-module-trailing-catch.sh /home/eric/tmp/review-2026-10-19/bin/graphix
# Expected: both checks refuse with "a catch covers the statements after it,
#   and the last statement of a block has none: it would cover nothing".
# Observed: the module check passes (exit 0); only the script is refused.
G=${1:-graphix}
d=$(mktemp -d)
printf 'let x = 1;\ncatch(e) println("caught")\n' > "$d/m.gx"
printf 'mod m;\nm::x\n' > "$d/main.gx"
printf 'let x = 1;\ncatch(e) println("caught")\n' > "$d/script.gx"
echo "-- module ending in catch:"; timeout -s KILL 10 "$G" --check "$d/main.gx"; echo "exit $?"
echo "-- script ending in catch:"; timeout -s KILL 10 "$G" --check "$d/script.gx" 2>&1 | tail -1
rm -r "$d"
