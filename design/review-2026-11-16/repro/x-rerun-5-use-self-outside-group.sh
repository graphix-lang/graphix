#!/bin/sh
# Title: `use m::self` (self outside a group) is accepted as `use m`, while `use {self}`
#   (self inside a group) is refused with "`self` outside a use group" (the 10-26 probe
#   t-parser-b-use-self-outside-group, never filed; still so).
# Command: sh design/review-2026-11-16/repro/x-rerun-5-use-self-outside-group.sh [graphix]
# Expected (design/module_system.md: `self` names the module itself "in a group"):
#   `use m::self` refused as self outside a group; `use {self}` refused with a message
#   that says the group has no path for `self` to name.
# Observed: `use m::self` checks (exit 0, warns unused import `m`) and `graphix fmt`
#   rewrites it to `use m`; `use {self}` gets "note: `self` outside a use group".
G=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cd "$D" || exit 1
printf 'mod m;\nuse m::self;\nm::x\n' > main.gx
printf 'let x = 1\n' > m.gx
timeout -s KILL 30 $G --check main.gx; echo "use m::self: exit $?"
timeout -s KILL 30 $G fmt --stdout main.gx
printf 'use {self};\n1\n' > s2.gx
timeout -s KILL 30 $G --check s2.gx 2>&1 | grep note
rm -rf "$D"
