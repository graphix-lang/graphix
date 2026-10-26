#!/bin/sh
# Title: `use m::self` (self outside a group) is accepted as `use m`, while
# `use {self}` (self inside a group) is refused with "`self` outside a use
# group".
# Command: sh design/review-2026-10-26/repro/t-parser-b-use-self-outside-group.sh
# Expected: `use m::self` refused (module_system.md: `self` names the module
#   itself "in a group"); `use {self}` refused with a message that does not
#   claim it stands outside a group.
# Observed: `use m::self` checks (exit 0, warns `unused import m`) and
#   `graphix fmt` rewrites it to `use m`; `use {self}` is refused with
#   "note: `self` outside a use group".
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cd "$D" || exit 1
printf 'mod m;\nuse m::self;\nm::x\n' > main.gx
printf 'let x = 1\n' > m.gx
timeout -s KILL 30 $G --check main.gx; echo "use m::self: exit $?"
timeout -s KILL 30 $G fmt --stdout main.gx
printf 'use {self};\n1\n' > s2.gx
timeout -s KILL 30 $G --check s2.gx 2>&1 | grep note
rm -rf "$D"
