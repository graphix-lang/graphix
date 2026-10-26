#!/bin/sh
# Title: a `use .. as` rename may change a name's case class, binding a name no code can write
# Command: sh t-parser-b-rename-case.sh
# Expected: the check refuses `use m::T as t` (a type renamed lowercase) and `use m::v as V`
#   (a value renamed uppercase), since a type position parses only a capitalized name and an
#   uppercase name in an expression is a type or abstract constructor.
# Observed: both are accepted; the only output is two "unused import" warnings, and any
#   attempt to use the names (`let x: t = 1`, `V + 1`) is a parse error far from the cause.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf 'type T = i64;\nlet v = 1\n' > "$D/m.gx"
printf 'mod m;\nuse m::T as t;\nuse m::v as V;\n1\n' > "$D/main.gx"
timeout -s KILL 30 "$G" --check "$D/main.gx"; echo "rc=$?"
rm -rf "$D"
