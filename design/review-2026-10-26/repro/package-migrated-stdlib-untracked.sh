#!/bin/sh
# Title: an old-format packages.toml migrates the stdlib packages it does
#        not name as untracked, not as removed, contrary to
#        book/src/packages/using.md ("counts as removed").
# Command: sh design/review-2026-10-26/repro/package-migrated-stdlib-untracked.sh
# Expected (per the book): sys, gui, ... listed under "removed stdlib packages".
# Observed: only "stdlib packages: array core"; the rewritten file has
#           removed = [] (the code's intended behaviour: migrate_old).
set -e
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
mkdir -p "$d/graphix"
printf '[packages]\ncore = "0.4.0"\narray = "0.4.0"\n' > "$d/graphix/packages.toml"
XDG_DATA_HOME="$d" timeout -s KILL 60 "$GX" package list
cat "$d/graphix/packages.toml"
rm -rf "$d"
