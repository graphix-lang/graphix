#!/bin/bash
# t-format-resolver: parse_modpath skips a GRAPHIX_MODPATH entry whose scheme no
# factory reads, while its doc comment and book/src/shell.md say "entries with
# no registered scheme are filesystem paths".
#
# Run: bash design/review-2026-11-16/repro/t-format-resolver-modpath-unregistered-scheme.sh
# Expected (per the docs): GRAPHIX_MODPATH=lib:mods is the directory `lib:mods`,
#   so `mod m;` finds lib:mods/m.gx and the check passes.
# Observed (e1be2fbf2): the entry is skipped (scheme `lib` has no resolver; a
#   warning at most) and the check fails: "module m could not be found".
set -u
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
mkdir -p "$D/lib:mods" "$D/prog"
printf 'let x = 1\n' > "$D/lib:mods/m.gx"
printf 'mod m;\nm::x\n' > "$D/prog/main.gx"
cd "$D"
GRAPHIX_MODPATH='lib:mods' timeout -s KILL 30 $G --no-cache -n -i --check prog/main.gx 2>&1 | tail -2; echo "rc=${PIPESTATUS[0]}"
echo "== the same directory as file:lib:mods"
GRAPHIX_MODPATH="file:$D/lib:mods" timeout -s KILL 30 $G --no-cache -n -i --check prog/main.gx; echo "rc=$?"
