#!/bin/sh
# Title: a package's src/graphix/main.gxi applies when main.gx is run as a
#        file, but a standalone build embeds main.gx alone (Load::Text, no
#        interface) and packs main.gxi into the VFS as a stray module
# Command: sh design/review-2026-11-16/repro/shell-standalone-main-gxi.sh
# Expected: the program behaves the same run as a file and as the
#   standalone binary's embedded program.
# Observed (debug build at e1be2fbf2):
#   as a file (main.gxi beside it): 1, exit 0
#   as the standalone runs it (main.gx text with no interface, which is what
#   graphix-derive's main_program_impl include_str!s and Shell::configure
#   loads as Load::Text, sig None in GX::load_exprs):
#   "undefined type P", exit 1
# The standalone half is simulated by running a copy of main.gx with no
# .gxi beside it; building a standalone binary needs cargo.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
mkdir -p "$D/pkg/src/graphix" "$D/standalone"
printf 'type P = {x: i64};\n' > "$D/pkg/src/graphix/main.gxi"
printf 'let p: P = {x: 1};\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\np.x\n' \
  > "$D/pkg/src/graphix/main.gx"
cp "$D/pkg/src/graphix/main.gx" "$D/standalone/main.gx"
echo "as a file:"
timeout -s KILL 60 "$G" --no-cache "$D/pkg/src/graphix/main.gx"; echo "exit $?"
echo "as the standalone runs it:"
out=$(timeout -s KILL 60 "$G" --no-cache "$D/standalone/main.gx" 2>&1); rc=$?
echo "$out" | tail -1; echo "exit $rc"
rm -rf "$D"
