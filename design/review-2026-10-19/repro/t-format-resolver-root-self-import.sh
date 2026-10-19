#!/bin/sh
# A script that declares a module of its own file name (`mod main;` in
# main.gx) loads ITSELF as module `main`: the root file never enters the
# resolver's LoadChain, so the self-import is not reported as a cycle and
# the error is about main/main.gx, a file nobody named.
#
# Run: sh design/review-2026-10-19/repro/t-format-resolver-root-self-import.sh
# Expected: "import cycle: main -> main (...main.gx)" at the `mod main` statement
# Observed: module main::main could not be found: <dir>/main/main.gx or
#           <dir>/main/main/mod.gx: no such file
B=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
printf 'mod main;\n1\n' > "$d/main.gx"
timeout -s KILL 30 "$B" --check "$d/main.gx"
echo "exit $?"
rm -rf "$d"
