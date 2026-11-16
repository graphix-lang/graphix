#!/bin/bash
# Title: a book example whose check WARNS still passes examples_compile
#
# examples_compile builds every example with only CFlag::ExpandSeq, so a
# check warning (here: a constant write in a select arm, the trap the input
# page warns of) is printed and the build returns Ok. The same program fails
# with warnings as errors (-W error), and every one of the 129 book examples
# and 43 bench programs passes -W error today.
#
# Command: bash design/review-2026-11-16/repro/examples-tui-warning-passes-gate.sh
# Expected: the gate's mode refuses the broken example (rc 1).
# Observed: "WARNING ... constants in an arm do not fire ..." then rc=0 for the
#   gate's mode; rc=1 only with -W error.
set -u
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
export XDG_CACHE_HOME=$TMP/cache
sed 's/mode <- k ~ `Insert;/mode <- `Insert;/' \
  "$ROOT/book/src/examples/tui/input_vim.gx" > "$TMP/input_vim.gx"
diff "$ROOT/book/src/examples/tui/input_vim.gx" "$TMP/input_vim.gx"
echo "== the gate's mode (check, warnings printed)"
timeout -s KILL 60 "$GX" --check --no-cache "$TMP/input_vim.gx"; echo "rc=$?"
echo "== warnings as errors"
timeout -s KILL 60 "$GX" --check --no-cache -W error "$TMP/input_vim.gx" 2>&1 | tail -1; echo "rc=${PIPESTATUS[0]}"
