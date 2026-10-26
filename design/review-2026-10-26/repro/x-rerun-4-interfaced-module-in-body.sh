#!/bin/sh
# Title: a file module with a .gxi declared in a function body is refused: "sig item val y: i64 is missing an implementation"
# Command: sh design/review-2026-10-26/repro/x-rerun-4-interfaced-module-in-body.sh [path/to/graphix]
# Expected: every case checks (exit 0): m.gx implements the one val its m.gxi declares.
# Observed: top level and top-level block: exit 0; the same `mod m;` in a lambda body
#   (called or not, annotated or generic, GRAPHIX_NO_SUBST=1 alike): "sig item val y: i64
#   is missing an implementation", exit 1; deleting m.gxi makes the body case check.
G=${1:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf 'let y = 2\n' > $D/m.gx
printf 'val y: i64;\n' > $D/m.gxi
printf 'mod m;\nm::y + 1\n' > $D/top.gx
printf '{\n  mod m;\n  m::y + 1\n}\n' > $D/block.gx
printf 'let h = |k: i64| -> i64 {\n  mod m;\n  m::y + k\n};\nh(1)\n' > $D/body.gx
for f in top block body; do
  echo "== $f"; timeout -s KILL 30 $G --check $D/$f.gx > $D/out 2>&1; echo "exit $?"; tail -1 $D/out
done
rm $D/m.gxi
echo "== body without m.gxi"; timeout -s KILL 30 $G --check $D/body.gx > $D/out 2>&1; echo "exit $?"
rm -rf $D
