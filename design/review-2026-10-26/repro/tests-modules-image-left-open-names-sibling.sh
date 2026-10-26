#!/bin/sh
# Title: module a's "left open" refusal tells the user to annotate sibling b's `w`,
#   at b.gx's line and column, with no file named
# Command: sh design/review-2026-10-26/repro/tests-modules-image-left-open-names-sibling.sh
# Expected: ... left open: annotate `x` (line 1, column 5)
# Observed (HEAD e6ffc6eed, debug):
#   2: the check decides a type the code around the module left open:
#      annotate `x` (line 1, column 5), `w` (line 3, column 5)
#   `w` is b.gx's binding (line 3 of b.gx); the error is placed in main.gx, where
#   line 3 is `mod b;`. With only `mod a` the hint names `x` alone.
d=$(mktemp -d)
printf 'let x = never();\nmod a;\nmod b;\na::y + b::w\n' > $d/main.gx
printf 'val y: i64;\n' > $d/a.gxi
printf 'let y = super::x + 1\n' > $d/a.gx
printf 'val w: i64;\n' > $d/b.gxi
printf '\n\nlet w = super::x + 2\n' > $d/b.gx
timeout -s KILL 60 ${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix} --no-cache --check $d/main.gx 2>&1
rm -rf $d
