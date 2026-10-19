#!/bin/bash
# Title: "the check decides a type the code around the module left open"
#   names neither the binding nor the place that decided it
# Command: bash design/review-2026-10-19/repro/tests-modules-image-left-open-names-nothing.sh
# Expected: the refusal names the outer binding whose type the module's
#   check would decide (`x`, `z`) and the module expression that decided it.
# Observed (HEAD 9445a1d21): both programs get the identical message at
#   `mod m`: "compiling module m / the check decides a type the code around
#   the module left open: annotate the binding it decides" -- which binding
#   (x or z) and which use in m.gx are not said.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d); cd "$d"
printf 'val y: i64;\n' > m.gxi
printf 'let a = 1;\nlet b = 2;\nlet y = a + super::x + b\n' > m.gx
printf 'let x = never();\nlet z = never();\nz <- 1;\nmod m;\nm::y\n' > one.gx
echo "== x decided"; timeout -s KILL 30 "$G" --check one.gx 2>&1 | tail -3
printf 'let a = 1;\nlet b = super::z;\nlet y = a + super::x + b\n' > m.gx
printf 'let x: i64 = never();\nlet z = never();\nmod m;\nm::y\n' > two.gx
echo "== z decided"; timeout -s KILL 30 "$G" --check two.gx 2>&1 | tail -3
rm -rf "$d"
