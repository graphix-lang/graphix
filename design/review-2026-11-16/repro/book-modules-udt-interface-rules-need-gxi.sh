#!/bin/bash
# Title: book/src/modules/interfaces.md "Impls Between Siblings" and "A Module Decides Only Its
# Own Types": both rules hold only for a module that has its own .gxi, which neither the prose
# nor the examples say: the p.gx/p/c.gx example has no c.gxi, and a sibling without an
# interface sees the hidden impl.
# Command: bash book-modules-udt-interface-rules-need-gxi.sh
# Expected per the book: (1) b's Show::show(1) is refused ("i64 does not implement Show here");
#   (2) the p.gx / p/c.gx example is refused ("annotate `cell`").
# Observed: (1) b without b.gxi: accepted; with b.gxi: refused.
#           (2) as written (no p/c.gxi): accepted; with p/c.gxi: refused.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d /home/eric/tmp/review-2026-11-16/work/book-modules-udt/gxi.XXXX)
mkdir -p $D/s $D/q/p
printf 'trait Show { val show: fn(self) -> string };\nmod a;\nmod b;\nb::y\n' > $D/s/main.gx
printf 'use super::Show;\nval x: i64;\n' > $D/s/a.gxi
printf 'impl Show for i64 { let show = |x| "int [x]" };\nlet x = 1\n' > $D/s/a.gx
printf 'use super::Show;\nlet y = Show::show(1)\n' > $D/s/b.gx
echo "(1) sibling b without an interface:"; timeout -s KILL 20 $G --no-cache --check $D/s/main.gx 2>&1 | tail -1; echo "exit $?"
printf 'val y: string;\n' > $D/s/b.gxi
echo "(1) sibling b with b.gxi:"; timeout -s KILL 20 $G --no-cache --check $D/s/main.gx 2>&1 | tail -1
printf 'mod p;\np::v\n' > $D/q/main.gx
printf 'let cell = never();\nlet v = 1;\nmod c;\n' > $D/q/p.gx
printf 'use super::cell;\nlet w = 2;\ncell <- w\n' > $D/q/p/c.gx
echo "(2) the book's p.gx + p/c.gx, no c.gxi:"; timeout -s KILL 20 $G --no-cache --check $D/q/main.gx 2>&1 | tail -1; echo "exit $?"
printf 'val w: i64;\n' > $D/q/p/c.gxi
echo "(2) with p/c.gxi:"; timeout -s KILL 20 $G --no-cache --check $D/q/main.gx 2>&1 | tail -1
