#!/bin/bash
# Title: an argument refused by a type the callee's ONE instance fixed elsewhere names neither the binding nor the fix
# Command: bash design/review-2026-11-16/repro/x-diagnostics-instance-arg-no-cause.sh
# Expected: the refusal says why the formal is i64 and what makes the program pass:
#   (a) `h` is one instance (a call's result, not a lambda): `let h = |x| mk(0)(x)` passes;
#   (b) f writes its parameter into `a`, which is i64: `let a: [i64, string] = 1` passes,
#       as the direct write `a <- "s"` already says ("declare a: [i64, string] where it is bound").
# Observed: both are "type mismatch i64 does not contain string" at the argument "s", nothing more.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'let mk = |z| |x| x;\nlet h = mk(0);\n(h(1), h("s"))\n' > $d/a.gx
printf 'let a = 1;\nlet f = |v| { a <- v; v };\nf("s");\na\n' > $d/b.gx
printf 'let mk = |z| |x| x;\nlet h = |x| mk(0)(x);\n(h(1), h("s"))\n' > $d/a_fixed.gx
printf 'let a: [i64, string] = 1;\nlet f = |v| { a <- v; v };\nf("s");\na\n' > $d/b_fixed.gx
for f in a b a_fixed b_fixed; do echo "== $f"; timeout -s KILL 60 $G --check $d/$f.gx 2>&1 | tail -1; done
rm -rf $d
