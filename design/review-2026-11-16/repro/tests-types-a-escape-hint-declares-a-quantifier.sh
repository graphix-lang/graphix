#!/bin/sh
# Title: a write of a declared variable's value to an outer binding is
#   refused with a hint ("declare y: [i64, 'x] ... writing for 'x the type
#   of `x`") whose annotation is itself refused ("'a escapes the definition").
# Command: sh design/review-2026-11-16/repro/tests-types-a-escape-hint-declares-a-quantifier.sh
# Expected: the first refusal names a fix that passes: give y a type holding
#   every 'a (its bound, or Any), or declare no 'a, as the escape rule says.
# Observed (HEAD e1be2fbf2):
#   1: y is i64 and cannot hold 'a: unbound within Any; declare y: [i64, 'x]
#      where it is bound (line 1, column 7), writing for 'x the type of `x` ...
#   2 (the hint applied): 'a escapes the definition: ... give that a type
#      holding them all (the bound of 'a, or Any), or declare no 'a
#   3 (Any): accepted
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf '%s\n' "{ let y: i64 = 0; let f = 'a: Any |x: 'a| -> 'a { y <- x; x }; f(1) }" > $d/1.gx
printf '%s\n' "{ let y: [i64, 'x] = 0; let f = 'a: Any |x: 'a| -> 'a { y <- x; x }; f(1) }" > $d/2.gx
printf '%s\n' "{ let y: Any = 0; let f = 'a: Any |x: 'a| -> 'a { y <- x; x }; f(1) }" > $d/3.gx
for i in 1 2 3; do echo "== $i: $(cat $d/$i.gx)"; timeout -s KILL 30 $G --no-cache --check $d/$i.gx 2>&1 | grep -v ' at: ' | tail -1; done
rm -rf $d
