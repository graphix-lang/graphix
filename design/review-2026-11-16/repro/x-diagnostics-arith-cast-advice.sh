#!/bin/bash
# Title: the arithmetic refusal advises "cast one side explicitly", which never makes the program pass
# Command: bash design/review-2026-11-16/repro/x-diagnostics-arith-cast-advice.sh
# Expected: following the advice (cast one side) passes the check, or the advice names what does
#   (`x$` for a nullable/Result operand, `cast<f64>(x)$` for mixed numerics, interpolation for strings).
# Observed: every "advised" program is refused again with the same advice:
#   cast<i64>(x) + 1     -> cannot compute [i64, Error<`InvalidCast(string)>] + i64 ... (cast one side explicitly)
#   cast<f64>(x) + y     -> cannot compute [f64, Error<`InvalidCast(string)>] + f64 ... (cast one side explicitly)
#   s + 1 (s: string)    -> ... (cast one side explicitly)   [the "interpolate" hint appears only for string + string]
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'let x: [i64, null] = 3;\ncast<i64>(x) + 1\n' > $d/a.gx
printf 'let x: i64 = 3;\nlet y: f64 = 1.0;\ncast<f64>(x) + y\n' > $d/b.gx
printf 'let s = "a";\ns + 1\n' > $d/c.gx
printf 'let x: i64 = 3;\nlet y: f64 = 1.0;\ncast<f64>(x)$ + y\n' > $d/ok.gx
for f in a b c ok; do echo "== $f"; timeout -s KILL 60 $G --check $d/$f.gx 2>&1 | tail -1; done
rm -rf $d
