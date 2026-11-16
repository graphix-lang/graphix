#!/bin/sh
# Title: "impl Show for Counter is implemented twice" is placed at the .gxi's
#   declaration and names the duplicate by a bare line and column with no file,
#   which read in the frame's file (the .gxi) points at nothing.
# Command: sh design/review-2026-11-16/repro/tests-traits-a-implemented-twice-placed-at-gxi.sh
# Expected: the error placed at the second `impl` in m.gx, naming the first.
# Observed: "at line: 5, column: 1 in file .../m.gxi" then "impl Show for Counter is
#   implemented twice (at line: 7, column: 1)"; m.gxi has 6 lines, the duplicate
#   is m.gx line 7.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
printf 'mod m;\nm::Show::show(m::make(1))\n' > $d/main.gx
printf '// a\n// b\ntrait Show { val show: fn(self) -> string };\ntype Counter;\nimpl Show for Counter;\nval make: fn(x: i64) -> Counter\n' > $d/m.gxi
printf 'type Counter = Abstract<i64>;\n// x\n// y\n// z\nimpl Show for Counter { let show = |c| "one" };\n// w\nimpl Show for Counter { let show = |c| "two" };\nlet make = |x| Counter(x)\n' > $d/m.gx
timeout -s KILL 30 $G --no-cache --check $d/main.gx 2>&1 | tail -2
rm -rf $d
