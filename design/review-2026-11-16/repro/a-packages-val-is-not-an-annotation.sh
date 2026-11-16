#!/bin/sh
# A `.gxi` val's type is not an annotation on its implementation: the author
# restates it on the `let`.
#
# command:  sh a-packages-val-is-not-an-annotation.sh /path/to/graphix
# expected: the first module checks: `val x: [i64, null]` over `let x = 3` is
#           what `let x: [i64, null] = 3` is, and `val f: fn(n: i64) -> [i64,
#           null]` over `let f = |n| n + 1` what `|n| -> [i64, null] n + 1` is
# observed: first:  val x is declared [i64, null] but implemented i64
#           second: val f is declared fn(n: i64) -> [i64, null] but implemented
#                   fn(n: i64) -> i64 (the parameter WAS taken from the val)
#           third:  rc 0, with both types written a second time in m.gx
G=${1:-graphix}
D=$(mktemp -d)
cd "$D" || exit 1
printf 'val x: [i64, null];\nval f: fn(n: i64) -> [i64, null];\n' > m.gxi
printf 'mod m;\nm::f(1)\n' > main.gx
echo "--- first: nothing restated"
printf 'let x = 3;\nlet f = |n| n + 1\n' > m.gx
timeout -s KILL 30 "$G" --no-cache --check main.gx; echo "rc $?"
echo "--- second: x restated"
printf 'let x: [i64, null] = 3;\nlet f = |n| n + 1\n' > m.gx
timeout -s KILL 30 "$G" --no-cache --check main.gx; echo "rc $?"
echo "--- third: both restated"
printf 'let x: [i64, null] = 3;\nlet f = |n| -> [i64, null] n + 1\n' > m.gx
timeout -s KILL 30 "$G" --no-cache --check main.gx; echo "rc $?"
rm -rf "$D"
