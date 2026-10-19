#!/bin/sh
# t-print: cast<T>(..) never hugs its lone bracketed argument, unlike every other
# lone-argument form (f([, T({, `Tag([, never(, any(, rethrow().
# command: sh t-print-cast-hug.sh
# expected: let x = cast<Array<i64>>([\n    1111111111,\n ...\n]);
# observed: let x = cast<Array<i64>>(\n    [\n        1111111111, ...\n    ]\n);
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
printf 'let x = cast<Array<i64>>([1111111111, 2222222222, 3333333333, 44444444444]);\nx\n' > $d/a.gx
$G fmt --stdout --width 40 $d/a.gx
rm -rf $d
