#!/bin/sh
# x-fix-types-13: interface matching compares ground unions as sets only when BOTH sides are
# Type::Set (matches.rs sig_matches_inner); a union of primitives normalizes to one multi-bit
# Type::Primitive, so `val g: fn(x: T) -> [T, null]` with `type T = [i64, string]` refuses an
# implementation written `-> [null, i64, string]` ("signature has [null, T], implementation has
# [i64, null, string]"), while `-> [null, T]` and the argument `x: [string, i64]` match.
# expected: --check exits 0. usage: sh x-fix-types-13.sh /path/to/graphix
G=${1:-graphix}
d=$(mktemp -d)
printf "type T = [i64, string];\nval g: fn(x: T) -> [T, null]\n" > $d/m.gxi
printf 'let g = |x: [string, i64]| -> [null, i64, string] x\n' > $d/m.gx
printf 'mod m;\nm::g("s")\n' > $d/main.gx
timeout -s KILL 30 $G --check $d/main.gx; echo "exit $?"
rm -rf $d
