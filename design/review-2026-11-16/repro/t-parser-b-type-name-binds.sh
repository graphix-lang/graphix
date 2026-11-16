#!/bin/sh
# Title: a select arm whose whole pattern is a primitive type name (`i64 =>`,
#   `string =>`) binds a variable of that name over everything reaching the
#   arm instead of testing the type; the refusals that follow never say so.
#   patternexp::structure_pattern reads it as a Bind (fname admits type
#   names), and `pattern` only takes a type test when ` as` follows.
# Command: sh design/review-2026-11-16/repro/t-parser-b-type-name-binds.sh
# Expected: a refusal (or a note) at the arm naming the slip: "`i64` here
#   binds a name; a type test is `i64 as _`".
# Observed: 1: "unreachable arm" at the second arm; 2: "cannot compute
#   [i64, string] + i64" (the bind `i64` is the whole scrutinee); 3: checks,
#   and running it prints "a string: 1".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
printf 'let x: [i64, string] = 1;\nselect x { string => "s", i64 => "i" }\n' > $D/1.gx
printf 'let x: [i64, string] = 1;\nselect x { i64 => i64 + 1, _ => 0 }\n' > $D/2.gx
printf 'let x: [i64, string, null] = 1;\nselect x { null as _ => "null", string => "a string: [string]" }\n' > $D/3.gx
for f in 1 2 3; do
    echo "== $f"
    timeout -s KILL 30 $G --no-cache --check $D/$f.gx > $D/out 2>&1
    echo "check exit $?"; tail -1 $D/out
done
rm -rf $D
