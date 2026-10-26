#!/bin/sh
# Title: comments ending an item's line are refused in map literals, any(..)/never(..)
#        arguments and impl bodies, though accepted in every other expression list
# Command: sh design/review-2026-10-26/repro/t-parser-a-list-eol-coverage.sh
# Expected: each program checks, as the same comment after a call argument, an array
#           element or a block statement does
# Observed: parse error, note "a comment stands on a line of its own, above what it describes"
B=${B:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf 'let m = {\n    1 => 2, // one\n    3 => 4\n};\nm\n' > $D/map.gx
printf 'let x = any(\n    1, // first\n    2\n);\nx\n' > $D/any.gx
printf 'let x: i64 = never(\n    1, // first\n    2\n);\nx\n' > $D/never.gx
printf 'type T = {a: i64};\nimpl Display for T {\n    let to_string = |x: T| -> string "T"; // the impl\n};\n"x"\n' > $D/impl.gx
printf 'let f = |#a = 1, b| a + b;\nf(\n    #a: 1, // first\n    2\n)\n' > $D/call_ok.gx
for f in map any never impl call_ok; do
  timeout -s KILL 30 $B --check $D/$f.gx > $D/out 2>&1; echo "$f exit $?"; grep note $D/out
done
rm -rf $D
