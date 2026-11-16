#!/bin/sh
# tests-parser-labeled-first: "labeled arguments come before positional ones"
# is refused at three parser sites (lambdaexp::apply_args, lambdaexp's lambda
# formals, typexp's fn type) with no pin at any of them, and none reports at
# the labeled item that follows a positional one: the call's caret stands on
# its `)` with the note one column past it, the lambda's on its closing `|`,
# the fn type's past its return type.
# Command: sh design/review-2026-11-16/repro/tests-parser-labeled-first.sh
# Expected: each refusal reports at `#a` (the first labeled item after a
#   positional one), leading with the reason.
# Observed: call -> column 11 `)`, note at column 12; lambda -> column 19 `|`;
#   fn type -> column 34, after `-> i64`.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
D=$(mktemp -d)
for src in 'let g = |#a = 1, x| x;
g(1, #a: 2)' 'let g = |x, #a = 1| x;
g(1)' 'let g: fn(x: i64, #a: i64) -> i64 = never();
0'; do
    printf '%s' "$src" > "$D/t.gx"
    echo "== $src"
    timeout -s KILL 30 "$G" --check --no-cache "$D/t.gx" 2>&1 | sed -n '5,12p'
done
rm -rf "$D"
