#!/bin/sh
# A let whose value is a block ending in a lambda has no static callee,
# though one ending in a name of that lambda has (Bind::lambda_def_value
# sees through parentheses only; Bind::aliased_lambda sees through a
# block's final expression).
#
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-c-block-lambda-no-static-callee.sh
# Expected: every program prints 2 (each call's function is traceable).
# Observed: 1 and 3 print 2; 2 is refused "#[native] expression did not
#   fully fuse to native code: emit_clif: call `f(1)` has no static callee".
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
n=0
for p in \
  '{ let n = 1; let f = |x: i64| x + n; #[native] f(1) }' \
  '{ let f = { let n = 1; |x: i64| x + n }; #[native] f(1) }' \
  '{ let f = { let n = 1; let h = |x: i64| x + n; h }; #[native] f(1) }'
do
  n=$((n + 1))
  printf 'let result = %s;\nsys::exit(sys::time::after_idle(duration:50.ms, 0));\nresult\n' "$p" > "$d/p$n.gx"
  echo "== $n: $p"
  timeout -s KILL 60 "$G" --no-cache "$d/p$n.gx" 2>&1 | tail -2
done
rm -rf "$d"
