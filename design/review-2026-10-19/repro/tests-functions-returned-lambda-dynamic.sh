#!/bin/sh
# Title: lang::functions returned_lambda_resolves claims calls to a returned
#   lambda resolve statically; add1(2) dispatches dynamically.
# Command: sh design/review-2026-10-19/repro/tests-functions-returned-lambda-dynamic.sh
# Expected (per the test's comment): add1(2) resolves through bind_to_lambda (b2l=true).
# Observed: "RESOLVE add1(2) ... b2l=false"; the value 33 is the same either way,
#   so the test cannot tell.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > $D/t.gx <<'GX'
sys::exit(sys::time::after_idle(duration:100.ms, 0));
{
  let mk = |x: i64| |y: i64| x + y;
  let add1 = mk(i64:1);
  add1(i64:2) + mk(i64:10)(i64:20)
}
GX
GXDBG_RESOLVE=1 timeout -s KILL 30 $G --no-cache --no-fusion $D/t.gx 2>&1 | grep -E "^RESOLVE (mk|add1)|^33"
rm -rf $D
