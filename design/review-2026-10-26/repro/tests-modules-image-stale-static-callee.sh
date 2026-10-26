#!/bin/sh
# Title: a <- is refused naming a static call that a dynamic module's reload deleted
# Command: sh design/review-2026-10-26/repro/tests-modules-image-stale-static-callee.sh
# Expected: after `src <- "let v = 5"` reloads module d, nothing calls f, so
#   `f <- |x: i64| x + 100` compiles.
# Observed (HEAD e6ffc6eed, debug):
#   error: ... f cannot be written: an earlier compile calls it as a fixed function
#   at line: 1, column: 9: super::f(1)
#   CompileCtx::static_callees keeps the record after the calling body is deleted;
#   only Bind::delete of the callee removes an entry.
printf 'let f = |x: i64| x + 1\nlet src = "let v = super::f(1)"\nlet s = mod d dynamic { sandbox unrestricted; sig { val v: i64 }; source src }\nd::v\nsrc <- "let v = 5"\nd::v\nf <- |x: i64| x + 100\n' \
  | timeout -s KILL 20 ${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix} --no-cache 2>&1
