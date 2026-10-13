#!/bin/sh
# c-callsite-04: REPL: a call statically resolved in an earlier input
# ignores a later `f <- ..`.
# Command: sh design/review-2026-10-12/repro/c-callsite-04.sh path/to/graphix
# Expected: (102, 102) -- what the same statements give as one script
#   (both calls see the written f with n = 2).
# Observed: (3, 102) -- `r = f(n)` was compiled in an earlier input,
#   resolved f statically (try_static_resolve guards only the current
#   batch's `<-` targets), so it keeps calling the old f. Same with
#   --no-fusion.
G=${1:-graphix}
(printf 'let f = |x| x + 1\n'; sleep 1
 printf 'let n = 1\n'; sleep 1
 printf 'let r = f(n)\n'; sleep 1
 printf 'f <- |x| x + 100\n'; sleep 1
 printf 'n <- 2\n'; sleep 1
 printf '(r, f(n))\n'; sleep 2) | timeout -s KILL 20 "$G" --no-cache 2>&1 | tail -1
