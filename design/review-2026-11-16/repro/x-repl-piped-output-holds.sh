#!/bin/sh
# Title: piped, the value being shown still holds its static calls while the
#        line that replaces it compiles, so `g <- ..` is refused there while
#        a terminal (Ctrl-C clears the display first) accepts it.
# Command: sh design/review-2026-11-16/repro/x-repl-piped-output-holds.sh
# Expected: `g <- ...` replaces the display of `g(5)` and is accepted; `g(5)`
#           then shows 105, as in a terminal session (g(5), Ctrl-C, g <- ..).
# Observed: "g cannot be written: an earlier compile calls it as a fixed
#           function at line: 1, column: 1: g(5)" -- the call it names is the
#           display this very line replaces.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
export XDG_CACHE_HOME=$D/cache
(printf 'let g = |x: i64| -> i64 x + 1\ng(5)\n'; sleep 0.3
 printf 'g <- |x: i64| -> i64 x + 100\n'; sleep 0.3; printf 'g(5)\n'; sleep 0.3) |
  timeout -s KILL 30 "$G" --no-cache --no-init 2>&1
rm -rf $D
