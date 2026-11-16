#!/bin/sh
# Title: a piped REPL line that is not UTF-8 ends the whole session.
# Command: sh design/review-2026-11-16/repro/x-repl-non-utf8-line.sh
# Expected: the bad line is reported as that input's error and `x + 1` then
#           shows `-: i64` / `2` (exit 0).
# Observed: "Error: stream did not contain valid UTF-8", exit 1; every binding
#           is lost and the later lines are never read.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
export XDG_CACHE_HOME=$D/cache
(printf 'let x = 1\n'; sleep 0.3; printf '"\377\376"\n'; sleep 0.3; printf 'x + 1\n'; sleep 0.5) |
  timeout -s KILL 30 "$G" --no-cache --no-init
echo "exit=$?"
rm -rf $D
