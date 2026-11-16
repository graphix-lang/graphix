#!/bin/sh
# Title: a REPL transcript's refusal is checked for refusing, never for its message
# Command: sh design/review-2026-11-16/repro/tests-shell-compiler-b-repl-refusal-text.sh
# The inputs are book/src/functions/polymorphism.md's `eq(v, v)` transcript.
# Expected (what the page shows, and what examples_compile's book_fences_check
# lets stand because Repl::transcript only asks result.is_err()):
#   error: ... 'a must be Discernible, but [fn(x: i64) -> i64, fn(s: string) -> string]
#   holds fn(x: i64) -> i64 and fn(s: string) -> string, which have the same runtime
#   form; wrap them in distinct variants
# Observed (the check today):
#   1: [fn(x: i64) -> i64, fn(s: string) -> string] must be Discernible here, but it
#      holds fn(x: i64) -> i64 and fn(s: string) -> string, which have the same
#      runtime form; wrap them in distinct variants
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
printf '%s\n' \
  'let eq = |x, y| x == y' \
  'let inc = |x: i64| x + 1' \
  'let v: [fn(x: i64) -> i64, fn(s: string) -> string] = inc' \
  'eq(v, v)' |
  timeout -s KILL 60 "$GX" --no-cache --no-netidx 2>&1 | grep -A6 'in expr eq'
