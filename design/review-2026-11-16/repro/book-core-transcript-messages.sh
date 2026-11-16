#!/bin/sh
# Title: REPL transcripts' refusal messages are not checked by the book gate, and two have drifted
# Command: sh design/review-2026-11-16/repro/book-core-transcript-messages.sh
# Expected (the pages): fundamental_types.md shows `0: at: line: 1, column: 2, in: i64:1` for `!1`;
#   polymorphism.md shows `'a must be Discernible, but [fn(x: i64) -> i64, fn(s: string) -> string]
#   holds ...` for `eq(v, v)`.
# Observed: `in: 1` (an i64 literal prints bare), and `[fn(x: i64) -> i64, fn(s: string) -> string]
#   must be Discernible here, but it holds ...`. examples_compile.rs Repl::transcript only asserts
#   that the input is refused, so neither drift fails the gate.
G=/home/eric/tmp/review-2026-11-16/bin/graphix
printf '!1\nlet eq = |x, y| x == y\nlet inc = |x: i64| x + 1\nlet v: [fn(x: i64) -> i64, fn(s: string) -> string] = inc\neq(v, v)\n' \
  | timeout -s KILL 30 $G --no-cache 2>&1 | grep -v '^Welcome\|^Press\|^$'
