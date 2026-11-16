#!/bin/sh
# Title: a REPL line holding only a comment is compiled as an output
# expression (ExprKind::NoOp is not in is_output_kind's non-output list),
# so the shell prints its type and replaces the current display with it.
# Command:
#   sh rt-repl-comment-output.sh
# Expected: nothing printed for the comment line, the display of `x` left
#   standing.
# Observed (e1be2fbf2):
#   -: i64
#   1
#   -: _
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
printf 'let x = 1\nx\n// a note\n' | timeout -s KILL 15 "$GRAPHIX" --no-cache -n -i 2>&1 | tail -n +3
