#!/bin/bash
# Title: the REPL treats a `use` line as an output expression and stops reading input
# Command: bash design/review-2026-10-19/repro/book-core-repl-use-is-output.sh
# Expected: the `use` line prints nothing; the next line prints "-: i64" and "2"
#           (book/src/shell.md: "Use statements ... doesn't output").
# Observed: "-: _" for the use line, then nothing: the REPL waits on the use
#           "expression" until Ctrl-C, and `len([1, 2])` is never evaluated.
#           A `let`, `type`, `trait` or `catch` line does not do this.
printf 'use array::len\nlen([1, 2])\n' |
  timeout -s KILL 5 /home/eric/tmp/review-2026-10-19/bin/graphix --no-cache 2>&1
