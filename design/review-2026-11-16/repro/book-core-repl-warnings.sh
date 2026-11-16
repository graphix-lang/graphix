#!/bin/sh
# Title: shell.md says the REPL "doesn't enable warnings by default"; only the unhandled-error one is off
# Command: sh design/review-2026-11-16/repro/book-core-repl-warnings.sh
# Expected (shell.md REPL Mode): no WARNING lines in the REPL.
# Observed: `a[15]?` gives no "will not be caught" warning (as the page means), but the until
#   input prints "WARNING ... this until reads only the step's entry snapshot ...".
G=/home/eric/tmp/review-2026-11-16/bin/graphix
printf 'let a = [1]\na[15]?\nlet ready = false\nlet go = 1\nseq go { until ready; 1 }\n' \
  | timeout -s KILL 30 $G --no-cache 2>&1 | grep -v '^Welcome\|^Press\|^$'
