#!/bin/bash
# x-recursion: in the REPL a budget stop ends the shell; the interrupt resets the session
#
# The book (functions/recursion.md) says a program that exceeds a budget "is
# stopped the way the interrupt key, Ctrl-\, stops it", and the REPL's
# interrupt replaces the runtime with a fresh one and keeps the shell. A
# budget stop instead closes the runtime's channel with a cause the shell
# does not treat as a reset (`Session::interrupted` is true for
# `StopCause::Interrupt` alone), so the session loop breaks with the error
# and the shell exits 1: the inputs after the runaway never run.
#
# command: bash design/review-2026-11-16/repro/x-recursion-repl-budget-ends-shell.sh
# expected: the runaway input is reported, the session is reset, and the
#   last input still answers (a fresh session has no `f`, so it reports
#   that), exit 0.
# observed:
#   -: i64
#   5
#   -: i64
#   Error: a runaway recursion exceeded the runtime's budget (GRAPHIX_STACK_BUDGET or GRAPHIX_ACTIVATION_BUDGET) and it stopped
#   exit=1      (and `1 + 1` is never read)
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
printf '%s\n' \
  'let rec f = |n: i64| -> i64 select n { 0 => 0, _ => 1 + f(n - 1) }' \
  'f(5)' \
  'let rec g = |n: i64| -> i64 1 + g(n + 1)' \
  'g(0)' \
  '1 + 1' \
  | GRAPHIX_ACTIVATION_BUDGET=20000 timeout -s KILL 30 "$G" --no-cache
echo "exit=$?"
