#!/bin/sh
# Title: a piped REPL ends at the first idle verdict, so a timer-gated
#        sys::exit among its lines never sets the status
# Command: sh design/review-2026-11-16/repro/shell-piped-timer-exit-lost.sh
# Expected (book/src/shell.md: the end of piped input ends the shell "once
#   what its lines started has run ... with the status a sys::exit among them
#   asked for"):
#     piped after_idle: 3
#     piped timer:      4
#     script after_idle: 3
# Observed (debug build at e1be2fbf2):
#     piped after_idle: 0
#     piped timer:      0
#     script after_idle: 3
# Output::process_ended breaks on GXHandle::wait_idle, and wait_idle reads a
# pending timer as idle, so the shell ends before the timer the line started.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
T=$(mktemp -d)
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 3))\n' \
  | timeout -s KILL 60 "$G" -i --no-cache >/dev/null 2>&1
echo "piped after_idle: $?"
printf 'let t = sys::time::timer(duration:300.ms, false)\nsys::exit(t ~ 4)\n' \
  | timeout -s KILL 60 "$G" -i --no-cache >/dev/null 2>&1
echo "piped timer:      $?"
printf 'sys::exit(sys::time::after_idle(duration:100.ms, 3))\n' > "$T/p.gx"
timeout -s KILL 60 "$G" --no-cache "$T/p.gx" >/dev/null 2>&1
echo "script after_idle: $?"
rm -rf "$T"
