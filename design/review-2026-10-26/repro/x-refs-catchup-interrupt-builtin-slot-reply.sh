#!/bin/bash
# An interrupt that skips a collection slot whose callee is a builtin loses the
# builtin's reply: GX::redeliver_unseen_replies counts a reply seen when its
# reader's boundary was visited, and a builtin slot has no boundary of its own
# (its reader registers under the enclosing statement's, which the cycle
# visited before the slot loop stopped on ctx.interrupted()).
# Drives the REPL (piped stdin, so SIGINT only interrupts the cycle): `go` and the
# fold slot's after_idle reply land in one cycle about half the time; `spin`
# wedges that cycle until the SIGINT.
# command: bash x-refs-catchup-interrupt-builtin-slot-reply.sh [CALLBACK]   (10 runs)
# expected: every run prints "fold 1.s" (before or after "go 1"), as the
#           lambda-wrapped callback '|acc, x| sys::time::after_idle(acc, x)' does
#           (13 runs: 12 printed it, 2 of them after the interrupt, redelivered)
# observed (debug binary, JIT): with the bare builtin `sys::time::after_idle`,
#           5 of 11 runs print only "go 1": the fold's reply never arrives.
CB=${1:-sys::time::after_idle}
B=${B:-/home/eric/tmp/review-2026-10-26/bin}
for i in $(seq 1 10); do
  ( echo 'let rec spin = |n: i64| -> i64 select n < 0 { true => n, false => spin(n + 1) }';
    echo "let go = sys::time::after_idle(duration:300.ms, 0); let r = spin(go); let v = array::fold([duration:1.s], duration:300.ms, $CB); println(\"fold [v]\"); let z = println(\"go [go ~ 1]\")";
    sleep 4 ) | timeout -s KILL 12 $B/graphix --no-cache > /tmp/rout.$$ 2>&1 &
  p=$!
  sleep 2
  pk=$(pgrep -f "^[/].*/real/graphix --no-cache" | head -1)
  [ -n "$pk" ] && kill -INT $pk
  wait $p
  grep -E "fold|go 1" /tmp/rout.$$ | tr '\n' ' '; echo
  rm -f /tmp/rout.$$
done
