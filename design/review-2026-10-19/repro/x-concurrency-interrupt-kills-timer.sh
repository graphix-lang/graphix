#!/bin/bash
# x-concurrency: an interrupt (Ctrl-C in the REPL, GXHandle::interrupt) that lands in
# the cycle delivering a timer's fire kills the timer for good when the timer lives
# inside a function: GXLambda::update returns before its body when interrupted, the
# Timer never sees its reply in ctx.event, keeps the spent id armed-in-name and never
# re-arms. At top level (no lambda dispatch to skip) the timer survives.
#
# Command: design/review-2026-10-19/repro/x-concurrency-interrupt-kills-timer.sh yes
#          design/review-2026-10-19/repro/x-concurrency-interrupt-kills-timer.sh no
#   (GRAPHIX=<binary> to override the graphix binary; run each a few times: the
#    timer inside `f` and the `go` timer must land in one cycle, which they do about
#    half the time since both are armed in one cycle with one period)
# Expected (both): after the interrupt `cnt` keeps counting: "1 2 3 ... 11"
# Observed at 9445a1d21:
#   yes: "0" in about half the runs (8 of 16) -- the repeating timer inside f is
#        dead; the other runs are the ones where the two timers fired in
#        different cycles
#   no:  "2 3 4 ... 12" in every run (8 of 8)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
mkfifo $D/in
if [ "$1" = yes ]; then
  TK='let f = |x: i64| { let t = sys::time::timer(duration:500.ms, true); t ~ x }; let tk = f(1)'
else
  TK='let tk = sys::time::timer(duration:500.ms, true)'
fi
descendants() { local c; for c in $(pgrep -P $1); do echo $c; descendants $c; done; }
timeout -s KILL 7 $G --no-cache < $D/in 2>&1 &
TO=$!
(
  echo 'let rec spin = |n: i64| -> i64 select n < 0 { true => n, false => spin(n + 1) }'
  echo 'let cnt = 0'
  # `go` and the timer in `tk` are armed in one cycle and fire in one cycle; `r`
  # spins in that cycle until interrupted, before `tk`'s root runs
  echo "let go = sys::time::timer(duration:500.ms, false); let r = spin(go ~ 0); $TK; cnt <- tk ~ cnt + 1"
  sleep 1.5
  P=$(for q in $(descendants $TO); do [ "$(cat /proc/$q/comm 2>/dev/null)" = graphix ] && echo $q; done | head -1)
  kill -INT $P
  sleep 0.5
  echo 'cnt'
  sleep 5
) > $D/in
wait $TO 2>/dev/null
rm -rf $D
