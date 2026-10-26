#!/bin/sh
# t-misc: GRAPHIX_EVAL_THREADS (stack::eval_threads) and GRAPHIX_PAR_T
# (cost.rs) ignore a value they cannot use without a word, unlike
# GRAPHIX_PAR / GRAPHIX_PRUNE / GRAPHIX_STACK_BUDGET / GRAPHIX_ACTIVATION_BUDGET,
# which report it (stack::env_mode).
# Command: sh t-misc-eval-threads-typo.sh
# Expected: a "GRAPHIX_EVAL_THREADS=... is not ..." line on stderr for each bad value.
# Observed: only GRAPHIX_PAR=forse is reported; abc, 0, -2 run silently with one worker a core.
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d); cd "$d" || exit 1
printf 'sys::exit(sys::time::after_idle(duration:50.ms, 0));\n1 + 1\n' > ev.gx
for v in abc 0 -2; do
  echo "== GRAPHIX_EVAL_THREADS=$v"
  GRAPHIX_EVAL_THREADS=$v GRAPHIX_PAR=force timeout -s KILL 30 "$GRAPHIX" --no-cache ev.gx
done
echo "== GRAPHIX_PAR_T=lots"
GRAPHIX_PAR_T=lots timeout -s KILL 30 "$GRAPHIX" --no-cache ev.gx
echo "== GRAPHIX_PAR=forse (reported)"
GRAPHIX_PAR=forse timeout -s KILL 30 "$GRAPHIX" --no-cache ev.gx
rm -rf "$d"
