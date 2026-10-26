#!/bin/sh
# f-select: a kernel runs an unreached arm's guard, and its `$` logs a
# swallowed error the node-walk never had.
# Command: sh design/review-2026-10-26/repro/f-select-unreached-guard-logs.sh
# Expected: both modes log 0 "ignored error" lines (the `0 =>` arm claims
#   every value, so the guard `(10 /? d)$ > 1` never runs).
# Observed: --no-fusion logs 0; the JIT logs 4 (one per cycle).
BIN=/home/eric/tmp/review-2026-10-26/bin/graphix
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
let step = 0;
step <- select step { s if s < 3 => s + 1, _ => never() };
let f = |x: i64, d: i64| select x {
  0 => "zero",
  _ if (10 /? d)$ > 1 => "big",
  _ => "other"
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
f(0, step - step)
GX
for m in --no-fusion ""; do
  rm -rf $D/ld; mkdir $D/ld
  RUST_LOG=warn timeout -s KILL 30 $BIN --no-cache $m --log-dir $D/ld $D/p.gx >/dev/null
  echo "mode[$m] ignored-error logs: $(cat $D/ld/graphix.log 2>/dev/null | grep -c 'ignored error')"
done
rm -rf $D
