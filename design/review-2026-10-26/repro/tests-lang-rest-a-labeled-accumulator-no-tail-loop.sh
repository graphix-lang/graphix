#!/bin/sh
# Title: a tail recursion with a labeled accumulator is no native loop:
# it keeps one activation per depth where the positional form keeps none.
# Command: sh tests-lang-rest-a-labeled-accumulator-no-tail-loop.sh
# Expected: both programs print 200000 under a 100000-activation budget,
#   as a stateless tail recursion is "one native loop".
# Observed: positional prints 200000; labeled prints "a runaway recursion
#   exceeded the runtime's budget ... and it stopped" (fused, 175 ms, but
#   200000 live activations; --no-fusion 4.9 s).
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > "$D/labeled.gx" <<'GX'
let r = {
  let rec f = |#acc: i64 = 0, n: i64| -> i64 select n { 0 => acc, _ => f(#acc: acc + 1, n - 1) };
  f(200000)
};
sys::exit(sys::time::after_idle(duration:10.ms, 0));
r
GX
cat > "$D/positional.gx" <<'GX'
let r = {
  let rec f = |acc: i64, n: i64| -> i64 select n { 0 => acc, _ => f(acc + 1, n - 1) };
  f(0, 200000)
};
sys::exit(sys::time::after_idle(duration:10.ms, 0));
r
GX
for p in labeled positional; do
  echo "== $p"
  GRAPHIX_ACTIVATION_BUDGET=100000 timeout -s KILL 60 "$G" --no-cache "$D/$p.gx" 2>&1 | tail -1
done
