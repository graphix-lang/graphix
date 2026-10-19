#!/bin/sh
# c-lambda: a lambda literal in a substituted instance loses its ParamCalls,
# so a builtin handed it is judged by the literal's own facts alone.
#
# Command: sh design/review-2026-10-19/repro/c-lambda-instance-param-calls.sh
# Expected: all four runs refuse/accept alike: `g`'s callback calls `f`, an
#   async function, through `filter`, so `h` is async: #[async] accepted,
#   #[sync] refused, with or without GRAPHIX_NO_SUBST.
# Observed: with instance substitution (the default) `#[async]` is refused
#   ("this function is sync") and `#[sync]` is accepted; GRAPHIX_NO_SUBST=1
#   gives the right verdicts, as does `filter(v, f)` in place of the literal.
B=${B:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
for attr in async sync; do
cat > $d/p.gx <<GX
{
  let g = |v: i64, f: fn(x: i64) -> bool| filter(v, |x| f(x));
  #[$attr]
  let h = |v: i64| g(v, |x| sys::time::after_idle(duration:1.ms, x) > 0);
  h(1)
}
GX
echo "== #[$attr], substituted instances"
timeout -s KILL 30 $B --no-cache --expand $d/p.gx 2>&1 | grep -E "#\[$attr\]:" || echo "accepted"
echo "== #[$attr], GRAPHIX_NO_SUBST=1"
GRAPHIX_NO_SUBST=1 timeout -s KILL 30 $B --no-cache --expand $d/p.gx 2>&1 | grep -E "#\[$attr\]:" || echo "accepted"
done
rm -rf $d
