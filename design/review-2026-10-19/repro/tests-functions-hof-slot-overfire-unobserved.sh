#!/bin/sh
# Title: lang::functions hof_slot_{callee_first_call,nested_prev_len,constant_fires}
#   cannot see a slot that raises more than once.
# Command: sh design/review-2026-10-19/repro/tests-functions-hof-slot-overfire-unobserved.sh
# The fixtures read `raised` once, at the cycle count(n) reaches 4. A catch
# handler's event fires at most once per cycle (later raises queue), so
# `raised` is 3 there whether each new slot raises once (the fixture) or
# every slot raises every cycle (the variant, whose callback reads n).
# Expected (if the tests could tell the two apart): the first line with
#   count 4 differs between the programs.
# Observed: both print (4, 3) first; only later values differ
#   (fixture settles at 4, variant climbs to 10).
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > $D/fixture.gx <<'GX'
let n = array::iter([1, 2, 3, 4]);
let raised = 0;
let out = {
  catch(e) raised <- e ~ raised + 1;
  array::map(array::init(n, |i| i), |x| {
    let e: [i64, Error<`E>] = error(`E);
    e? + x
  })
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
(count(n), raised)
GX
sed 's/= error(`E);/= select n { _ => error(`E) };/' $D/fixture.gx > $D/overfire.gx
for p in fixture overfire; do
  echo "== $p"
  timeout -s KILL 30 $G --no-cache $D/$p.gx 2>&1
done
rm -rf $D
