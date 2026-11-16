#!/bin/sh
# Title: a seqq capture rides its last value through bottom; the same seq waits for it.
# Command: sh design/review-2026-11-16/repro/tests-seq-seqq-capture-rides-bottom.sh
# `x` is 1 at steps 4-5, bottom at 6-11, 2 from 12; runs are triggered at steps 1 and 8.
# Run 2 is triggered while `x` is bottom.
# Observed: seq  -> Trace([5:(1, 1) 13:(8, 2)])   run 2 waits for x and reads 2
#           seqq -> Trace([7:(1, 1) 11:(8, 1)])   run 2 reads the 1 x held before it went bottom
# seqq.rs `stale_capture` pins the second behaviour ([10, 20, 20]: request 3 reads 20 while x is
# bottom); seq.rs `seq_step_waits_for_an_absent_value_in_each_run` pins the first.
F=${F:-/home/eric/tmp/review-2026-11-16/bin/graphix-fuzz}
D=$(mktemp -d)
for kw in seq seqq; do
cat > $D/p.gx <<EOF
{
  let step = 0;
  step <- select step { s if s < 16 => s + 1, _ => never() };
  let go = select step { 1 | 8 => step, _ => never() };
  let x = select step { s if s < 4 => never(), s if s < 6 => 1, s if s < 12 => never(), _ => 2 };
  $kw go { let v = x; (go, v) }
}
EOF
echo "$kw: $(timeout -s KILL 120 $F run $D/p.gx 2>&1 | grep 'Interp/InLanguage:')"
done
rm -rf $D
