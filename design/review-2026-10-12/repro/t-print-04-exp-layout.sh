#!/bin/sh
# Exponential layout: each nesting level of `let x = seq <unbreakable> {..}`
# is laid out twice by pretty_tail_bare (print.rs:321-332).
# Observed (debug binary): --indent 1: d=16 0.4s, d=20 5.8s, d=24 94s;
# --indent 2: d=20 6.3s, d=24 >100s.
for d in 16 20 24; do
  python3 "$(dirname "$0")/t-print-04-exp-layout.py" $d > /tmp/sq$d.gx
  time graphix fmt --indent 2 --stdout /tmp/sq$d.gx > /dev/null
done
