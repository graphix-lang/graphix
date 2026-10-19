#!/bin/sh
# Title: twins almost never re-enter the writing arm after it slept
# Command: sh fuzz-gen-b-twin-wake-rate.sh
# Expected: a healthy share of twin programs whose dispatch sequence puts
#   the writing arm to sleep (x = 0, the quiet arm) and then selects it
#   again, the sleeping-arm `&mut` write the twin oracle exists for.
# Observed (HEAD 9445a1d21, seed 21, 4000 reactive programs):
#   twins 600 quiet 424 with a 0 epoch 13 0 then nonzero (re-entry after sleep) 10
#   i.e. 1.7% of twins; epochs are drawn uniformly from -5..31.
B=/home/eric/tmp/review-2026-10-19/bin/graphix-fuzz
T=$(mktemp)
timeout -s KILL 60 $B gen 4000 21 --reactive > $T 2>/dev/null
timeout -s KILL 60 python3 -I - "$T" <<'PY'
import re, sys
progs = [p for p in open(sys.argv[1]).read().split("\n\n") if p.strip()]
tw = [p for p in progs if "TwinDiverged" in p]
quiet = [p for p in tw if "i64:0 => null" in p]
wake = anyzero = 0
for p in quiet:
    vals = [int(v) for v in re.findall(r"(?:in0|cx0)=i64:(-?\d+)", p.split("\n")[0])]
    z = [v == 0 for v in vals]
    anyzero += any(z)
    wake += any(z[i] and not all(z[i + 1:]) and i + 1 < len(z) for i in range(len(z)))
print("twins", len(tw), "quiet", len(quiet), "with a 0 epoch", anyzero,
      "0 then nonzero (re-entry after sleep)", wake)
PY
rm -f $T
