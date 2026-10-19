#!/bin/sh
# Title: `graphix --warm` runs the program's first cycle: its effects happen
# Command: sh shell-warm-runs-effects.sh
# Expected: --warm compiles, writes the cache entries and exits; nothing of
#   the program runs (no "SIDE EFFECT" line, no out.txt).
# Observed: "SIDE EFFECT" is printed and out.txt is written on every run
#   (cold and warm cache alike), exit 0.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
printf 'println("SIDE EFFECT");\nsys::fs::write_all(#path: "%s/out.txt", "written")\n' $D > $D/p.gx
XDG_CACHE_HOME=$D/cache timeout -s KILL 60 $G --warm $D/p.gx
echo "exit $?"
ls -l $D/out.txt
# Second witness: --warm also waits for the first cycle to finish. With a
# first cycle of 100000 node-walk activations, `--warm` takes ~10 s even
# when every entry is already in the cache (observed 10.2 s, twice).
printf 'let rec f = |n: i64| -> i64 select n { 0 => 0, n => f(n - 1) };\nf(100000)\n' > $D/slow.gx
for i in 1 2; do
  /usr/bin/time -f "%e s" env XDG_CACHE_HOME=$D/cache timeout -s KILL 120 $G --warm --no-fusion $D/slow.gx
done
