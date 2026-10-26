#!/bin/sh
# Title: a catch per activation makes deep recursion quadratic (handler-chain walks per install and per raise)
# Command: sh design/review-2026-10-26/repro/c-error-op-catch-depth-quadratic.sh
# Expected: time linear in depth, as without the catch (base: 360 / 550 / 1035 ms at 8k/16k/32k).
# Observed (debug binary, --no-fusion, noisy shared machine):
#   catch, no raise:        872 / 2361 / 7875 ms at 8k/16k/32k   (x3.3 per doubling)
#   catch + raise per level: 2619 / 8921 ms at 8k/16k
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for n in 8000 16000 32000; do
cat > $D/base.gx <<GX
let rec f = |n: i64| -> i64 select n { 0 => 0, _ => { let hit = n; hit + f(n - 1) } };
sys::exit(sys::time::after_idle(duration:10.ms, 0));
f($n)
GX
cat > $D/catch.gx <<GX
let rec f = |n: i64| -> i64 select n { 0 => 0, _ => { let hit = n; catch(e) println(e); hit + f(n - 1) } };
sys::exit(sys::time::after_idle(duration:10.ms, 0));
f($n)
GX
for p in base catch; do
  s=$(date +%s%N); timeout -s KILL 170 $G --no-cache --no-fusion $D/$p.gx >/dev/null 2>&1; e=$(date +%s%N)
  echo "$p n=$n $(( (e - s)/1000000 ))ms"
done; done
rm -rf $D
