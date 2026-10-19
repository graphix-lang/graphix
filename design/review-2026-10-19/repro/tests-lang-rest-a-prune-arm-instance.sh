#!/bin/sh
# Title: prune.rs pins only block-statement skipping; arm and instance boundaries go unpinned
# Command: sh tests-lang-rest-a-prune-arm-instance.sh [path/to/graphix]
# Each program holds 20000 idle map slots behind a boundary the ticking counter DOES
# walk every tick: a select arm whose scrutinee fires each tick (arm), and a function
# instance re-dispatched each tick (instance). Pruned, the ticks skip the slots; unpruned
# they walk them all. prune.rs's idle_map puts its slots in a statement that reads
# nothing ticking, so the statement-level skip hides the inner boundaries and a change
# that stopped arm or instance boundaries from skipping would pass it.
# Observed (debug build, 300 ticks): arm on 0.78s / off 4.5s; instance on 0.73s / off 3.9s.
G=${1:-/home/eric/tmp/review-2026-10-19/bin/graphix}
T=$(mktemp -d)
cat > $T/arm.gx <<'GX'
let n = 0;
select n { x if x < 300 => n <- (x ~ n) + 1, _ => never() };
let m = select n >= 0 { true => array::map(array::init(20000, |i| i), |x| once(x) * 2), false => [] };
sys::exit(n ~ select n { 300 => 0, _ => never() });
(n, array::len(m))
GX
cat > $T/instance.gx <<'GX'
let n = 0;
select n { x if x < 300 => n <- (x ~ n) + 1, _ => never() };
let g = |k: i64| { let m = array::map(array::init(20000, |i| i), |x| once(x) * 2); (k, array::len(m)) };
sys::exit(n ~ select n { 300 => 0, _ => never() });
g(n)
GX
for f in arm instance; do for p in on off; do
  s=$(date +%s.%N)
  GRAPHIX_PRUNE=$p timeout -s KILL 170 $G --no-cache --no-fusion $T/$f.gx | tail -1
  e=$(date +%s.%N)
  echo "$f prune=$p $(echo "$e - $s" | bc)s"
done; done
rm -rf $T
