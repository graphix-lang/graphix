#!/bin/sh
# Title: a warm image replays warnings in task-completion order, not the
#        program order the cold run printed them in
# Command: sh design/review-2026-10-26/repro/t-env-warning-replay-order.sh
# Expected: the warm start prints the warnings in the cold run's order:
#           a.gx a.gx b.gx b.gx c.gx c.gx d.gx d.gx
# Observed: cold prints program order; warm prints a different order each
#           run, e.g. "d.gx b.gx c.gx b.gx d.gx c.gx a.gx a.gx".
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
export XDG_CACHE_HOME=$D/cache
cat > $D/main.gx <<'GX'
mod a;
mod b;
mod c;
mod d;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
a::x + b::x + c::x + d::x
GX
for m in a b c d; do
  echo 'val x: i64' > $D/$m.gxi
  printf 'let x = 1;\nlet f = |b: bool| { let v = 0; select b { true => v <- 1, false => v <- 2 }; v };\nlet y = f(true)\n' > $D/$m.gx
done
order() { grep -o "WARNING at line: [0-9]*, column: [0-9]* [^:]*" | sed 's/.*\///' | tr '\n' ' '; echo; }
for i in 1 2 3; do
  rm -rf $D/cache
  printf 'cold: '; timeout -s KILL 60 $G $D/main.gx 2>&1 | order
  printf 'warm: '; timeout -s KILL 60 $G $D/main.gx 2>&1 | order
done
rm -rf $D
