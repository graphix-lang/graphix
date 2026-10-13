#!/bin/sh
# x-engine-firing-03: a dynamic call in a never-selected arm misses its
# first wake catch-up.
# The arm `1 => k(0)` reaches in1 only through k's closure g. in1 fires
# (n=2) while the arm has never been selected; the arm is selected after
# n=3. The static twin `g(0)` catches the fire up (count = 1); the
# dynamic `k(0)` does not (count bottom: no output) until the SECOND
# selection (after n=6), when it gives 1 like the static one.
# The fuzz-schedule form of the same program is x-engine-firing-01.gx.
# run: sh x-engine-firing-03.sh [path/to/graphix]   (add --no-fusion: same)
# expected (both):  ... (3, -1) (3, 1) (4, 1) (4, -1) ... (6, 1) ...
# observed static:  (1, -1) (2, -1) (3, -1) (3, 1) (4, 1) (4, -1) (5, -1) (6, -1) (6, 1) ...
# observed dynamic: (1, -1) (2, -1) (3, -1) (4, -1) (5, -1) (6, -1) (6, 1) ...
# observed dynref:  same as static. `k(0) + in1 * 0` makes in1 a compile-time
#   ref of the arm, so TrackedFires tracks it from the start and the fresh
#   instance sees the catch-up: the tracker's refs are the whole cause.
GX=${1:-graphix}
D=$(mktemp -d)
for kind in static dyn dynref; do
  if [ $kind = static ]; then
    CALL='g(0)'; FARG=''; ARG=''
  elif [ $kind = dynref ]; then
    CALL='k(0) + (in1 * 0)'; FARG=', k: fn(x: i64) -> i64'; ARG=', select in2 {0 => g, _ => g}'
  else
    CALL='k(0)'; FARG=', k: fn(x: i64) -> i64'; ARG=', select in2 {0 => g, _ => g}'
  fi
  cat > $D/$kind.gx <<GX
let clk = sys::time::timer(duration:30.ms, true);
let n = count(clk);
let in0 = 0;
let in1 = 0;
let in2 = 0;
in1 <- filter(n, |n| (n == 2) || (n == 5)) ~ (n + 3);
in0 <- select n { 3 => 1, 4 => 0, 6 => 1, _ => never() };
in2 <- select n { 1 => 0, _ => never() };
let g = |x| count(in1) + x;
let f = |c$FARG| select c {1 => $CALL, _ => -1};
sys::exit(sys::time::after_idle(duration:300.ms, 0));
(n ~ n, f(in0$ARG))
GX
  echo "== $kind"
  timeout -s KILL 30 "$GX" --no-cache $D/$kind.gx 2>&1 | tr '\n' ' '
  echo
done
rm -rf $D
