#!/bin/sh
# x-invalid-states-03: a seq abort opens a gap that a stale error delivery is counted against
#
# The machine Catch acknowledges raises by stepping `received` up to
# ErrorHandler::generation(), which both raise() and abort() advance.
# Run 1 raises 30 errors in one cycle (deliveries queue, one per cycle);
# the select arm sleeps mid-drain, so Catch::sleep's give_up_in_flight
# acknowledges all 30. After the wake a second run is aborted (n == 18)
# in a cycle where one of the old queued deliveries still arrives. The
# abort makes received != generation, so Catch::update counts the stale
# delivery as a raise: handled() decrements the enclosing handler's
# `nested` with no matching increment, and the manual abort fire then
# pushes `received` one past `generation`.
#
# usage: GRAPHIX=<debug graphix binary> sh x-invalid-states-03.sh
#
# Program A (enclosing catch). Expected: runs to n == 80 and exits.
# Observed (debug build, serial or forked, fused or --no-fusion):
#   panicked at graphix-compiler/src/lib.rs: unraised nested error
#   (ErrorHandler::handled <- Catch::update), at n == 18.
#
# Program B (no enclosing catch). Expected: the trigger at n == 40
# starts a run ("run 40 at 41"), the arm sleeps at n == 50 and wakes,
# and the program exits at n == 60 (it does when the 8..12 window is
# removed). Observed: no run after the abort (the action that idles the
# machine never runs: received > generation, so every trigger is
# dropped), and at n == 50 Catch::sleep's give_up_in_flight loop spins
# toward 2^64 iterations: the last line is "c 49" and the timeout kills
# it (exit 137).
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
cat > "$D/a.gx" <<'GX'
let n = 0;
n <- select n { x if x < 80 => x + 1, _ => never() };
let xs = array::init(30, |i| i);
let trig = filter(n, |x| (x == 3) || (x == 14));
let ab = filter(n, |x| x == 18);
let out = select n {
  x if (x >= 8) && (x < 12) => -1,
  _ => {
    catch(e) println("outer [n] [e]");
    seq let t = trig; abort(ab) {
      println("run [t] at [n]");
      let r = select t {
        3 => array::map(xs, |x| (x /? (x - x))?),
        _ => never<Array<i64>>()
      };
      array::len(r)
    }
  }
};
println("cycle [n]");
sys::exit(sys::time::after_idle(duration:300.ms, n));
out
GX
cat > "$D/b.gx" <<'GX'
let n = 0;
n <- select n { x if x < 60 => x + 1, _ => never() };
let xs = array::init(30, |i| i);
let trig = filter(n, |x| (x == 3) || (x == 14) || (x == 40));
let ab = filter(n, |x| x == 18);
let out = select n {
  x if ((x >= 8) && (x < 12)) || ((x >= 50) && (x < 52)) => -1,
  _ => seq let t = trig; abort(ab) {
    println("run [t] at [n]");
    let r = select t {
      3 => array::map(xs, |x| (x /? (x - x))?),
      40 => [1, 2],
      _ => never<Array<i64>>()
    };
    array::len(r)
  }
};
println("c [n]");
println("cycle [n] [out]");
sys::exit(sys::time::after_idle(duration:300.ms, n));
out
GX
echo "== A"; GRAPHIX_PAR=off timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/a.gx" 2>&1 | grep -E "^run|unraised|^cycle 1[78]"
echo "== B"; GRAPHIX_PAR=off timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/b.gx" 2>&1 | grep -E "^run|^c 49|^c 5[0-9]"
rm -rf "$D"
