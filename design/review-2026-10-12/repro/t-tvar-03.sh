#!/bin/sh
# t-tvar-03: settle witness copies the conjunct with reset_tvars, cutting it
# off from live cells it shares.
# (t-tvar-03.gx in this directory is a different finding's repro.)
#
# f(y) settles its return-only 'b eagerly to the witness of 'b: Array<'a>;
# TVar::witness returns c.reset_tvars(), so 'b := Array<'fresh> instead of
# Array<'a> with 'a = y's cell. g's signature becomes
# fn(i64) -> fn(v: Array<'fresh>) -> i64 and h(["a", "b"]) checks.
#
# Run: sh t-tvar-03.sh /path/to/graphix
# Expected: both --check and the run refuse h(["a", "b"]) with
#   type mismatch Array<i64> does not contain Array<string>
#   (as they do with `let g = |y: i64| f(y)`).
# Observed: --check exits 0; the run (and --expand) refuses with
#   an instance at fn(x: i64) -> (fn(v: Array<string>) -> i64) of a
#   definition typed fn<'b: Array<'a>>(x: 'a) -> (fn(v: 'b) -> 'a)
#   and graphix-fuzz check reports "DIVERGENCE - the check accepted what
#   the build refused".
G=${1:-graphix}
P=$(mktemp --suffix=.gx)
cat > "$P" <<'GX'
let f = 'b: Array<'a> |x: 'a| -> fn(v: 'b) -> 'a |v: 'b| select v { [h, ..] => h, _ => x };
let g = |y| {
  let k = f(y);
  let z: i64 = y;
  k
};
let h = g(1);
let r = h(["a", "b"]);
let n: i64 = r;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
n * 2
GX
echo "== --check"; timeout -s KILL 30 "$G" --no-cache --check "$P"; echo "exit $?"
echo "== run";     timeout -s KILL 30 "$G" --no-cache "$P"; echo "exit $?"
rm -f "$P"
