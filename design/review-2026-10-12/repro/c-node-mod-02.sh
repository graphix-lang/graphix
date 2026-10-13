#!/usr/bin/env bash
# c-node-mod-02: a banked `~` paid when its argument first arrives fires as
# WAKE_FIRED (Sample::update tags the immediate debt answer from the STALE
# trigger), so a sleeping arm's catch-up treats the payment as a wake's own
# fire and a `<-` target's let keeps its last write.
#
# run: GRAPHIX=<path to graphix> bash c-node-mod-02.sh
#
# Three variants of one program, differing only in `p`:
#   once     `let t = once(k); let p = t ~ v;`       one banked fire, paid on v's arrival
#   take2    `let t = take(#n: 2, k); let p = t ~ v;` two banked fires: one paid at once,
#                                                     one through the debt variable
#   direct   `let p = v;`                             v's arrival as a plain fire
# expected: every variant ends (4, 42) (5, 42) (6, 42): v's arrival at k=3 is a real
#           event the sleeping arm did not see, so the wake at k=4 catches q up to 42
# observed: once   -> (4, -1) (4, 7) (5, 7) (6, 7)    q keeps 7 forever
#           take2  -> (4, -1) (4, 42) (5, 42) (6, 42)
#           direct -> (4, 42) (5, 42) (6, 42) (7, 42)   (last four lines)
# Same with --no-fusion (`~` node-walks).
set -u
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
prog() {
  cat <<EOF
let clock = sys::time::timer(duration:100.ms, true);
let k = 0;
k <- clock ~ k + 1;
let v = never<i64>();
v <- select k { 3 => 42, _ => never() };
$1
let w = 0;
let on = true;
on <- select k { 2 => false, 4 => true, _ => never() };
sys::exit(select k { 7 => 0, _ => never() });
let r = select on {
  true => { let q = any(p, w); q <- select k { 1 => 7, _ => never() }; q },
  false => -1
};
(k ~ k, r)
EOF
}
prog 'let t = once(k); let p = t ~ v;' > "$D/once.gx"
prog 'let t = take(#n: 2, k); let p = t ~ v;' > "$D/take2.gx"
prog 'let p = v;' > "$D/direct.gx"
for f in once take2 direct; do
  echo "== $f"
  timeout -s KILL 30 "$G" --no-cache "$D/$f.gx" | tail -4
done
