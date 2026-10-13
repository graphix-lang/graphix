#!/bin/sh
# x-engine-firing-01: an outer arm's wake republishes a `let` WAKE-tagged
# (its constant fires, its input is stale); in the same cycle the inner
# select re-selects the arm that slept through that let's REAL fire.
# TrackedFires::deliver sees the input delivered live (triggers()) and
# spends the pending FIRED bit without upgrading the tag, so the `<-`
# target `t` treats the value as the wake's own and keeps its last write.
#
# run: GRAPHIX_FUZZ=graphix-fuzz sh x-engine-firing-01.sh
# expected (all three): last epoch [0:i64:7] (or 14 for `a + a`): v's 0 -> 7
#   fire at e3 was seen by no selected reader, so the woken arm catches it up.
# observed (both engines, every session):
#   sleep  (outer arm sleeps at e4, wakes at e5): ... [0:i64:-1]; [0:i64:99]   <- event lost
#   awake  (outer arm stays selected):            ... [0:i64:-5]; [0:i64:7]
#   sleep, `let v = a + a` (no constant in v):    ... [0:i64:-1]; [0:i64:14]
FUZZ=${GRAPHIX_FUZZ:-graphix-fuzz}
D=$(mktemp -d)
body() {
cat <<GX
{
let f = |c, a, d, e| select c {
  i64:0 => {
    let v = $1;
    select d {
      i64:0 => i64:-5,
      _ => {
        let t = v + i64:0;
        t <- e ~ i64:99;
        t
      }
    }
  },
  _ => i64:-1
};
f(in0, in1, in3, in2)
}
GX
}
{ echo '// schedule-v1: cap=64 events=512; in3=i64:1 in2=i64:0; in3=i64:0; in1=i64:7; in0=i64:1; in0=i64:0 in3=i64:1'; body 'a * i64:1'; } > $D/sleep.gx
{ echo '// schedule-v1: cap=64 events=512; in3=i64:1 in2=i64:0; in3=i64:0; in1=i64:7; in0=i64:0; in3=i64:1'; body 'a * i64:1'; } > $D/awake.gx
{ echo '// schedule-v1: cap=64 events=512; in3=i64:1 in2=i64:0; in3=i64:0; in1=i64:7; in0=i64:1; in0=i64:0 in3=i64:1'; body 'a + a'; } > $D/sleep_noconst.gx
for p in sleep awake sleep_noconst; do
  echo "== $p"
  timeout -s KILL 180 $FUZZ run $D/$p.gx 2>&1 | grep -E '^(Interp|Jit)/InLanguage:'
done
rm -rf $D
