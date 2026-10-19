#!/bin/sh
# rt: GXRt::ref_var finds a reader by a linear scan of the variable's reader
# list (graphix-rt/src/rt.rs, `readers.iter_mut().find`), and every
# collection slot is its own Reader (its own Prune boundary), so n slots that
# read one global cost O(n^2) to register.
# Command: sh design/review-2026-10-19/repro/rt-by-ref-linear-scan.sh
# Expected: the global-reading map costs about what the control does.
# Observed (debug build at 9445a1d21, --no-fusion):
#   global  80000: 4.8 s   160000: 16.5 s
#   control 80000: 1.2 s   160000:  2.7 s
BIN=${BIN:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
for body in "x + k" "x + 1"; do
  for n in 80000 160000; do
    cat > $D/r.gx <<G
let k = 1;
let a = array::init($n, |i| i);
let b = array::map(a, |x| $body);
sys::exit(sys::time::after_idle(duration:1.ms, array::len(b)))
G
    s=$(date +%s.%N)
    timeout -s KILL 170 $BIN --no-cache --no-fusion $D/r.gx >/dev/null
    e=$(date +%s.%N)
    echo "|x| $body  n=$n: $(echo "$e - $s" | bc) s"
  done
done
rm -rf $D
