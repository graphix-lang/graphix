#!/bin/sh
# fuzz-main-aux-03: a reference in the watched value makes every comparison
# "nondeterministic"; the confirm ladder drops it silently.
# command: sh fuzz-main-aux-03.sh <path to graphix-fuzz>
# expected: AGREE (the i64 field is 6 in every mode; the reference names x)
# observed: `check` prints only "UNSURE — no divergence, but a retry, slowness,
#   containment or nondeterminism decided it (see above)" with nothing above;
#   `run` shows the reference as a raw bind id that differs on every run:
#   Interp u64:4611686018427389929, Jit ...932, cold ...935/...938, warm 8093/12142.
#   The ladder's rerun of the node-walk disagrees with itself, so Settled::Drop:
#   a JIT error in the i64 field would be dropped the same way, unrecorded.
FUZZ=${1:-graphix-fuzz}
P=$(mktemp --suffix=.gx)
cat > "$P" <<'GX'
{
    let x = i64:5;
    (&x, x + i64:1)
}
GX
timeout -s KILL 180 "$FUZZ" run "$P"
timeout -s KILL 180 "$FUZZ" check "$P"
rm -f "$P"
