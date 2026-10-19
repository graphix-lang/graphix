#!/bin/sh
# Title: two sibling modules whose interfaces declare `impl Cast for T;` and
#        whose implementations leave `to_value` to the representation panic
#        at the module-check join (debug) / lose one re-registration (release).
# Command: sh t-env-cast-repr-race.sh [path/to/graphix]
# Expected: "[3] [4]"
# Observed (debug build at 9445a1d21):
#   thread 'tokio-rt-worker' panicked at graphix-types/src/tracked.rs:198:13:
#   a fork's write to TraitId(..) would replace a write made since the fork
#   Error: loading initial modules ... channel closed
# With one module providing `to_value` (no re-registration) it prints "[3] [4]".
GX=${1:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > $D/main.gx <<'G'
mod a;
mod b;
let wa = json::write_str(a::mk(3))$;
let wb = json::write_str(b::mk(4))$;
"[wa] [wb]"
G
for m in a b; do
cat > $D/$m.gxi <<'G'
type Pos;
impl Cast for Pos;
val mk: fn(x: i64) -> Pos
G
cat > $D/$m.gx <<'G'
type Pos = Abstract<Array<i64>>;
impl Cast for Pos {
    let from_value = |v| select cast<Array<i64>>(v) {
        error as e => e,
        a => Pos(a)
    }
};
let mk = |x: i64| -> Pos Pos([x])
G
done
timeout -s KILL 30 $GX --no-cache $D/main.gx
rm -rf $D
