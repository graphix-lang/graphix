#!/bin/sh
# Title: two sibling modules whose interfaces declare `impl Cast for T;` and
#   whose implementations leave the methods out race on the global `impls`
#   registry: check_sig_items unregisters/re-registers the declared impl
#   inside each module's parallel check task, and the joins collide.
# Command: sh design/review-2026-10-19/repro/c-module-traits-sibling-cast-narrowing.sh
# Expected: prints "1 2" (json of each representation) and keeps running.
# Observed (debug build at 9445a1d21):
#   thread 'tokio-rt-worker' panicked at graphix-types/src/tracked.rs:198:13:
#   a fork's write to TraitId(...) would replace a write made since the fork
#   Error: loading initial modules / channel closed
# (A release build has no debug_assert: the later join silently restores the
#  other module's un-narrowed declared impl.)
set -e
BIN=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cd "$D"
cat > main.gx <<'G'
mod a;
mod b;
"[json::write_str(a::make(1))] [json::write_str(b::make(2))]"
G
cat > a.gxi <<'G'
type T;
impl Cast for T;
val make: fn(x: i64) -> T
G
cat > a.gx <<'G'
type T = Abstract<i64>;
impl Cast for T {};
let make = |x: i64| -> T T(x)
G
sed 's/T/U/g' a.gxi > b.gxi
sed 's/T/U/g' a.gx > b.gx
timeout -s KILL 30 "$BIN" --no-cache main.gx || true
rm -rf "$D"
