#!/bin/sh
# tests-types-03: reading a packed Graphix abstract value fails until the
# process has minted one (GxAbstract's AbstractWrapper registers lazily on
# the first abstract_value::wrap).
# Run: sh design/review-2026-10-12/repro/tests-types-03.sh <path to graphix>
# Both programs read the same bytes (what `pack::write_bytes(Box(5))` prints)
# 50 ms after start; the first mints a Box beforehand, the second does not.
# Expected: both print "accepted: 5".
# Observed: the first prints (1, "accepted: 5"); the second prints
#   "refused: error:[\"InvalidCast\", \"can't cast abstract:J1oM... to Box: not this abstract type\"]"
GX=${1:-graphix}
D=$(mktemp -d)
cat > "$D/warm.gx" <<'GX'
type Box = Abstract<i64>;
let warm = Box(1);
let t = sys::time::after_idle(duration:50.ms, null);
let r: Result<Box, [`PackErr(string), `InvalidCast(string)]> = pack::read(t ~ bytes:GydaDDF+kktOYbgtbxPJpH0IgVt4u3Lm5Q8DQm94AAYAAAAAAAAABQ==);
sys::exit(sys::time::after_idle(duration:200.ms, 0));
(warm.0, select r { error as e => "refused: [e]", b => "accepted: [b.0]" })
GX
cat > "$D/cold.gx" <<'GX'
type Box = Abstract<i64>;
let t = sys::time::after_idle(duration:50.ms, null);
let r: Result<Box, [`PackErr(string), `InvalidCast(string)]> = pack::read(t ~ bytes:GydaDDF+kktOYbgtbxPJpH0IgVt4u3Lm5Q8DQm94AAYAAAAAAAAABQ==);
sys::exit(sys::time::after_idle(duration:200.ms, 0));
select r { error as e => "refused: [e]", b => "accepted: [b.0]" }
GX
timeout -s KILL 60 "$GX" --no-cache "$D/warm.gx"
timeout -s KILL 60 "$GX" --no-cache "$D/cold.gx"
rm -rf "$D"
