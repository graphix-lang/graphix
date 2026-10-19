#!/bin/sh
# Title: a map literal's effects run keys-then-values in the node-walk, written order in the JIT
# Command: sh c-data-map-map-effect-order.sh   (GRAPHIX=path to the graphix binary)
# Expected: both engines print dbg(x), dbg(x + 1), dbg(x + 2), dbg(x + 3) in one order
#   (the written order k1, v1, k2, v2)
# Observed: the JIT (fused) prints 10, 11, 12, 13; --no-fusion prints 10, 12, 11, 13
#   (Map::n holds every key and then every value, and node::gather updates in that order;
#   emit_map_new_node emits k1, v1, k2, v2)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
P=$(mktemp --suffix=.gx)
cat > "$P" <<'GX'
let f = |x: i64| {dbg(x) => dbg(x + 1), dbg(x + 2) => dbg(x + 3)};
let r = f(10);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
GX
echo "--- jit"; timeout -s KILL 30 "$G" --no-cache "$P" 2>&1 | grep dbg
echo "--- node-walk"; timeout -s KILL 30 "$G" --no-cache --no-fusion "$P" 2>&1 | grep dbg
rm -f "$P"
