#!/bin/sh
# Title: every runtime loop pass try_recv's every watch channel, so each
#        idle db subscription (one channel each) costs every cycle
# Command: sh design/review-2026-10-26/repro/x-concurrency-watch-sweep-per-cycle.sh
# Expected: 3000 cycles of a counter take about as long with 20000 idle db
#           subscriptions as with 20000 plain slots (nothing they read moves)
# Observed (HEAD e6ffc6eed, debug build): "watches 20000: 1.365334057s" vs
#           "plain 20000: 12763956.ns" and "watches 1: 12307620.ns":
#           about 450 us a cycle, ~22 ns per idle channel per cycle
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
run() {
cat > "$D/p.gx" <<GXEOF
let db = db::open("$D/db")\$;
let t: db::Tree<i64, i64> = db::tree(db, "t")\$;
let subs = array::init($2, |i| $3);
let go = sys::time::after_idle(duration:200.ms, array::len(subs));
let n: i64 = never();
n <- go ~ 0;
n <- select n { k if k < 3000 => k + 1, _ => never() };
let start = sys::time::now(go);
let fin = sys::time::now(select n { 3000 => n, _ => never() });
sys::exit(fin ~ 0);
sys::time::diff(fin, start)
GXEOF
echo "$1 $2: $(timeout -s KILL 170 "$GX" --no-cache "$D/p.gx" 2>/dev/null | tail -1)"
rm -rf "$D/db"
}
run watches 1 "db::subscription::new(#prefix: i, t)"
run watches 20000 "db::subscription::new(#prefix: i, t)"
run plain 20000 "(i, t)"
rm -rf "$D"
