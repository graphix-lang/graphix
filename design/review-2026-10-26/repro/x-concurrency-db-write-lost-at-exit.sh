#!/bin/sh
# Title: a db insert followed by sys::exit is lost: the runtime's teardown,
#        where the database's last handle drops and flushes (Reaped), is
#        not waited for before the process exits
# Command: sh design/review-2026-10-26/repro/x-concurrency-db-write-lost-at-exit.sh
# Expected: "after exit: 42" (the insert completed and the program ended
#           normally; dropping the database's last handle flushes it)
# Observed (HEAD e6ffc6eed, debug build): "after exit: null"
#           in 5 of 6 runs (the drop races the exit). With a 1.5 s wait before the exit (sled's own periodic flush)
#           the read gives 42.
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > "$D/w.gx" <<GXEOF
let db = db::open("$D/db")\$;
let t: db::Tree<string, i64> = db::tree(db, "t")\$;
let r = db::insert(t, "k", 42)\$;
sys::exit(r ~ 0);
r
GXEOF
cat > "$D/r.gx" <<GXEOF
let db = db::open("$D/db")\$;
let t: db::Tree<string, i64> = db::tree(db, "t")\$;
let r = db::get(t, "k")\$;
sys::exit(sys::time::after_idle(duration:200.ms, r ~ 0));
r
GXEOF
timeout -s KILL 30 "$GX" --no-cache "$D/w.gx" >/dev/null 2>&1
echo "after exit: $(timeout -s KILL 30 "$GX" --no-cache "$D/r.gx" 2>/dev/null | tail -1)"
rm -rf "$D"
