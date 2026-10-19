#!/bin/sh
# Title: a db tree keyed by a variant union orders keys by their data form,
# not as `<` orders them: a bare variant (data: a string) sorts after every
# payload variant (data: an array), whatever the tags.
# Command: sh design/review-2026-10-19/repro/x-kind-markers-db-variant-key-order.sh
# Expected: (true, (`A, 1))      -- `A < `Z(1), and db::first is the least key
# Observed: (true, (`Z(1), 2))
d=$(mktemp -d)
cat > "$d/p.gx" <<GX
type K = [\`Z(i64), \`A];
let x: K = \`A;
let y: K = \`Z(1);
let db = db::open("$d/a.db")\$;
let t: db::Tree<K, i64> = db::tree(db, null)\$;
let o1 = db::insert(t, x, 1)\$;
let o2 = db::insert(t, o1 ~ y, 2)\$;
let f = db::first(o2 ~ t)\$;
sys::exit(sys::time::after_idle(duration:200.ms, 0));
(x < y, f)
GX
timeout -s KILL 30 /home/eric/tmp/review-2026-10-19/bin/graphix --no-cache "$d/p.gx"
rm -rf "$d"
