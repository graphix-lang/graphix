#!/bin/sh
# Title: writers erase kinds, so keys that differ only by kind collide: a map
# whose keys are a string and a bare variant (or a tuple and an array) loses an
# entry when written (rep::strip rebuilds the map from stripped keys), and a db
# tree keyed by [string, `A] stores "A" and `A under one key.
# Command: sh design/review-2026-10-19/repro/x-kind-markers-data-key-collision.sh
# Expected: (2, 2, 2, 2) then (null, 2, 1, ..) -- or the writers refuse the value
# Observed: (2, 1, 2, 1, "{\"A\":2}") then (1, 1, 2, ("A", 2)): the insert of `A
#           returned and replaced the value stored under "A"
d=$(mktemp -d)
cat > "$d/m.gx" <<'GX'
let m: Map<[string, `A], i64> = {"A" => 1, `A => 2};
let n: Map<[(i64, i64), Array<i64>], i64> = {(1, 2) => 1, [1, 2] => 2};
let p: Map<[string, `A], i64> = pack::read(pack::write_bytes(m)$)$;
let q: Map<[(i64, i64), Array<i64>], i64> = pack::read(pack::write_bytes(n)$)$;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(map::len(m), map::len(p), map::len(n), map::len(q), json::write_str(m)$)
GX
cat > "$d/db.gx" <<GX
type K = [string, \`A];
let x: K = "A";
let y: K = \`A;
let db = db::open("$d/c.db")\$;
let t: db::Tree<K, i64> = db::tree(db, null)\$;
let o1 = db::insert(t, x, 1)\$;
let o2 = db::insert(t, o1 ~ y, 2)\$;
let n = db::len(o2 ~ t)\$;
let g = db::get(o2 ~ t, x)\$;
let f = db::first(o2 ~ t)\$;
sys::exit(sys::time::after_idle(duration:200.ms, 0));
(o2, n, g, f)
GX
timeout -s KILL 30 /home/eric/tmp/review-2026-10-19/bin/graphix --no-cache "$d/m.gx"
timeout -s KILL 30 /home/eric/tmp/review-2026-10-19/bin/graphix --no-cache "$d/db.gx"
rm -rf "$d"
