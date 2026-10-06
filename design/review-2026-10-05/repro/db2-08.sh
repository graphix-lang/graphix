#!/usr/bin/env bash
# db2-08: a db subscription registers with sled asynchronously, so a write
# issued as soon as the Subscription value fires can miss it.
#
# DbSubscribe::update (stdlib/graphix-package-db/src/subscribe.rs:198-199)
# calls Tree::watch_prefix inside the task it spawns: the sled subscriber
# exists only once a tokio worker first polls that task, while the
# Subscription value fires in the same cycle (:218). sled delivers an
# event only to the subscribers registered when the write reserves its
# broadcast (sled 0.34 Subscribers::reserve), so an insert sampled on the
# Subscription can run on a blocking thread before the registration and
# its event is lost. The window opens when another tokio worker steals the
# task (the shell's multi-thread runtime); the run_with_tempdir tests
# (lib_tests/db.rs db_subscribe_*) run on a current_thread runtime, which
# polls the task before the insert's, so they pass.
#
# Each case runs its program RUNS times (default 60) over a fresh db and
# counts the runs where every insert completed but on_insert did not
# report every key.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/db2-08.sh
#
# expected: missed 0 in every case.
# observed (HEAD c722befe, debug build, 16 cores; totals over 3-4 runs of
# these programs, the rate varies with the run and the machine's load):
#   insert sampled on sub, same cycle:              missed 15/200
#   insert via key <- sub ~ "k" (the tests' form):  missed 11/220
#   8 inserts sampled on sub:                       missed 6/120 (the missed
#                                                   runs got 4, 1 and 3 of the
#                                                   8 keys: the registration
#                                                   landed mid-burst)
#   insert 20 ms after sub (control):               missed 0/200
# With TOKIO_WORKER_THREADS=1 the first case missed 0/40 (=2: 3/40);
# GRAPHIX_PAR=off and --no-fusion change nothing (3/50 each).
set -u
GRAPHIX=${GRAPHIX:-graphix}
RUNS=${RUNS:-60}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
head='let db = db::open("'"$dir"'/db")$;
let t: db::Tree<string, i64> = db::tree(db, null)$;
let sub = db::subscription::new(t);'
tail='let evs = db::subscription::on_insert(sub)$;
println("evs [evs]");
println("inserted [i1]");
sys::exit(sys::time::after_idle(duration:300.ms, i1 ~ 0))'
# case_ <name> <keys expected> <statements defining i1>
case_() {
    printf '%s\n' "$head" "$3" "$tail" > "$dir/p.gx"
    local missed=0 i out keys
    for i in $(seq 1 "$RUNS"); do
        rm -rf "$dir/db"
        out=$(timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/p.gx" 2>&1)
        keys=$(grep -o 'key: "[^"]*"' <<<"$out" | sort -u | wc -l)
        if grep -q '^inserted' <<<"$out" && [ "$keys" -lt "$2" ]; then
            missed=$((missed + 1))
        fi
    done
    echo "$1: missed $missed/$RUNS"
}
case_ "insert sampled on sub, same cycle" 1 'let i1 = db::insert(t, sub ~ "k", 1)$;'
case_ "insert via key <- sub ~ \"k\" (the tests' form)" 1 'let key = never();
key <- sub ~ "k";
let i1 = db::insert(t, key, 1)$;'
case_ "8 inserts sampled on sub" 8 'let i1 = (
    db::insert(t, sub ~ "k1", 1)$, db::insert(t, sub ~ "k2", 2)$,
    db::insert(t, sub ~ "k3", 3)$, db::insert(t, sub ~ "k4", 4)$,
    db::insert(t, sub ~ "k5", 5)$, db::insert(t, sub ~ "k6", 6)$,
    db::insert(t, sub ~ "k7", 7)$, db::insert(t, sub ~ "k8", 8)$
);'
case_ "insert 20 ms after sub (control)" 1 'let later = sys::time::timer(sub ~ duration:20.ms, false);
let i1 = db::insert(t, later ~ "k", 1)$;'
