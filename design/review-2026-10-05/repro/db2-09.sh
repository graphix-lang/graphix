#!/usr/bin/env bash
# db2-09: db::txn::commit is not durable although the book promises ACID
# transactions.
#
# The Commit arm of TxnCtx::run (stdlib/graphix-package-db/src/txn.rs:207)
# returns Ok(()) without TransactionalTree::flush(), so sled's
# flush_if_configured does nothing: the commit sits in sled's in-memory log
# buffer until the periodic flusher (500 ms) or an explicit db::flush. That
# flusher takes sled's process-wide concurrency lock, which an open
# db::txn holds for its whole life (the sled closure blocks on the command
# channel), so while another transaction is open the commit does not reach
# disk at all. sys::exit is std::process::exit: sled's Drop never flushes.
# book/src/stdlib/db.md:4 and :130 say "ACID transactions".
#
# Each case runs a writer program, then a fresh process that reads key "k".
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/db2-09.sh
#
# expected: k = 42 after every case whose commit returned null.
# observed (HEAD c722befe, debug build):
#   commit, exit at once:                      committed: null -> k = null
#   commit, db::flush, exit:                   committed: null -> k = 42
#   commit, wait 1.5 s, exit:                  committed: null -> k = 42
#   commit, second txn open for 3 s, exit:     committed: null -> k = null
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
head='let db = db::open("'"$dir"'/db")$;
let t: db::Tree<string, i64> = db::tree(db, "t")$;'
commit='let txn = db::txn::begin(t ~ db)$;
let tt: db::txn::TxnTree<string, i64> = db::txn::tree(txn, txn ~ "t")$;
let c = db::txn::commit(db::txn::insert(tt, "k", 42)$ ~ txn);
println(c ~ "committed: [c]");'
printf '%s\n' "$head" 'let v = db::get(t, "k")$;' 'println(v ~ "  after restart, k = [v]");' \
    'sys::exit(sys::time::after_idle(duration:200.ms, v ~ 0))' > "$dir/read.gx"
case_() {
    rm -rf "$dir/db"
    echo "== $1"
    printf '%s\n' "$head" "$commit" "$2" > "$dir/write.gx"
    timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/write.gx"
    timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/read.gx"
}
case_ "commit, exit at once" 'sys::exit(c ~ 0)'
case_ "commit, db::flush, exit" 'sys::exit(db::flush(c ~ db) ~ 0)'
case_ "commit, wait 1.5 s, exit" 'sys::exit(sys::time::after_idle(duration:1500.ms, c ~ 0))'
case_ "commit, second txn open for 3 s, exit" 'let txn2 = db::txn::begin(c ~ db)$;
let tt2: db::txn::TxnTree<string, i64> = db::txn::tree(txn2, txn2 ~ "t")$;
let open = db::txn::insert(tt2, "x", 1)$;
sys::exit(sys::time::after_idle(duration:3.s, open ~ 0))'
