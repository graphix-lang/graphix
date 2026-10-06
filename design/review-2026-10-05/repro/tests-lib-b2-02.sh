#!/usr/bin/env bash
# tests-lib-b2-02: pack/toml stream fixtures race write_exact against
# shutdown on the same stream.
#
# pack_stream_tcp (stdlib/graphix-tests/src/lib_tests/pack.rs:69-70) and
# toml_stream_tcp (toml.rs:76-77) write `Write::write_exact(client, ..)?;
# Socket::shutdown(client)?;`. Both fire when `client` fires and each
# spawns a task (CachedArgsAsync -> rt.spawn_var). The two tasks contend for
# the stream's tokio Mutex (io.rs:286, tcp.rs:154), and nothing orders the
# shutdown after the write. run! uses `flavor = "current_thread"`
# (graphix-package-core/src/testing.rs:620-705), which polls tasks in spawn
# order, so the tests pass. The shell's multi-thread runtime polls them in
# either order: when the shutdown wins, write_all fails with EPIPE,
# read_all sees an empty stream and `msg.name` never arrives. This script
# runs each fixture body verbatim as a script (plus an exit gate) N times,
# and also the pack body with the shutdown sequenced on the write
# (`let written = Write::write_exact(..)?; Socket::shutdown(written ~
# client)?`) as the control.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/tests-lib-b2-02.sh 40
#
# expected: every run of every program prints "alice" (ok=40 bad=0).
#
# observed (HEAD c722befe, debug build):
#   pack: ok=35 bad=5 other=0 of 40
#   toml: ok=35 bad=5 other=0 of 40
#   pack_sequenced: ok=40 bad=0 other=0 of 40
# Across runs pack failed 4-8 of 40 and toml 3-12 of 40. The pack body run
# by hand with GRAPHIX_PAR=off --no-fusion (spawns in program order) failed
# 10 of 40: the order is the tokio scheduler's. Each bad run prints
#   unhandled error in file .../pack.gx at line: 9, column: 5
#     ["IOError", "write_exact failed: Broken pipe (os error 32)"]
# and then the read's error at line 11 instead of "alice".
set -u
GRAPHIX=${GRAPHIX:-graphix}
N=${1:-40}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

body() { # $1 = pack|toml, $2 = write statement, $3 = shutdown statement
    cat <<EOF
let r = {
    use sys::io::{Read, Write};
    use sys::tcp::Socket;
    type Msg = {age: i64, name: string};
    let listener = sys::tcp::listen("127.0.0.1:0")?;
    let addr = sys::tcp::listener_addr(listener)?;
    let client = sys::tcp::connect(addr)?;
    let server = sys::tcp::accept(listener, client)?;
    ${2:+$2 }Write::write_exact(client, $1::write_bytes({name: "alice", age: 30})?)?;
    Socket::shutdown($3client)?;
    let msg: Msg = $1::read(Read::read_all(server)?)?;
    msg.name
};
sys::exit(sys::time::after_idle(duration:500.ms, 0));
r
EOF
}

body pack "" "" > "$dir/pack.gx"
body toml "" "" > "$dir/toml.gx"
body pack "let written =" "written ~ " > "$dir/pack_sequenced.gx"

for prog in pack toml pack_sequenced; do
    ok=0 bad=0 other=0
    for _ in $(seq 1 "$N"); do
        out=$(timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/$prog.gx" 2>&1)
        if grep -q '"alice"' <<< "$out"; then
            ok=$((ok + 1))
        elif grep -q 'Broken pipe' <<< "$out"; then
            bad=$((bad + 1))
            [ "$bad" -eq 1 ] && grep 'unhandled error' <<< "$out"
        else
            other=$((other + 1))
        fi
    done
    echo "$prog: ok=$ok bad=$bad other=$other of $N"
done
