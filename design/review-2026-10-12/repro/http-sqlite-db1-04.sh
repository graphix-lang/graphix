#!/bin/bash
# http-sqlite-db1-04: HTTP server has no header-read or idle timeout; an idle
# connection holds its #max_connections permit forever (slowloris).
#
# Run: bash design/review-2026-10-12/repro/http-sqlite-db1-04.sh [path/to/graphix]
#
# A server with #max_connections: 1; a raw TCP client connects at 0.5 s and
# sends nothing; a GET is issued at 33 s (past hyper's 30 s default
# header_read_timeout, which is off because http1::Builder has no timer).
# Expected: the idle socket is closed by ~30 s and the GET answers "ok".
# Observed: no `resp=` line at all; the idle socket is closed only when the
# program exits at 38 s ("idle: server closed/sent after 37.6s: b''").
# Control (CONTROL=1, no idle client, GET at 3 s): resp={body: "ok", ...}.
set -u
GX=${1:-graphix}
D=$(mktemp -d)
cat > "$D/prog.gx" <<'GX'
let server = http::serve(
    #addr: "127.0.0.1:38941",
    #max_connections: 1,
    #handler: |req| {body: "ok", headers: [], status: u16:200, url: ""}
)$;
let client = http::default_client(server)$;
let go = sys::time::timer(duration:33.s, false);
let resp = http::get(client, go ~ "http://127.0.0.1:38941/");
println("resp=[resp]");
let done = sys::time::timer(duration:38.s, false);
sys::exit(done ~ 0);
resp
GX
cat > "$D/idle.py" <<'PY'
import socket, time
time.sleep(0.5)
s = socket.create_connection(("127.0.0.1", 38941))
t0 = time.time()
s.settimeout(45)
try:
    d = s.recv(1)
    print(f"idle: server closed/sent after {time.time()-t0:.1f}s: {d!r}", flush=True)
except Exception as e:
    print(f"idle: no close after {time.time()-t0:.1f}s ({e})", flush=True)
PY
if [ -n "${CONTROL:-}" ]; then
    sed -i 's/33\.s/3.s/; s/38\.s/5.s/' "$D/prog.gx"
    timeout -s KILL 30 "$GX" --no-cache "$D/prog.gx"
else
    timeout -s KILL 50 python3 -I "$D/idle.py" & P=$!
    timeout -s KILL 60 "$GX" --no-cache "$D/prog.gx"
    wait $P
fi
rm -rf "$D"
