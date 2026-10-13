#!/usr/bin/env bash
# http-sqlite-db1-10: HTTP client reads response bodies with no size limit
#
# A local server answers one GET with an endless chunked body; the program
# calls http::request_bin (http::request is the same, via resp.text()).
# Command: bash design/review-2026-10-12/repro/http-sqlite-db1-10.sh [graphix-binary]
# Expected: the request fails with a catchable `HTTPError("... body over ...")
#   once the body passes a bound (as http::serve does with #max_body).
# Observed: the client buffers the whole body (BodyKind::read ->
#   resp.bytes()); the process reached its 6 GB memory cap in ~3 s
#   ("[probe] OOM-KILLED", exit 251; server: "sent 6046 MiB").
set -u
GX=${1:-graphix}
D=$(mktemp -d)
cat > "$D/srv.py" <<'PY'
import socket, sys
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("127.0.0.1", int(sys.argv[1]))); s.listen(4)
chunk = b"a" * (1 << 20)
frame = (b"%x\r\n" % len(chunk)) + chunk + b"\r\n"
c, _ = s.accept(); c.recv(65536)
c.sendall(b"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n")
sent = 0
try:
    while True:
        c.sendall(frame); sent += len(chunk)
except Exception as e:
    print("server: sent %d MiB before %s" % (sent >> 20, e), file=sys.stderr)
PY
cat > "$D/prog.gx" <<'GX'
let client = http::default_client(null)$;
let resp = http::request_bin(client, "http://127.0.0.1:18731/")$;
resp.status
GX
timeout -s KILL 150 python3 -I "$D/srv.py" 18731 & SP=$!
sleep 0.5
timeout -s KILL 140 "$GX" --no-cache "$D/prog.gx"; echo "rc=$?"
kill -9 $SP 2>/dev/null; wait
rm -rf "$D"
