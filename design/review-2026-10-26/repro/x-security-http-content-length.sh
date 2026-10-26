#!/bin/sh
# Title: an http::serve handler's own content-length header is passed to
# hyper beside a Full body of another length.
# Command: sh design/review-2026-10-26/repro/x-security-http-content-length.sh
# Expected: the response carries the body's real length (the handler's
#   content-length dropped or refused with a 500), both pipelined requests
#   answered.
# Observed (debug build): hyper's debug assertion panics the connection task
#   ("payload claims content-length of 11, custom content-length header
#   claims 1", hyper proto/h1/role.rs) and the client reads b''.
#   In a release build hyper trusts the header (role.rs: "we are just going
#   to trust that the values match"): it writes `content-length: 1` and then
#   11 body bytes, so the 10 extra bytes are read as the start of the next
#   response on the keep-alive connection (response splitting).
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
let s = http::serve(#addr: "127.0.0.1:18731", #handler: |req| {
    body: "hello world",
    headers: [("content-length", "1")],
    status: u16:200
});
s
GX
cat > $D/c.py <<'PY'
import socket, time
time.sleep(3)
s = socket.create_connection(("127.0.0.1", 18731), timeout=3)
s.sendall(b"GET /a HTTP/1.1\r\nHost: x\r\n\r\nGET /b HTTP/1.1\r\nHost: x\r\n\r\n")
time.sleep(1)
data = b""
try:
    while True:
        c = s.recv(65536)
        if not c: break
        data += c
except Exception:
    pass
print(repr(data))
PY
(timeout -s KILL 12 $G --no-cache $D/p.gx 2>&1 | grep -A1 panicked) &
timeout -s KILL 10 python3 -I $D/c.py
wait
rm -rf $D
