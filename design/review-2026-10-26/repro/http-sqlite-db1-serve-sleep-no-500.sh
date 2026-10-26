#!/bin/sh
# Title: http::serve drops a held request's connection at the server's sleep
#        instead of answering 500 as http/mod.gxi's `serve` doc promises.
# Command: sh design/review-2026-10-26/repro/http-sqlite-db1-serve-sleep-no-500.sh
# Expected: the client's held GET is answered "HTTP/1.1 500 Internal Server Error"
#           when the arm holding the server sleeps at 1.5 s.
# Observed: "after 0.9s got b''" -- the connection is closed with no response
#           (HttpServe::sleep aborts every connection task via Conns::abort_all
#           before the dropped reply senders can produce the 500).
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/p.gx <<'GX'
let off = sys::time::timer(duration:1500.ms, false);
let state = true;
state <- off ~ false;
let r = select state {
  true => http::serve(#addr: "127.0.0.1:18431", #handler: |req: http::Request| {
    let t = sys::time::timer(duration:3.s, false);
    t ~ {body: "late", headers: [], status: u16:200}
  }),
  false => never()
};
sys::exit(sys::time::timer(duration:5.s, false) ~ 0);
r
GX
cat > $D/c.py <<'PY'
import socket, time
time.sleep(0.6)
s = socket.create_connection(("127.0.0.1", 18431)); s.settimeout(8)
s.sendall(b"GET / HTTP/1.1\r\nHost: x\r\n\r\n"); t0 = time.time()
d = s.recv(4096)[:60]; print("after %.1fs got %r" % (time.time() - t0, d))
PY
timeout -s KILL 20 $G --no-cache $D/p.gx > /dev/null 2>&1 &
timeout -s KILL 15 python3 -I $D/c.py
wait
