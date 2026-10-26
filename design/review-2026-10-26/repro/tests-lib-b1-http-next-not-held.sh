#!/bin/sh
# Title: http_abandoned_request_then_good / http_raising_handler_then_good cannot
# fail when the first request's instance is never released: under the default
# connection limit the second request is served on its own connection while the
# first is still pending.
# Command: sh design/review-2026-10-26/repro/tests-lib-b1-http-next-not-held.sh
# Expected (for the tests to pin "once its client gives up, the next is served"):
#   part 1 prints nothing, because the second request waits on the first.
# Observed: part 1 prints "200 hi bob" ~0.4 s in, while the first request has 4.5 s
#   left on its 5 s timeout. Part 2 (#max_connections: 1, r2 after r1's 200 ms
#   timeout) also prints "200 hi bob": the strengthened fixture passes today.
BIN=${BIN:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/a.gx <<'GX'
let handler = |req: http::Request| {
    let text = select buffer::to_string_lossy(req.raw) { "" => never(), s => s };
    { body: "hi [text]", headers: [], status: u16:200 }
};
let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
let addr = http::server_addr(server);
let client = http::default_client(server)$;
let r1 = http::request(#timeout: duration:5.s, client, "http://[addr]/");
let r2 = http::request(#method: `POST, #body: "bob", sys::time::after_idle(duration:100.ms, client), "http://[addr]/")$;
sys::exit(sys::time::after_idle(duration:200.ms, r2 ~ 0));
"[r2.status] [r2.body]"
GX
cat > $D/b.gx <<'GX'
let handler = |req: http::Request| {
    let text = select buffer::to_string_lossy(req.raw) { "" => never(), s => s };
    { body: "hi [text]", headers: [], status: u16:200 }
};
let server = http::serve(#addr: "127.0.0.1:0", #max_connections: 1, #handler: handler)$;
let addr = http::server_addr(server);
let client = http::default_client(server)$;
let r1 = http::request(#timeout: duration:200.ms, client, "http://[addr]/");
let r2 = http::request(#timeout: duration:3.s, #method: `POST, #body: "bob", r1 ~ client, "http://[addr]/");
sys::exit(sys::time::after_idle(duration:200.ms, r2 ~ 0));
select r2 { error as e => "[e]", r => "[r.status] [r.body]" }
GX
echo "part 1 (default limit, r1 pending 5 s):"; timeout -s KILL 30 $BIN --no-cache $D/a.gx 2>/dev/null
echo "part 2 (#max_connections: 1):"; timeout -s KILL 30 $BIN --no-cache $D/b.gx 2>/dev/null
rm -rf $D
