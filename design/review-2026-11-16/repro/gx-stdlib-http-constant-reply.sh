#!/bin/sh
# http::serve's doc says a request "is answered by that instance's first
# value"; a handler whose reply is a constant has a value but never fires, so
# every request hangs until the client gives up, with no error or log.
# (sys::net::rpc's doc says the same of #f.)
# Command: sh gx-stdlib-http-constant-reply.sh   (GRAPHIX = the graphix binary)
# Expected (per http.gxi): A: hi rc=0, B: hi rc=0
# Observed: A: rc=28 (curl timed out after 3 s), B: hi rc=0
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
F=$(mktemp --suffix .gx)
cat > "$F" <<'GX'
let s = http::serve(#addr: "127.0.0.1:38917", #handler: |req| {body: "hi", headers: [], status: u16:200});
let s2 = http::serve(#addr: "127.0.0.1:38918", #handler: |req| req ~ {body: "hi", headers: [], status: u16:200});
(s, s2)
GX
timeout -s KILL 8 "$GRAPHIX" --no-cache "$F" > /dev/null 2>&1 &
sleep 2
printf 'A: '; timeout -s KILL 4 curl -s -m 3 http://127.0.0.1:38917/; echo " rc=$?"
printf 'B: '; timeout -s KILL 4 curl -s -m 3 http://127.0.0.1:38918/; echo " rc=$?"
wait
rm -f "$F"
