#!/bin/bash
# Title: sys::net::list's poll aborts the listing in flight, so a listing slower
#   than its #update period never answers (default period 1 s).
# Command: bash design/review-2026-10-19/repro/sys-net-list-poll-starves.sh
#   (local resolver on 127.0.0.1:47821, a python proxy on 47822 that delays
#   each direction by 0.3 s; everything is killed before exit)
# Expected: through the proxy both listings answer: (n >= 1, 1)
# Observed: through the proxy (0, 1): the default-polled list never answers in
#   11 s, while the same listing on a 30 s trigger does; direct: (1, 1).
set -u
N=${N:-$HOME/tmp/target/debug/netidx}
G=${G:-$HOME/tmp/review-2026-10-19/bin/graphix}
HERE=$(cd $(dirname $0) && pwd)
W=$(mktemp -d); cd $W
echo '{"parent": null, "children": [], "member_servers": [{"pid_file": "", "addr": "127.0.0.1:47821", "max_connections": 768, "hello_timeout": 30, "reader_ttl": 60, "writer_ttl": 120, "auth": "Anonymous"}], "perms": {}}' > server.json
echo '{"addrs": [["127.0.0.1:47821", "Anonymous"]], "base": "/"}' > direct.json
echo '{"addrs": [["127.0.0.1:47822", "Anonymous"]], "base": "/"}' > proxied.json
cat > ls.gx <<'GX'
let poll = sys::net::list("/slow");
let rare = sys::net::list(#update: sys::time::timer(duration:30.s, true), "/slow");
let np = 0;
np <- poll ~ np + 1;
let nr = 0;
nr <- rare ~ nr + 1;
sys::exit(sys::time::after_idle(duration:12.s, 0));
sys::time::timer(duration:11.s, false) ~ (np, nr)
GX
timeout -s KILL 60 $N resolver-server -f -c server.json > rs.log 2>&1 &
RS=$!
timeout -s KILL 60 python3 -I $HERE/sys-net-list-poll-starves-delayproxy.py 47822 47821 0.3 > px.log 2>&1 &
PX=$!
sleep 1
( echo '/slow/a|i64|1'; sleep 30 ) | timeout -s KILL 35 $N publisher -c direct.json -a anonymous > pub.log 2>&1 &
PB=$!
sleep 2
echo -n "delay 0.3 s each way: "; timeout -s KILL 30 $G --no-cache --config proxied.json --auth anonymous ls.gx 2>&1 | tail -n 1
echo -n "direct: "; timeout -s KILL 30 $G --no-cache --config direct.json --auth anonymous ls.gx 2>&1 | tail -n 1
kill -9 $PB $PX $RS 2>/dev/null; wait 2>/dev/null
rm -rf $W
