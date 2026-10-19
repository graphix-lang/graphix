#!/bin/bash
# Title: a warm program image keeps a file module after netidx starts serving it
#   (NetidxResolver records no Probe when the module is absent from netidx).
# Command: bash design/review-2026-10-19/repro/sys-net-netidx-absent-probe.sh
#   (needs the netidx CLI at $N and graphix at $G; runs a local resolver on
#   127.0.0.1:47811 and kills it before exiting)
# Expected: run1 "file", run2 "netidx", run3 "netidx"
#   (GRAPHIX_MODPATH lists netidx:/mods BEFORE the file dir, so once /mods/foo.gx
#   is published it shadows files/foo.gx, cold or warm)
# Observed: run1 "file", run2 "file" (warm program entry), run3 "netidx" (--no-cache)
set -u
N=${N:-$HOME/tmp/target/debug/netidx}
G=${G:-$HOME/tmp/review-2026-10-19/bin/graphix}
W=$(mktemp -d)
mkdir -p $W/files $W/script $W/cache
echo 'let v = "file"' > $W/files/foo.gx
printf 'mod foo;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nfoo::v\n' > $W/script/main.gx
cat > $W/server.json <<'J'
{"parent": null, "children": [], "member_servers": [{"pid_file": "", "addr": "127.0.0.1:47811", "max_connections": 768, "hello_timeout": 10, "reader_ttl": 60, "writer_ttl": 120, "auth": "Anonymous"}], "perms": {}}
J
echo '{"addrs": [["127.0.0.1:47811", "Anonymous"]], "base": "/"}' > $W/client.json
cd $W
timeout -s KILL 90 $N resolver-server -f -c server.json > rs.log 2>&1 &
RS=$!
sleep 1
export GRAPHIX_MODPATH="netidx:/mods,$W/files" XDG_CACHE_HOME=$W/cache
echo -n "run1 (nothing on netidx): "; timeout -s KILL 30 $G --config client.json --auth anonymous script/main.gx 2>&1 | tail -n 1
( echo '/mods/foo.gx|string|let v = "netidx"'; sleep 40 ) | timeout -s KILL 50 $N publisher -c client.json -a anonymous > pub.log 2>&1 &
PB=$!
sleep 2
echo -n "run2 (netidx serves foo, cache on): "; timeout -s KILL 30 $G --config client.json --auth anonymous script/main.gx 2>&1 | tail -n 1
echo -n "run3 (netidx serves foo, --no-cache): "; timeout -s KILL 30 $G --no-cache --config client.json --auth anonymous script/main.gx 2>&1 | tail -n 1
kill -9 $PB $RS 2>/dev/null; wait 2>/dev/null
rm -rf $W
