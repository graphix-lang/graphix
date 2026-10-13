#!/bin/bash
# t-format-resolver-01: NetidxResolver skips the layout refusals: a stray
# .gxi is silently dropped and two implementations pick one.
#
# Starts a private netidx resolver on a random localhost port, publishes a
# module tree under /lib, and checks + runs netidx:/lib/main.gx.
#   bash design/review-2026-10-12/repro/t-format-resolver-01.sh stray
#   bash design/review-2026-10-12/repro/t-format-resolver-01.sh both
#   bash design/review-2026-10-12/repro/t-format-resolver-01.sh ok    (control)
# GX=<graphix binary> overrides the binary; needs `netidx` on PATH.
#
# stray: /lib/m/mod.gx + /lib/m.gxi ("val pub_v: i64"), main reads m::secret.
#   expected: refused, "/lib/m.gxi is no interface: the module is
#             /lib/m/mod.gx ..." (what the file and VFS resolvers say)
#   observed: check exit 0, the run prints 1 (the interface was dropped,
#             m::secret is public)
# both: /lib/m.gx and /lib/m/mod.gx both published.
#   expected: refused, "... both define module m"
#   observed: check exit 0, the run prints "from m.gx"
# ok: /lib/m.gx + /lib/m.gxi: refused "m::secret not defined" (as it should).
set -u
W=$(mktemp -d)
PORT=$((40000 + RANDOM % 20000))
GX=${GX:-/tmp/claude-1000/-home-eric-proj-graphix/4ed578a5-7145-438d-b21b-e50e92617f4d/scratchpad/bin/graphix}
cat > $W/server.json <<J
{"parent":null,"children":[],"member_servers":[{"pid_file":"","addr":"127.0.0.1:$PORT","max_connections":768,"hello_timeout":10,"reader_ttl":60,"writer_ttl":120,"auth":"Anonymous"}],"perms":{}}
J
cat > $W/client.json <<J
{"addrs":[["127.0.0.1:$PORT","Anonymous"]],"base":"/"}
J
case "$1" in
  stray) DATA='/lib/main.gx|string|mod m; m::secret
/lib/m/mod.gx|string|let secret = 1; let pub_v = 2
/lib/m.gxi|string|val pub_v: i64' ;;
  both) DATA='/lib/main.gx|string|mod m; m::v
/lib/m.gx|string|let v = "from m.gx"
/lib/m/mod.gx|string|let v = "from m/mod.gx"' ;;
  ok) DATA='/lib/main.gx|string|mod m; m::secret
/lib/m.gx|string|let secret = 1; let pub_v = 2
/lib/m.gxi|string|val pub_v: i64' ;;
esac
timeout -s KILL 40 netidx resolver-server -f --config $W/server.json >$W/rs.log 2>&1 &
RS=$!
sleep 1
( echo "$DATA"; sleep 35 ) | timeout -s KILL 38 netidx publisher --config $W/client.json --bind local --auth anonymous >$W/pub.log 2>&1 &
PB=$!
sleep 2
env -u GRAPHIX_MODPATH XDG_CACHE_HOME=$W/cache timeout -s KILL 20 $GX --no-cache --config $W/client.json --auth anonymous --bind local --check netidx:/lib/main.gx
echo "check exit: $?"
env -u GRAPHIX_MODPATH XDG_CACHE_HOME=$W/cache timeout -s KILL 8 $GX --no-cache --config $W/client.json --auth anonymous --bind local netidx:/lib/main.gx
echo "run exit: $?"
kill -9 $RS $PB 2>/dev/null; pkill -9 -P $$ 2>/dev/null
wait 2>/dev/null
rm -rf $W
