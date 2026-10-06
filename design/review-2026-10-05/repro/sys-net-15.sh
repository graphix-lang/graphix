#!/usr/bin/env bash
# sys-net-15: NetidxResolver treats any .gxi fetch error as "module has no
# interface" (stdlib/graphix-package-sys/src/loader.rs:112, `intf_sub.ok()`),
# and any .gx fetch error as TryNextMethod (loader.rs:107-109), where
# FilesResolver returns Resolution::Broken for a module or interface that is
# there but cannot be read (graphix-types/src/expr/resolver.rs:280-288).
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/sys-net-15.sh
# (uses the default netidx config; with none, graphix uses the machine-local
# resolver on 127.0.0.1:59200 and starts it if it is not running)
#
# module bar: bar.gx = "let visible = 1; let hidden = 2", bar.gxi = "val visible: i64";
# the consumer is `mod bar; println("hidden = [bar::hidden]")`.
#
# expected:
#   good    (bar.gxi a valid interface)            -> bar::hidden not defined
#   nonstr  (bar.gxi published as i64 42)          -> a resolve error, as a
#           non-UTF-8 bar.gxi on disk gives ("could not resolve module bar")
#   dead    (bar.gxi's publisher exited; the resolver lists it until writer_ttl)
#                                                  -> a resolve error
#   implbad (bar.gx published as i64 42; GRAPHIX_MODPATH=netidx:..,file:fallback
#           where fallback/bar.gx has hidden = 200) -> a resolve error
# observed (HEAD c722befe, debug build):
#   good    -> bar::hidden not defined
#   nonstr  -> hidden = 2    (the interface was dropped silently)
#   dead    -> hidden = 2    (the interface was dropped silently)
#   implbad -> hidden = 200  (a later resolver's module stood in)
set -u
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
B=/local/review_sys_net_15/$$-$(date +%s)
mkdir -p "$D/consumer" "$D/fallback"
cat > "$D/pub_dead.gx" <<EOF
sys::net::publish("$B/dead/bar.gxi", "val visible: i64")\$;
sys::exit(sys::time::after_idle(duration: 1.s, 0))
EOF
cat > "$D/pub_live.gx" <<EOF
let body = "let visible = 1; let hidden = 2";
sys::net::publish("$B/good/bar.gx", body)\$;
sys::net::publish("$B/good/bar.gxi", "val visible: i64")\$;
sys::net::publish("$B/nonstr/bar.gx", body)\$;
sys::net::publish("$B/nonstr/bar.gxi", 42)\$;
sys::net::publish("$B/dead/bar.gx", body)\$;
sys::net::publish("$B/implbad/bar.gx", 42)\$;
println(sys::time::after_idle(duration: 1.s, "P2 ready"));
sys::exit(sys::time::after_idle(duration: 20.s, 0))
EOF
cat > "$D/consumer/main.gx" <<'EOF'
mod bar;
println("hidden = [bar::hidden]");
sys::exit(sys::time::after_idle(duration: 300.ms, 0))
EOF
echo 'let visible = 100; let hidden = 200' > "$D/fallback/bar.gx"
# P1 publishes the interface and exits without unpublishing
timeout -s KILL 30 "$G" --no-cache "$D/pub_dead.gx" > /dev/null 2>&1
timeout -s KILL 60 "$G" --no-cache "$D/pub_live.gx" > "$D/p2.out" 2>&1 &
P2=$!
for _ in $(seq 1 150); do grep -q 'P2 ready' "$D/p2.out" && break; sleep 0.2; done
run() {
  echo "== $1"
  GRAPHIX_MODPATH=$2 timeout -s KILL 30 "$G" --no-cache "$D/consumer/main.gx" 2>&1 | tail -3
}
run good netidx:$B/good
run nonstr netidx:$B/nonstr
run dead netidx:$B/dead
run implbad "netidx:$B/implbad,file:$D/fallback"
wait $P2
