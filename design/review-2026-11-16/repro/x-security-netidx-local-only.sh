#!/bin/sh
# Title: with no netidx config, graphix's netidx is whatever listens on
# 127.0.0.1:59200: the first process to hold the port is the resolver for
# every local user, anonymous, and the first graphix run to find it free
# leaves a resolver daemon running after it exits.
#
# Command: timeout -s KILL 60 sh design/review-2026-11-16/repro/x-security-netidx-local-only.sh
#   (GRAPHIX=<binary> overrides the graphix binary; the run uses ports
#   47913/47914, not 59200, and a scratch HOME so no netidx config is found)
# Expected: no shared, unauthenticated resolver chosen by whoever binds a
#   loopback port first; no process left running after the program exits.
# Observed:
#   1. a plain TCP listener (standing in for another user's process) on the
#      port receives graphix's netidx hello: "the fake resolver ... was
#      trusted by graphix". A real resolver there (another user's graphix
#      started one) serves this user's subscriptions, rpc calls and
#      GRAPHIX_MODPATH=netidx: modules, and sees every path they publish.
#   2. without a listener, graphix exits and a re-executed graphix with
#      NETIDX_LOCAL_ONLY_WE_ARE_THE_RESOLVER=true stays listening on the port
#      ("daemon left running"); this script then stops it.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
mkdir -p "$D/home"
cat > "$D/sub.gx" <<'GX'
let v: Result<string, [`SubscribeError(string), `InvalidCast(string)]> = sys::net::subscribe("/review/x");
sys::exit(sys::time::after_idle(duration:3.s, 0));
v
GX
cat > "$D/fake.py" <<'PY'
import os, socket, subprocess, sys
gx, prog, home, port = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])
l = socket.socket()
l.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
l.bind(("127.0.0.1", port))
l.listen(8)
l.settimeout(15)
env = dict(os.environ, HOME=home, XDG_CONFIG_HOME=home + "/.config",
           NETIDX_LOCAL_ONLY_RESOLVER_PORT=str(port), TMPDIR=home)
env.pop("NETIDX_CFG", None)
p = subprocess.Popen(["timeout", "-s", "KILL", "20", gx, "--no-cache", prog], env=env,
                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
try:
    c, peer = l.accept()
    c.settimeout(3)
    print("the fake resolver on 127.0.0.1:%d was trusted by graphix: hello %r"
          % (port, c.recv(64)[:12]))
    c.close()
except socket.timeout:
    print("no connection")
p.wait()
PY
timeout -s KILL 30 python3 -I "$D/fake.py" "$G" "$D/sub.gx" "$D/home" 47913
H="$D/home"
env HOME="$H" XDG_CONFIG_HOME="$H/.config" NETIDX_LOCAL_ONLY_RESOLVER_PORT=47914 TMPDIR="$H" \
  timeout -s KILL 20 "$G" --no-cache "$D/sub.gx" >/dev/null 2>&1
sleep 1
for pf in "$H"/netidx_resolver*/pid; do
  [ -f "$pf" ] || continue
  pid=$(cat "$pf")
  echo "daemon left running: pid $pid $(tr '\0' ' ' < /proc/$pid/cmdline)"
  kill "$pid"
done
sleep 1
rm -rf "$D"
