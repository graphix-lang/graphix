#!/bin/bash
# sys-io: after an arm's sleep, a connect still in flight holds the site's next connect
# Command: bash sys-io-connect-held-after-sleep.sh [path/to/graphix]
# A localhost listener with a full accept queue drops SYNs, so a connect to
# it hangs (the OS gives up after ~127 s). The arm holding the connect sleeps
# at 300 ms and wakes at 600 ms with the address of a live listener.
# Expected: ("connected", ~600 ms) shortly after the wake, as when the first
#   address is the live one (replace "127.0.0.1:PORT" by `good`).
# Observed: nothing for 60 s (killed, exit 137). TcpConnectEv is not
#   CANCEL_SAFE, so the slept connect is not aborted, and the Fifo site's next
#   call waits for it (CachedArgsAsync::waits_on_old), though its reply is
#   dropped unanswered when it comes.
GX=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
DIR=$(mktemp -d)
trap 'kill $PY 2>/dev/null; rm -rf "$DIR"' EXIT
cat > "$DIR/blackhole.py" <<'PY'
import socket, sys, time
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("127.0.0.1", 0)); s.listen(0)
port = s.getsockname()[1]
fill = []
for _ in range(4):
    c = socket.socket(); c.setblocking(False)
    try: c.connect(("127.0.0.1", port))
    except BlockingIOError: pass
    fill.append(c)
time.sleep(0.2)
print(port, flush=True)
time.sleep(float(sys.argv[1]))
PY
timeout -s KILL 100 python3 -I "$DIR/blackhole.py" 90 > "$DIR/port" &
PY=$!
sleep 0.5
cat > "$DIR/p.gx" <<GX
let l = sys::tcp::listen("127.0.0.1:0")\$;
let good = sys::tcp::listener_addr(l)\$;
let mode: [\`A, \`B] = \`A;
mode <- sys::time::timer(duration:300.ms, false) ~ \`B;
let back = sys::time::timer(duration:600.ms, false);
mode <- back ~ \`A;
let addr = "127.0.0.1:$(cat "$DIR/port")";
addr <- back ~ good;
let c = select mode { \`A => sys::tcp::connect(addr), \`B => never() };
let t0 = sys::time::now(l);
let shown = select c { error as e => "[e]", _ => "connected" };
sys::exit(sys::time::after_idle(duration:3.s, shown) ~ 0);
(shown, sys::time::diff(sys::time::now(shown), t0))
GX
SECONDS=0
timeout -s KILL 60 "$GX" --no-cache "$DIR/p.gx"
echo "exit=$? elapsed=${SECONDS}s"
