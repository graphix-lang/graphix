#!/bin/sh
# x-security-05: HTTP client has no default timeout, and its Fifo call site
# lets one hung server block every later request.
# command: timeout -s KILL 60 sh design/review-2026-10-12/repro/x-security-05.sh <path-to-graphix>
# A python listener accepts on 127.0.0.1:38945 and never answers. The program
# GETs it through http::default_client, then 1 s later changes `url` to a
# refused port, which alone fails in milliseconds.
# expected: an `error: ... /refused` line about 1 s in (and, with a default
#           timeout, eventually one for /hang).
# observed: only the two url lines; no reply for 12 s, until the program exits.
#           With `http::request(#timeout: duration:3.s, c, url)` both errors
#           arrive together at 3.6 s: the refused request waited in the
#           site's Fifo queue behind the hung one.
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/p.gx" <<'GX'
let c = http::default_client(null)$;
let url = "http://127.0.0.1:38945/hang";
url <- sys::time::timer(duration:1.s, false) ~ "http://127.0.0.1:1/refused";
let r = http::get(c, url);
let s = select r { error as e => "error: [e]", v => "ok: [v.status]" };
println(s);
sys::exit(sys::time::timer(duration:12.s, false) ~ 0);
url
GX
timeout -s KILL 30 python3 -I -c "
import socket,time
s=socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(('127.0.0.1',38945)); s.listen()
c,_=s.accept(); print('listener: accepted, never answering', flush=True); time.sleep(20)" &
PY=$!
sleep 0.3
timeout -s KILL 30 "$G" --no-cache "$D/p.gx"
kill $PY 2>/dev/null; wait
rm -rf "$D"
