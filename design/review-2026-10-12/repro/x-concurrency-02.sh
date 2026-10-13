#!/bin/sh
# x-concurrency-02: Sleeping arm aborts a TLS handshake: TCP socket destroyed,
# handshake never restarts.
#
# Run from the repo root:
#   sh design/review-2026-10-12/repro/x-concurrency-02.sh path/to/graphix
#
# The server's sys::tls::accept starts on the accepted socket at tick 0, its arm
# sleeps at tick 1 and wakes at tick 2, when the client starts its handshake.
# CachedArgsAsync::sleep aborts the in-flight eval, which had already taken the
# TCP stream out of its Halves, so the socket is closed; at the wake
# Started::take_new sees the same stream and starts nothing.
#
# Expected (as the control, where the arm never sleeps):
#   "client ok"
#   "server ok"
# Observed with the sleep:
#   "client error error:[\"TLSError\", \"TLS handshake failed: tls handshake eof\"]"
#   (and the server never answers)
G=${1:-graphix}
T=$(mktemp -d)
prog() {
cat <<GX
let cert = sys::fs::read_all_bin("stdlib/graphix-tests/certs/server.pem")\$;
let key = sys::fs::read_all_bin("stdlib/graphix-tests/certs/server.key")\$;
let ca = sys::fs::read_all_bin("stdlib/graphix-tests/certs/ca.pem")\$;
let listener = sys::tcp::listen("127.0.0.1:0")\$;
let addr = sys::tcp::listener_addr(listener)\$;
let client_tcp = sys::tcp::connect(addr)\$;
let server_tcp = sys::tcp::accept(listener, client_tcp)\$;
let tick = count(sys::time::timer(server_tcp ~ duration:100.ms, 3)\$);
let active = true;
active <- select tick { 1 => $1, 2 => true, _ => never() };
let server = select active {
    true => sys::tls::accept(#cert: cert, #key: key, server_tcp),
    false => never()
};
let go = select tick { 2 => client_tcp, _ => never() };
let client = sys::tls::connect(#ca_cert: ca, "127.0.0.1", go);
let s = select server { error as e => "server error [e]", _ => "server ok" };
let c = select client { error as e => "client error [e]", _ => "client ok" };
sys::exit(sys::time::after_idle(duration:1.s, tick ~ 0));
let out: string = never();
out <- s;
out <- c;
out
GX
}
prog false > "$T/sleep.gx"
prog true > "$T/control.gx"
echo "== arm sleeps at tick 1 =="
timeout -s KILL 30 "$G" --no-cache "$T/sleep.gx"
echo "== control: arm never sleeps =="
timeout -s KILL 30 "$G" --no-cache "$T/control.gx"
rm -rf "$T"
