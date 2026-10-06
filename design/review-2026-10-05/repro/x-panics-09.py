#!/usr/bin/env python3
# x-panics-09: http::serve awaits the TLS handshake inline in the accept
# loop, so one silent client blocks every other connection.
#
# serve_loop (stdlib/graphix-package-http/src/lib.rs:630) runs
# `acceptor.accept(stream).await` before it spawns the connection task and
# with no timeout. While that handshake waits, listener.accept() is not
# called: a peer that opens a TCP connection to the HTTPS port and sends
# nothing stalls every other client until it disconnects. The plain-HTTP
# path spawns at once, so the same silent client does not affect it (the
# control below).
#
# The script runs an http::serve program over the repo's test certs
# (stdlib/graphix-tests/certs) on 127.0.0.1:0, then: one request, open a
# raw TCP socket that sends nothing, two requests (5 s timeout each), close
# the socket, one request. Then the same against a plain-HTTP server.
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/x-panics-09.py <graphix>
#
# expected: every request answers 200 'hello GET' at once, both servers.
#
# observed (HEAD c722befe, debug build):
#   https server at 127.0.0.1:33649
#     before silent client:       OK 200 b'hello GET'   0.09s
#     while silent client open:   FAIL TimeoutError: _ssl.c:1064: The handshake operation timed out 5.01s
#     again, still open:          FAIL TimeoutError: _ssl.c:1064: The handshake operation timed out 5.00s
#     after silent client closed: OK 200 b'hello GET'   0.06s
#   http server at 127.0.0.1:38113
#     (all four requests)         OK 200 b'hello GET'   0.00s
#
# #addr is gated on (cert, key) because with them still bottom when #addr
# fires, http::serve starts a plain-HTTP listener first (lib.rs:785).
import http.client, os, re, signal, socket, ssl, subprocess, sys, tempfile, time
from pathlib import Path

GX = sys.argv[1] if len(sys.argv) > 1 else os.environ.get("GRAPHIX", "graphix")
CERTS = Path(__file__).resolve().parents[3] / "stdlib/graphix-tests/certs"

PROGRAM = """
let cert = sys::fs::read_all_bin("%(certs)s/server.pem")$;
let key = sys::fs::read_all_bin("%(certs)s/server.key")$;
let handler = |req: http::Request| {
    body: "hello [req.method]",
    headers: [],
    status: u16:200,
    url: ""
};
let server = http::serve(
    #addr: (cert, key) ~ "127.0.0.1:0",
    %(tls)s
    #handler: handler
)$;
http::server_addr(server)
"""


def run(tls, tmp):
    prog = Path(tmp) / ("serve_%s.gx" % ("tls" if tls else "plain"))
    prog.write_text(PROGRAM % {
        "certs": CERTS,
        "tls": "#cert: cert,\n    #key: key," if tls else "",
    })
    env = dict(os.environ, XDG_CACHE_HOME=str(Path(tmp) / "xdg"))
    proc = subprocess.Popen(
        ["timeout", "-s", "KILL", "50", GX, "--no-cache", str(prog)],
        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env, text=True)
    try:
        m = re.search(r"127\.0\.0\.1:(\d+)", proc.stdout.readline())
        if m is None:
            print("no address printed")
            return
        addr = ("127.0.0.1", int(m.group(1)))
        print("%s server at %s:%d" % ("https" if tls else "http", *addr))

        def request(label):
            start = time.time()
            try:
                if tls:
                    c = http.client.HTTPSConnection(
                        *addr, timeout=5, context=ssl._create_unverified_context())
                else:
                    c = http.client.HTTPConnection(*addr, timeout=5)
                c.request("GET", "/")
                r = c.getresponse()
                out = "OK %d %r" % (r.status, r.read())
                c.close()
            except Exception as e:
                out = "FAIL %s: %s" % (type(e).__name__, e)
            print("  %-27s %-55s %.2fs" % (label, out, time.time() - start))

        request("before silent client:")
        silent = socket.create_connection(addr)
        time.sleep(0.5)
        request("while silent client open:")
        request("again, still open:")
        silent.close()
        time.sleep(0.3)
        request("after silent client closed:")
    finally:
        # timeout(1) leads its own process group: this kills the shell too
        os.killpg(proc.pid, signal.SIGKILL)
        proc.wait()


with tempfile.TemporaryDirectory() as tmp:
    run(True, tmp)
    run(False, tmp)
