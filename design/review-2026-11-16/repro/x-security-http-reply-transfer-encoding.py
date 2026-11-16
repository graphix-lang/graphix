# Title: http::serve drops a handler's content-length but passes its
# transfer-encoding to hyper, which then frames the response from it.
#
# Command: timeout -s KILL 60 python3 -I design/review-2026-11-16/repro/x-security-http-reply-transfer-encoding.py
#   (GRAPHIX=<binary> overrides the graphix binary)
# Expected: every response framed by the server alone, as `/plain` is
#   (`content-length: N`), whatever framing header the handler's Reply holds.
# Observed:
#   /chunked  transfer-encoding: chunked                 (chunked framing)
#   /gzip     transfer-encoding: gzip, chunked
#             connection: close, chunked                 (hyper appends to both)
#   /two      transfer-encoding: chunked, chunked        (chunked applied twice:
#                                                         RFC 9112 forbids it, and
#                                                         intermediaries disagree
#                                                         on how to frame it)
import os, re, shutil, socket, subprocess, sys, tempfile

gx = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
prog = """
let handler = |req: http::Request| {
    body: "hello [req.path]",
    headers: select req.path {
        "/chunked" => [("transfer-encoding", "chunked")],
        "/gzip" => [("transfer-encoding", "gzip")],
        "/two" => [("transfer-encoding", "chunked"), ("transfer-encoding", "chunked")],
        _ => []
    },
    status: u16:200
};
let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
http::server_addr(server)
"""
d = tempfile.mkdtemp()
path = os.path.join(d, "server.gx")
open(path, "w").write(prog)
p = subprocess.Popen(["timeout", "-s", "KILL", "40", gx, "--no-cache", path],
                     stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
addr = None
while True:
    line = p.stdout.readline()
    if not line:
        break
    m = re.search(r"127\.0\.0\.1:(\d+)", line)
    if m:
        addr = ("127.0.0.1", int(m.group(1)))
        break
for path in ["/plain", "/chunked", "/gzip", "/two"]:
    s = socket.create_connection(addr)
    s.settimeout(3)
    s.sendall(("GET %s HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" % path).encode())
    buf = b""
    try:
        while True:
            c = s.recv(4096)
            if not c:
                break
            buf += c
    except socket.timeout:
        buf += b"<timeout>"
    head = buf.split(b"\r\n\r\n", 1)[0]
    print(path, [h for h in head.split(b"\r\n") if not h.startswith(b"date:")], flush=True)
    s.close()
p.kill()
p.wait()
shutil.rmtree(d)
