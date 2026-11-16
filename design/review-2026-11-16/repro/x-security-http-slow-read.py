# Title: an http::serve client that never reads its response holds its
# connection, and its share of #max_connections, for good: no timeout
# bounds the response write (header_timeout and body_timeout bound only
# what the server reads).
#
# Command: timeout -s KILL 100 python3 -I design/review-2026-11-16/repro/x-security-http-slow-read.py
#   (GRAPHIX=<binary> overrides the graphix binary)
# Setup: a server with #max_connections: 1 and 2 s header/body timeouts
#   whose handler answers every request with a 16 MiB body. Client A
#   (4 KiB receive buffer) sends one GET and never reads. 8 s later client
#   B sends an ordinary GET.
# Expected: A's connection is closed once its write stalls past a send
#   deadline, and B is answered.
# Observed: "B got nothing in 20s" (A holds the only slot until it closes).
#   With BIG=4 (a 16-byte body that fits the socket buffers) B is answered
#   at once: the write that cannot finish is what holds the slot. With the
#   default 768 slots, 768 such clients lock out every other client for
#   as long as they stay connected.
import os, re, shutil, socket, subprocess, sys, tempfile, time

gx = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
big = int(os.environ.get("BIG", "24"))
prog = """
let big = array::fold(array::init(%d, |i| i), "x", |acc, i| str::concat(acc, acc));
let handler = |req: http::Request| {
    body: req ~ big,
    headers: [],
    status: u16:200
};
let server = http::serve(
    #addr: "127.0.0.1:0",
    #max_connections: 1,
    #header_timeout: duration:2.s,
    #body_timeout: duration:2.s,
    #handler: handler
)$;
http::server_addr(server)
""" % big
d = tempfile.mkdtemp()
path = os.path.join(d, "server.gx")
open(path, "w").write(prog)
p = subprocess.Popen(["timeout", "-s", "KILL", "90", gx, "--no-cache", path],
                     stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
addr = None
t0 = time.time()
while time.time() - t0 < 30:
    line = p.stdout.readline()
    if not line:
        break
    m = re.search(r"127\.0\.0\.1:(\d+)", line)
    if m:
        addr = ("127.0.0.1", int(m.group(1)))
        break
print("server at", addr, flush=True)
a = socket.socket()
a.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4096)
a.connect(addr)
a.sendall(b"GET / HTTP/1.1\r\nHost: x\r\n\r\n")
time.sleep(8)
b = socket.socket()
b.settimeout(20)
b.connect(addr)
b.sendall(b"GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")
try:
    print("B got:", b.recv(64)[:40], flush=True)
except socket.timeout:
    print("B got nothing in 20s", flush=True)
a.close()
b.close()
p.kill()
p.wait()
shutil.rmtree(d)
