# t-seq: a `*r <- v` statement in a seq step records r's reference site at the `*`
# Run: timeout -s KILL 120 python3 -I design/review-2026-11-16/repro/t-seq-lsp-deref-target-site.py /home/eric/tmp/review-2026-11-16/bin/graphix
# Expected: references of `r` include (4, 1, 'r') for the top-level `*r <- m`
#   and (7, 5, 'r') for the `*r <- 5` inside the seq.
# Observed: r decl -> [(2, 4, 'r'), (4, 1, 'r'), (7, 4, '*')]: inside the seq
#   the site is the statement's position, since the lowering rebuilds the
#   connect with WrittenAt::NOWHERE and notes the cell at the statement's pos.
import json, os, select, subprocess, sys, tempfile, time
SRC = """let clk = sys::time::timer(duration:10.ms, true);
let n = 0;
let r = &mut n;
let m = 0;
*r <- m;
let a = seq clk {
    n <- n + 1;
    *r <- 5;
    let q = 1;
    m <- q;
    n
};
a
"""
root = tempfile.mkdtemp()
path = os.path.join(root, "a.gx")
open(path, "w").write(SRC)
uri = "file://" + path
p = subprocess.Popen([sys.argv[1], "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.DEVNULL, cwd=root)
buf = b""
def send(msg):
    b = json.dumps(msg).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b); p.stdin.flush()
def recv(want, timeout=60):
    global buf
    end = time.time() + timeout
    while time.time() < end:
        while b"\r\n\r\n" in buf:
            h, rest = buf.split(b"\r\n\r\n", 1)
            n = int([l for l in h.split(b"\r\n") if l.lower().startswith(b"content-length")][0].split(b":")[1])
            if len(rest) < n: break
            body, buf = rest[:n], rest[n:]
            m = json.loads(body)
            if m.get("id") == want and ("result" in m or "error" in m): return m
        if select.select([p.stdout], [], [], 0.5)[0]:
            d = os.read(p.stdout.fileno(), 65536)
            if not d: break
            buf += d
send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"processId": None, "rootUri": "file://" + root, "capabilities": {}}})
recv(1)
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": uri, "languageId": "graphix", "version": 1, "text": SRC}}})
time.sleep(3)
send({"jsonrpc": "2.0", "id": 2, "method": "textDocument/references", "params": {"textDocument": {"uri": uri}, "position": {"line": 2, "character": 4}, "context": {"includeDeclaration": True}}})
m = recv(2)
lines = SRC.split("\n")
locs = sorted((l["range"]["start"]["line"], l["range"]["start"]["character"], l["range"]["end"]["character"]) for l in (m or {}).get("result") or [])
print("r decl ->", [(l, c, lines[l][c:e]) for (l, c, e) in locs])
send({"jsonrpc": "2.0", "id": 3, "method": "shutdown", "params": None}); recv(3, 5)
send({"jsonrpc": "2.0", "method": "exit", "params": None})
try: p.wait(timeout=5)
except Exception: p.kill()
