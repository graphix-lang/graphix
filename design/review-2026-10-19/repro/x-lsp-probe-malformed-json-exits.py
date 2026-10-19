"""One well-framed message whose body is not JSON ends the language server.

Run: timeout -s KILL 170 python3 design/review-2026-10-19/repro/x-lsp-probe-malformed-json-exits.py
Expected: the server answers or ignores the bad message (JSON-RPC: a -32700 parse
          error) and answers the hover that follows.
Observed:  stderr "Error: malformed LSP payload: ..." and exit 1; the hover gets no
           answer. stdio()'s reader thread returns the io::Error of
           lsp_server::Message::read, dropping the channel, and serve() ends.
"""
import os, sys, json, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from x_lsp_probe_lspc import Client

root = tempfile.mkdtemp()
f = root + "/main.gx"
text = "let yy = 1;\nyy\n"
open(f, "w").write(text)
c = Client(root)
c.open(f, text); c.barrier()
bad = b"not json at all"
c.raw(b"Content-Length: %d\r\n\r\n" % len(bad) + bad)
r = c.at("hover", f, 0, 4)
c.p.wait(timeout=10)
print("hover after bad message:", json.dumps(r), "exit code:", c.p.returncode)
print("stderr:", "".join(c.stderr).strip())
