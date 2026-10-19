"""The reference recorded for `*rr <- v` stands on `*r`, not on `rr`.

Run: timeout -s KILL 170 python3 design/review-2026-10-19/repro/x-lsp-probe-deref-connect-site.py
Expected: references of `rr` include line 3 columns 1-3 (`rr` in `*rr <- ...`), and
          hover answers at column 3 (the end of the name, as everywhere else).
Observed:  the reference is line 3 columns 0-2 (`*r`); hover at column 3 is null.
           ConnectDeref::compile records ReferenceSite { pos: spec.pos } and the
           parser's `connect` takes that position before the optional `*`.
"""
import os, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from x_lsp_probe_lspc import Client

root = tempfile.mkdtemp()
f = root + "/main.gx"
text = "let cell = 0;\nlet rr = &mut cell;\nlet clock = sys::time::timer(duration:10.ms, false);\n*rr <- clock ~ 5;\ncell\n"
open(f, "w").write(text)
c = Client(root)
c.open(f, text); c.barrier()
rs = c.at("references", f, 1, 4, {"context": {"includeDeclaration": True}}).get("result")
print("references of rr:", [(x["range"]["start"]["line"], x["range"]["start"]["character"], x["range"]["end"]["character"]) for x in rs])
for col in range(4):
    h = c.at("hover", f, 3, col).get("result")
    print("hover (3,%d):" % col, h and h["contents"]["value"].split("\n")[1])
c.shutdown()
