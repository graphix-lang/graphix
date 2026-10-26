# Title: a request that arrives with more messages queued behind it is answered
#        before any check, so a document opened while the server was busy gets
#        an answer from no check at all.
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/lsp-request-mid-burst-unchecked.py
# Expected: both documentSymbol answers for b.gx carry the types
#   (v: i64, w: fn(x: i64) -> i64): b.gx's root is dirty when the first arrives.
# Observed:
#   documentSymbol (first)  [('v', None), ('w', None)]
#   documentSymbol (second) [('v', 'i64'), ('w', 'fn(x: i64) -> i64')]
# A client that asks for the outline once after didOpen (VS Code) keeps the
# untyped one until the next edit.
import os, shutil, tempfile
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())

root = tempfile.mkdtemp()
heavy = "\n".join(f"let f{i} = |x: i64| -> i64 x + {i};" for i in range(400)) + "\nf1(1)\n"
open(os.path.join(root, "heavy.gx"), "w").write(heavy)
b = "let v = 1;\nlet w = |x: i64| x + v;\nw(v)\n"
open(os.path.join(root, "b.gx"), "w").write(b)
s = Server(root)
s.open("heavy.gx", heavy)  # the server checks this while the rest queues
s.open("b.gx", b)
sym = {"textDocument": {"uri": uri(s.path("b.gx"))}}
ids = []
for _ in range(2):
    s.id += 1
    ids.append(s.id)
    s.send({"jsonrpc": "2.0", "id": s.id, "method": "textDocument/documentSymbol", "params": sym})
got = {}
while len(got) < 2:
    m = s.read()
    if m.get("id") in ids:
        got[m["id"]] = m["result"]
for tag, k in zip(("first", "second"), ids):
    print(f"documentSymbol ({tag})", [(x["name"], x.get("detail")) for x in got[k]])
s.close_all()
shutil.rmtree(root)
