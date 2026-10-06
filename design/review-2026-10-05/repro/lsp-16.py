#!/usr/bin/env python3
# lsp-16: a document symbol's range stops at its name, and reference and
# definition locations are zero-width.
#
# document_symbols (graphix-lsp/src/symbols.rs:107) ends `range` where the
# name ends, though LSP's `range` encloses the whole declaration (clients
# use it to find the symbol the cursor is in: outline follow, breadcrumbs,
# sticky scroll). Query::location (graphix-lsp/src/query.rs:100) answers
# Range { start: at, end: at }, so a references list highlights nothing.
#
# main.gx: a four-line `let f = |x: i64| -> i64 { .. };` then `f(3)`.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-16.py <graphix>
#
# expected: symbol f range 0:0-3:1 (the declaration), selectionRange 0:4-0:5;
# reference/definition ranges cover the name (0:4-0:5, 4:0-4:1).
#
# observed (HEAD c722befe, debug build):
#   symbol f: range 0:0-0:5, selectionRange 0:4-0:5
#   references: 0:4-0:4, 4:0-4:0
#   definition: 0:4-0:4
import json, os, queue, subprocess, sys, tempfile, threading, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"


def uri(path):
    return "file://" + urllib.parse.quote(path, safe="/-._~")


class Client:
    def __init__(self, files):
        self.dir = tempfile.mkdtemp(prefix="lsp-16-")
        for f, t in files.items():
            open(os.path.join(self.dir, f), "w").write(t)
        env = dict(os.environ, XDG_CACHE_HOME=self.dir)
        self.p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
        self.q, self.id = queue.Queue(), 0
        threading.Thread(target=self.reader, daemon=True).start()
        self.request("initialize", {"processId": None, "rootUri": None, "capabilities": {},
                                    "workspaceFolders": [{"uri": uri(self.dir), "name": "w"}]})
        self.notify("initialized", {})

    def reader(self):
        while line := self.p.stdout.readline():
            if line.lower().startswith(b"content-length:"):
                n = int(line.split(b":")[1])
                while self.p.stdout.readline() not in (b"\r\n", b"\n", b""):
                    pass
                self.q.put(json.loads(self.p.stdout.read(n)))
        self.q.put(None)

    def send(self, msg):
        body = json.dumps(dict(msg, jsonrpc="2.0")).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.p.stdin.flush()

    def notify(self, method, params):
        self.send({"method": method, "params": params})

    def request(self, method, params):
        self.id += 1
        self.send({"id": self.id, "method": method, "params": params})
        while (m := self.q.get(timeout=100)) is not None:
            if m.get("id") == self.id and "method" not in m:
                return m.get("result")
        raise SystemExit(f"server died during {method}")

    def done(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


def rng(r):
    s, e = r["start"], r["end"]
    return f"{s['line']}:{s['character']}-{e['line']}:{e['character']}"


main = "let f = |x: i64| -> i64 {\n    let y = x + 1;\n    y * 2\n};\nf(3)\n"
c = Client({"main.gx": main})
doc = {"uri": uri(os.path.join(c.dir, "main.gx"))}
c.notify("textDocument/didOpen", {"textDocument": dict(doc, languageId="graphix",
                                                       version=1, text=main)})
for s in c.request("textDocument/documentSymbol", {"textDocument": doc}) or []:
    print(f"symbol {s['name']}: range {rng(s['range'])}, selectionRange {rng(s['selectionRange'])}")
at = {"textDocument": doc, "position": {"line": 4, "character": 0}}
refs = c.request("textDocument/references", dict(at, context={"includeDeclaration": True}))
print("references:", ", ".join(rng(r["range"]) for r in refs or []))
print("definition:", rng(c.request("textDocument/definition", at)["range"]))
c.done()
