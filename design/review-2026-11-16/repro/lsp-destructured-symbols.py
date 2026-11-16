#!/usr/bin/env python3
# Title: names a top-level `let` binds through a pattern (`let (lo, hi) =`,
#   `let {x, y} =`) are missing from documentSymbol and workspace/symbol.
# Command:
#   TMPDIR=<scratch> timeout -s KILL 60 python3 design/review-2026-11-16/repro/lsp-destructured-symbols.py
#   (GRAPHIX overrides the binary; default the review's debug build)
# Expected: documentSymbol and workspace/symbol list lo, hi, x, y, z.
# Observed:
#   documentSymbol   -> ['z']
#   workspace/symbol -> ['z']
import json, os, subprocess, sys, tempfile, urllib.parse

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
FILES = {
    "main.gx": "let (lo, hi) = (1, 2);\nlet {x, y} = {x: 3, y: 4};\nlet z = lo + hi + x + y;\nz\n",
}


class Lsp:
    def __init__(self, root):
        env = dict(os.environ, XDG_CACHE_HOME=os.path.join(root, ".cache"))
        self.p = subprocess.Popen(["timeout", "-s", "KILL", "100", BIN, "lsp"], env=env,
                                  stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        self.n = 0
        self.root = root
        self.call("initialize", {"processId": None, "rootUri": None, "capabilities": {},
                                 "workspaceFolders": [{"uri": self.uri(""), "name": "w"}]})
        self.send({"jsonrpc": "2.0", "method": "initialized", "params": {}})

    def uri(self, rel):
        return "file://" + urllib.parse.quote(os.path.join(self.root, rel))

    def send(self, m):
        b = json.dumps(m).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n%s" % (len(b), b))
        self.p.stdin.flush()

    def recv(self):
        n = None
        while True:
            line = self.p.stdout.readline()
            if line in (b"\r\n", b""):
                break
            k, v = line.decode().split(":", 1)
            if k.lower() == "content-length":
                n = int(v)
        return json.loads(self.p.stdout.read(n))

    def call(self, method, params):
        self.n += 1
        self.send({"jsonrpc": "2.0", "id": self.n, "method": method, "params": params})
        while True:
            m = self.recv()
            if m.get("id") == self.n and "method" not in m:
                return m.get("result")

    def open(self, rel, text):
        self.send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {
            "textDocument": {"uri": self.uri(rel), "languageId": "graphix", "version": 1, "text": text}}})

    def at(self, method, rel, line, ch):
        return self.call(method, {"textDocument": {"uri": self.uri(rel)},
                                  "position": {"line": line, "character": ch},
                                  "context": {"includeDeclaration": True}})


def where(r):
    if r is None:
        return None
    r = [r] if isinstance(r, dict) and "uri" in r else r
    return ["%s %d:%d" % (x["uri"].rsplit("/", 1)[1], x["range"]["start"]["line"],
                          x["range"]["start"]["character"]) for x in r]


root = tempfile.mkdtemp(prefix="lsp-destructured-")
for name, text in FILES.items():
    open(os.path.join(root, name), "w").write(text)
lsp = Lsp(root)
lsp.open("main.gx", FILES["main.gx"])
syms = lsp.call("textDocument/documentSymbol", {"textDocument": {"uri": lsp.uri("main.gx")}})
print("documentSymbol   ->", [s["name"] for s in syms or []])
print("workspace/symbol ->", [s["name"] for s in lsp.call("workspace/symbol", {"query": ""}) or []])
lsp.p.kill()
