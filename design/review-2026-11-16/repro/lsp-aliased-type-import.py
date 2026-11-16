#!/usr/bin/env python3
# Title: a type imported under an alias (`use util::Point as P`) has no
#   hover/definition at its uses, and references of `Point` miss them.
# Command:
#   TMPDIR=<scratch> timeout -s KILL 120 python3 design/review-2026-11-16/repro/lsp-aliased-type-import.py
#   (GRAPHIX overrides the binary; default the review's debug build)
# Expected: hover on `P` in `let p: P` shows `type Point = {..}`, definition
#   goes to util.gx 0:5, and references of `Point` include main.gx 2:7.
# Observed:
#   hover on P            -> None
#   definition on P       -> None
#   references of P       -> [main.gx 2:7]   (only itself)
#   references of Point   -> main.gx 1:10, main.gx 3:13, util.gx 0:5, util.gx 1:12
#                            (main.gx 2:7, the `P` annotation, is missing)
import json, os, subprocess, sys, tempfile, urllib.parse

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
FILES = {
    "main.gx": "mod util;\nuse util::Point as P;\nlet p: P = {x: 1, y: 2};\nlet q: util::Point = p;\nq.x\n",
    "util.gx": "type Point = {x: i64, y: i64};\nlet origin: Point = {x: 0, y: 0};\n",
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


root = tempfile.mkdtemp(prefix="lsp-alias-")  # under $TMPDIR
for name, text in FILES.items():
    open(os.path.join(root, name), "w").write(text)
lsp = Lsp(root)
lsp.open("main.gx", FILES["main.gx"])
hover = lsp.at("textDocument/hover", "main.gx", 2, 7)
print("hover on P           ->", hover and hover["contents"]["value"])
print("definition on P      ->", where(lsp.at("textDocument/definition", "main.gx", 2, 7)))
print("references of P      ->", where(lsp.at("textDocument/references", "main.gx", 2, 7)))
print("references of Point  ->", where(lsp.at("textDocument/references", "main.gx", 3, 15)))
lsp.p.kill()
