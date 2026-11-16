#!/usr/bin/env python3
# Title: a type declared inside a function body has no hover, definition or
#   references, at its declaration or its uses; a block-local one does.
# Command:
#   TMPDIR=<scratch> timeout -s KILL 120 python3 design/review-2026-11-16/repro/lsp-lambda-local-type.py
#   (GRAPHIX overrides the binary; default the review's debug build)
# Expected: `Inner` (line 6 declaration, line 7 use) answers like `Loc`
#   (line 1 declaration, line 2 use): hover `type Inner = (i64, i64)`,
#   definition main.gx 6:7, references [6:7, 7:9].
# Observed:
#   Loc   decl: hover type Loc = { a: i64 } / def ['main.gx 1:7'] / refs ['main.gx 1:7', 'main.gx 2:9']
#   Loc   use : hover type Loc = { a: i64 } / def ['main.gx 1:7'] / refs ['main.gx 1:7', 'main.gx 2:9']
#   Inner decl: hover None / def None / refs None
#   Inner use : hover None / def None / refs ['main.gx 7:9']
# The admin TUI hits it: `type Then` in graphix-package-netidx-admin
# src/graphix/tui/local.gx, inside a function, answers nothing.
import json, os, subprocess, sys, tempfile, urllib.parse

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
FILES = {
    "main.gx": "let r = {\n  type Loc = {a: i64};\n  let v: Loc = {a: 1};\n  v.a\n};\nlet f = |x: i64| {\n  type Inner = (i64, i64);\n  let t: Inner = (x, x);\n  t.0\n};\nr + f(2)\n",
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


root = tempfile.mkdtemp(prefix="lsp-local-type-")
for name, text in FILES.items():
    open(os.path.join(root, name), "w").write(text)
lsp = Lsp(root)
lsp.open("main.gx", FILES["main.gx"])
for what, line, ch in [("Loc   decl", 1, 8), ("Loc   use ", 2, 10), ("Inner decl", 6, 8), ("Inner use ", 7, 10)]:
    hover = lsp.at("textDocument/hover", "main.gx", line, ch)
    hover = hover and hover["contents"]["value"].split("\n")[1]
    print(what + ": hover", hover, "/ def", where(lsp.at("textDocument/definition", "main.gx", line, ch)),
          "/ refs", where(lsp.at("textDocument/references", "main.gx", line, ch)))
lsp.p.kill()
