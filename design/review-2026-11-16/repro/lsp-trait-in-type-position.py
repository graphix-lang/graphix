#!/usr/bin/env python3
# Title: a trait named as a bound (`'a: Show`) or as a parameter's type
#   (`|x: Show|`) has no hover/definition, and references of the trait miss
#   both sites.
# Command:
#   TMPDIR=<scratch> timeout -s KILL 120 python3 design/review-2026-11-16/repro/lsp-trait-in-type-position.py
#   (GRAPHIX overrides the binary; default the review's debug build)
# Expected: hover on `Show` at main.gx 2:12 and 3:12 shows the trait, as
#   it does at 1:10 (the use) and 2:18 (`Show::show`); references of Show
#   include main.gx 2:12 and 3:12.
# Observed:
#   Show in use      hover: trait Show { / def ['util.gx 1:6']
#   Show as param    hover: None / def None
#   Show as bound    hover: None / def None
#   references of Show: ['main.gx 1:10', 'main.gx 2:18', 'main.gx 3:25', 'util.gx 1:6', 'util.gx 2:5']
#   (main.gx 2:12 and 3:12 missing)
import json, os, subprocess, sys, tempfile, urllib.parse

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")
FILES = {
    "main.gx": "mod util;\nuse util::Show;\nlet f = |x: Show| Show::show(x);\nlet g = 'a: Show |x: 'a| Show::show(x);\nf(util::C(1)) + g(util::C(2))\n",
    "util.gx": "type C = Abstract<i64>;\ntrait Show { val show: fn(self) -> i64 };\nimpl Show for C { let show = |c| c.0 };\n",
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


root = tempfile.mkdtemp(prefix="lsp-trait-type-")
for name, text in FILES.items():
    open(os.path.join(root, name), "w").write(text)
lsp = Lsp(root)
lsp.open("main.gx", FILES["main.gx"])
for what, line, ch in [("Show in use  ", 1, 10), ("Show as param", 2, 12), ("Show as bound", 3, 12)]:
    hover = lsp.at("textDocument/hover", "main.gx", line, ch)
    hover = hover and hover["contents"]["value"].split("\n")[1]
    print(what, " hover:", hover, "/ def", where(lsp.at("textDocument/definition", "main.gx", line, ch)))
print("references of Show:", where(lsp.at("textDocument/references", "main.gx", 1, 10)))
lsp.p.kill()
