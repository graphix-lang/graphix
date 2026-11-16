#!/usr/bin/env python3
# Title: a module named `target` (or `vendor`, `node_modules`, or any
#   dot-name) is never in its project: the workspace scan skips the
#   directory, so its file is checked as a lone script.
# Command:
#   TMPDIR=<scratch> timeout -s KILL 60 python3 design/review-2026-11-16/repro/lsp-skipped-module-dir.py
#   (GRAPHIX overrides the binary; default the review's debug build)
#   `graphix --check main.gx` on the same files exits 0.
# Expected: target/mod.gx (opened alone) is checked under main.gx, as
#   util/mod.gx is: no diagnostics, hover on `v` shows `v: i64`.
# Observed:
#   target: diagnostics ['`super` goes above the package root'], hover on v None
#   util:   diagnostics [], hover on v '```graphix\nv: i64\n```'
import json, os, subprocess, sys, tempfile, urllib.parse

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")


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


for name in ["target", "util"]:
    root = tempfile.mkdtemp(prefix="lsp-skipped-")
    os.mkdir(os.path.join(root, name))
    open(os.path.join(root, "main.gx"), "w").write("let k = 1;\nmod %s;\n%s::v\n" % (name, name))
    body = "use super::k;\nlet v = k + 1;\n"
    open(os.path.join(root, name, "mod.gx"), "w").write(body)
    lsp = Lsp(root)
    lsp.open(name + "/mod.gx", body)
    diags = []
    lsp.n += 1
    lsp.send({"jsonrpc": "2.0", "id": lsp.n, "method": "workspace/symbol", "params": {"query": "q"}})
    while True:
        m = lsp.recv()
        if m.get("method") == "textDocument/publishDiagnostics":
            diags += [d["message"] for d in m["params"]["diagnostics"]]
        if m.get("id") == lsp.n:
            break
    hover = lsp.at("textDocument/hover", name + "/mod.gx", 1, 4)
    print("%s: diagnostics %s, hover on v %r" % (name, diags, hover and hover["contents"]["value"]))
    lsp.p.kill()
