"""Minimal LSP client over `graphix lsp` stdio, shared by the lsp-*.py repros.

Load it with: exec(open(os.path.join(os.path.dirname(__file__), "lsp-client.py")).read())
"""
import json, os, subprocess, sys, time, urllib.parse, select

BIN = "/home/eric/tmp/review-2026-10-26/bin/graphix"


def uri(path):
    return "file://" + urllib.parse.quote(path)


class Client:
    def __init__(self, root, encodings=None, env=None):
        e = dict(os.environ)
        e["XDG_CACHE_HOME"] = "/home/eric/tmp/review-2026-10-26/review/work/x-rerun-5/lsp/cache"
        if env:
            e.update(env)
        self.p = subprocess.Popen([BIN, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=open(
                                      "/home/eric/tmp/review-2026-10-26/review/work/x-rerun-5/lsp/stderr.log", "w"), env=e)
        self.id = 0
        self.diags = {}
        self.versions = {}
        caps = {}
        if encodings:
            caps = {"general": {"positionEncodings": encodings}}
        r = self.request("initialize", {"processId": None, "rootUri": None,
                                        "capabilities": caps,
                                        "workspaceFolders": [{"uri": uri(root), "name": "w"}]})
        self.init = r
        self.notify("initialized", {})

    def send(self, msg):
        b = json.dumps(msg).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
        self.p.stdin.flush()

    def read(self):
        hdr = b""
        while not hdr.endswith(b"\r\n\r\n"):
            c = self.p.stdout.read(1)
            if not c:
                raise EOFError
            hdr += c
        n = int([l for l in hdr.split(b"\r\n") if l.lower().startswith(b"content-length")][0].split(b":")[1])
        return json.loads(self.p.stdout.read(n))

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def request(self, method, params):
        self.id += 1
        i = self.id
        self.send({"jsonrpc": "2.0", "id": i, "method": method, "params": params})
        while True:
            m = self.read()
            if m.get("id") == i and "method" not in m:
                return m
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"]] = m["params"]

    def open(self, path, text):
        self.versions[path] = 1
        self.notify("textDocument/didOpen", {"textDocument": {"uri": uri(path), "languageId": "graphix", "version": 1, "text": text}})

    def change(self, path, text):
        self.versions[path] += 1
        self.notify("textDocument/didChange", {"textDocument": {"uri": uri(path), "version": self.versions[path]},
                                               "contentChanges": [{"text": text}]})

    def close(self, path):
        self.notify("textDocument/didClose", {"textDocument": {"uri": uri(path)}})

    def save(self, path):
        self.notify("textDocument/didSave", {"textDocument": {"uri": uri(path)}})

    def sync(self):
        return self.request("workspace/symbol", {"query": "\u0000"})

    def pos(self, path, line, ch, method):
        p = {"textDocument": {"uri": uri(path)}, "position": {"line": line, "character": ch}}
        if method == "textDocument/references":
            p["context"] = {"includeDeclaration": True}
        return self.request(method, p)

    def shutdown(self):
        try:
            self.request("shutdown", None)
            self.notify("exit", None)
            self.p.wait(timeout=10)
        except Exception:
            self.p.kill()
