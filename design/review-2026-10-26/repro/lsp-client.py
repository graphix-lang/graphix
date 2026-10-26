"""A minimal LSP stdio client over the review binary, for the lsp-*.py repros.
Load it with exec(open(<this file>).read()); it defines Server and uri."""
import json, os, subprocess, sys, time, urllib.parse, select

BIN = "/home/eric/tmp/review-2026-10-26/bin/graphix"


def uri(path):
    return "file://" + urllib.parse.quote(path)


class Server:
    def __init__(self, root, utf32=False, env=None):
        self.root = root
        e = dict(os.environ)
        e["XDG_CACHE_HOME"] = os.environ.get("LSP_REPRO_CACHE", "/tmp/lsp-repro-cache")
        if env:
            e.update(env)
        self.p = subprocess.Popen([BIN, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=e)
        self.id = 0
        self.diags = {}
        self.versions = {}
        caps = {"general": {"positionEncodings": ["utf-32", "utf-16"]}} if utf32 else {}
        r = self.request("initialize", {"processId": None, "rootUri": uri(root),
                                        "capabilities": caps,
                                        "workspaceFolders": [{"uri": uri(root), "name": "t"}]})
        self.init = r
        self.notify("initialized", {})

    def send(self, msg):
        body = json.dumps(msg).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.p.stdin.flush()

    def read(self, timeout=120):
        f = self.p.stdout
        hdr = b""
        while not hdr.endswith(b"\r\n\r\n"):
            c = f.read(1)
            if not c:
                raise EOFError
            hdr += c
        n = int([l for l in hdr.split(b"\r\n") if l.lower().startswith(b"content-length")][0].split(b":")[1])
        return json.loads(f.read(n))

    def request(self, method, params):
        self.id += 1
        rid = self.id
        self.send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
        while True:
            m = self.read()
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"]] = m["params"]["diagnostics"]
                continue
            if m.get("id") == rid:
                return m

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def path(self, f):
        return os.path.join(self.root, f)

    def open(self, f, text):
        self.versions[f] = 1
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(self.path(f)), "languageId": "graphix", "version": 1, "text": text}})

    def change(self, f, text):
        self.versions[f] += 1
        self.notify("textDocument/didChange", {
            "textDocument": {"uri": uri(self.path(f)), "version": self.versions[f]},
            "contentChanges": [{"text": text}]})

    def close(self, f):
        self.notify("textDocument/didClose", {"textDocument": {"uri": uri(self.path(f))}})

    def save(self, f):
        self.notify("textDocument/didSave", {"textDocument": {"uri": uri(self.path(f))}})

    def at(self, f, line, ch):
        return {"textDocument": {"uri": uri(self.path(f))}, "position": {"line": line, "character": ch}}

    def barrier(self):
        # a request after notifications: every diagnostic they caused has arrived
        return self.request("workspace/symbol", {"query": "zzzzzz_nothing"})

    def diag(self, f):
        return self.diags.get(uri(self.path(f)))

    def close_all(self):
        try:
            self.request("shutdown", None)
            self.notify("exit", None)
            self.p.wait(timeout=10)
        except Exception:
            self.p.kill()
