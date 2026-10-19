"""Minimal LSP client over the real `graphix lsp` on stdio."""
import json, os, subprocess, sys, threading, queue, time, urllib.parse

BIN = "/home/eric/tmp/review-2026-10-19/bin/graphix"


def uri(path):
    return "file://" + urllib.parse.quote(path)


class Client:
    def __init__(self, root, encodings=None, timeout=150):
        env = dict(os.environ)
        env["XDG_CACHE_HOME"] = os.environ.get("XDG_CACHE_HOME_PROBE", "/tmp")
        self.p = subprocess.Popen(
            ["timeout", "-s", "KILL", str(timeout), BIN, "lsp"],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)
        self.q = queue.Queue()
        self.diags = {}
        self.id = 0
        self.stderr = []
        threading.Thread(target=self._read, daemon=True).start()
        threading.Thread(target=self._err, daemon=True).start()
        caps = {}
        if encodings:
            caps["general"] = {"positionEncodings": encodings}
        self.init = self.request("initialize", {
            "processId": None, "rootUri": uri(root), "capabilities": caps,
            "workspaceFolders": [{"uri": uri(root), "name": "t"}]})
        self.notify("initialized", {})

    def _err(self):
        for line in self.p.stderr:
            self.stderr.append(line.decode(errors="replace"))

    def _read(self):
        f = self.p.stdout
        while True:
            hdr = {}
            while True:
                line = f.readline()
                if not line:
                    self.q.put(None)
                    return
                line = line.strip()
                if not line:
                    break
                k, v = line.split(b":", 1)
                hdr[k.lower()] = v.strip()
            n = int(hdr[b"content-length"])
            body = f.read(n)
            self.q.put(json.loads(body))

    def raw(self, data: bytes):
        self.p.stdin.write(data)
        self.p.stdin.flush()

    def send(self, msg):
        body = json.dumps(msg).encode()
        self.raw(b"Content-Length: %d\r\n\r\n" % len(body) + body)

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def request(self, method, params, wait=120):
        self.id += 1
        rid = self.id
        self.send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
        deadline = time.time() + wait
        while True:
            try:
                m = self.q.get(timeout=max(0.1, deadline - time.time()))
            except queue.Empty:
                return {"timeout": True}
            if m is None:
                return {"dead": True, "rc": self.p.poll()}
            if m.get("id") == rid and "method" not in m:
                return m
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"]] = m["params"]
            if time.time() > deadline:
                return {"timeout": True}

    def open(self, path, text, version=1):
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(path), "languageId": "graphix", "version": version, "text": text}})

    def change(self, path, text, version, rng=None):
        ch = {"text": text}
        if rng is not None:
            ch["range"] = rng
        self.notify("textDocument/didChange", {
            "textDocument": {"uri": uri(path), "version": version},
            "contentChanges": [ch]})

    def close(self, path):
        self.notify("textDocument/didClose", {"textDocument": {"uri": uri(path)}})

    def at(self, method, path, line, ch, extra=None):
        p = {"textDocument": {"uri": uri(path)}, "position": {"line": line, "character": ch}}
        if extra:
            p.update(extra)
        return self.request("textDocument/" + method, p)

    def barrier(self):
        return self.request("workspace/symbol", {"query": "zzzzunlikely"})

    def shutdown(self):
        r = self.request("shutdown", None, wait=20)
        self.notify("exit", None)
        try:
            self.p.wait(timeout=10)
        except Exception:
            self.p.kill()
        return r


def pos_of(text, needle, off=0, enc="utf16"):
    i = text.index(needle) + off
    before = text[:i]
    line = before.count("\n")
    last = before.rsplit("\n", 1)[-1]
    if enc == "utf16":
        ch = len(last.encode("utf-16-le")) // 2
    else:
        ch = len(last)
    return line, ch
