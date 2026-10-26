# A minimal LSP client over stdio for the x-lsp-probe repros: `graphix lsp`
# under `timeout -s KILL 170`, Content-Length framing, a request is a barrier
# (the server flushes its checks before answering when nothing else is queued).
import json, subprocess, threading, queue, os, sys, time, urllib.parse

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-10-26/bin/graphix")

def uri(p):
    return "file://" + urllib.parse.quote(p)

class Client:
    def __init__(self, root=None, utf32=False, env=None):
        e = dict(os.environ)
        e["XDG_CACHE_HOME"] = os.environ.get("XDG_CACHE_HOME_LSP", "/tmp/x-lsp-probe-cache")
        if env:
            e.update(env)
        self.p = subprocess.Popen(["timeout", "-s", "KILL", "170", BIN, "lsp"],
                                  stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE, env=e)
        self.q = queue.Queue()
        self.id = 0
        self.diags = {}
        self.notes = []
        threading.Thread(target=self._read, daemon=True).start()
        threading.Thread(target=self._err, daemon=True).start()
        self.stderr = []
        caps = {"general": {"positionEncodings": ["utf-32", "utf-16"] if utf32 else ["utf-16"]}}
        params = {"processId": None, "capabilities": caps,
                  "rootUri": uri(root) if root else None}
        if root:
            params["workspaceFolders"] = [{"uri": uri(root), "name": "w"}]
        self.init = self.request("initialize", params, timeout=120)
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
                line = line.decode().strip()
                if not line:
                    break
                k, v = line.split(":", 1)
                hdr[k.lower()] = v.strip()
            body = f.read(int(hdr["content-length"]))
            self.q.put(json.loads(body))

    def raw(self, data: bytes):
        self.p.stdin.write(data)
        self.p.stdin.flush()

    def send(self, msg):
        b = json.dumps(msg).encode()
        self.raw(b"Content-Length: %d\r\n\r\n" % len(b) + b)

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def request(self, method, params, timeout=60):
        self.id += 1
        rid = self.id
        self.send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
        return self.wait(rid, timeout)

    def wait(self, rid, timeout=60):
        end = time.time() + timeout
        while True:
            left = end - time.time()
            if left <= 0:
                return {"TIMEOUT": True}
            try:
                m = self.q.get(timeout=left)
            except queue.Empty:
                return {"TIMEOUT": True}
            if m is None:
                return {"EOF": True, "rc": self.p.poll()}
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"]] = m["params"]
                self.notes.append(m)
                continue
            if "method" in m:
                self.notes.append(m)
                continue
            if m.get("id") == rid:
                return m

    def drain(self, t=0.5):
        end = time.time() + t
        while time.time() < end:
            try:
                m = self.q.get(timeout=end - time.time())
            except queue.Empty:
                break
            if m is None:
                break
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"]] = m["params"]
            self.notes.append(m)

    def open(self, path, text, version=1, lang="graphix"):
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(path), "languageId": lang, "version": version, "text": text}})

    def change(self, path, text, version):
        self.notify("textDocument/didChange", {"textDocument": {"uri": uri(path), "version": version},
                                               "contentChanges": [{"text": text}]})

    def at(self, method, path, line, ch, extra=None):
        params = {"textDocument": {"uri": uri(path)}, "position": {"line": line, "character": ch}}
        if extra:
            params.update(extra)
        return self.request(method, params)

    def hover(self, path, line, ch):
        return self.at("textDocument/hover", path, line, ch)

    def definition(self, path, line, ch):
        return self.at("textDocument/definition", path, line, ch)

    def references(self, path, line, ch, decl=True):
        return self.at("textDocument/references", path, line, ch, {"context": {"includeDeclaration": decl}})

    def completion(self, path, line, ch):
        return self.at("textDocument/completion", path, line, ch)

    def symbols(self, path):
        return self.request("textDocument/documentSymbol", {"textDocument": {"uri": uri(path)}})

    def fmt(self, path):
        return self.request("textDocument/formatting", {"textDocument": {"uri": uri(path)},
                                                        "options": {"tabSize": 4, "insertSpaces": True}})

    def sync(self):
        # a request makes the server flush (when idle) before answering
        return self.request("workspace/symbol", {"query": "zzzznotthere"})

    def close(self):
        try:
            r = self.request("shutdown", None, timeout=20)
            self.notify("exit", None)
        except Exception as e:
            r = {"ERR": str(e)}
        try:
            self.p.wait(timeout=20)
        except subprocess.TimeoutExpired:
            self.p.kill()
            return {"shutdown": r, "rc": "HUNG"}
        return {"shutdown": r, "rc": self.p.returncode}

def show(label, x):
    print(f"== {label}: {json.dumps(x)[:1500]}")
