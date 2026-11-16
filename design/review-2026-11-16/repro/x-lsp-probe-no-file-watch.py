# The server never asks the client to watch files, so only a client that
# watches on its own (the VS Code extension's `synchronize.fileEvents`)
# ever sends workspace/didChangeWatchedFiles; in Neovim, Helix or Zed a
# module changed on disk outside the editor (git checkout, a generator)
# stays unchecked until some buffer is saved.
#
# Run: timeout -s KILL 120 python3 design/review-2026-11-16/repro/x-lsp-probe-no-file-watch.py
#
# The client offers workspace.didChangeWatchedFiles.dynamicRegistration.
# Expected: a client/registerCapability request for **/*.{gx,gxi}, and the
#   closed module's new error reported once it is registered.
# Observed (HEAD e1be2fbf2): no server request at all; after util.gx changes
#   on disk, main.gx's root is not checked again: no diagnostic anywhere.
import json, os, queue, subprocess, sys, tempfile, threading, urllib.parse

BIN = os.environ.get("GRAPHIX", "/home/eric/tmp/review-2026-11-16/bin/graphix")


def uri(p):
    return "file://" + urllib.parse.quote(p, safe="/")


class Client:
    def __init__(self, root):
        self.p = subprocess.Popen([BIN, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                  stderr=subprocess.DEVNULL)
        self.q, self.id, self.diags = queue.Queue(), 0, {}
        threading.Thread(target=self._read, daemon=True).start()
        params = {"processId": None, "capabilities": {"workspace": {
            "didChangeWatchedFiles": {"dynamicRegistration": True}}}}
        if root:
            params["workspaceFolders"] = [{"uri": uri(root), "name": "w"}]
        self.request("initialize", params)
        self.notify("initialized", {})

    def _read(self):
        f = self.p.stdout
        while True:
            n = None
            while True:
                line = f.readline()
                if not line:
                    return
                line = line.decode().strip()
                if not line:
                    break
                k, v = line.split(":", 1)
                if k.lower() == "content-length":
                    n = int(v)
            self.q.put(json.loads(f.read(n)))

    def send(self, msg):
        body = json.dumps(msg).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.p.stdin.flush()

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def request(self, method, params):
        self.id += 1
        self.send({"jsonrpc": "2.0", "id": self.id, "method": method, "params": params})
        while True:
            m = self.q.get(timeout=60)
            if "method" in m and "id" in m:
                print("  server request:", m["method"])
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"]] = m["params"]["diagnostics"]
            elif m.get("id") == self.id:
                return m.get("result")

    def open(self, path, text):
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(path), "languageId": "graphix", "version": 1, "text": text}})

    def hover(self, path, line, ch):
        r = self.request("textDocument/hover", {"textDocument": {"uri": uri(path)},
                                                "position": {"line": line, "character": ch}})
        return r and r["contents"]["value"].split("\n")[1]

    def stop(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


with tempfile.TemporaryDirectory() as d:
    m, u = d + "/main.gx", d + "/util.gx"
    open(m, "w").write("mod util;\nutil::v + 1\n")
    open(u, "w").write("let v = 1;\n")
    c = Client(d)
    c.open(m, open(m).read())
    c.request("workspace/symbol", {"query": "\u0000"})
    open(u, "w").write('let v = "s";\n')   # behind the editor's back
    c.request("workspace/symbol", {"query": "\u0000"})
    print("  diagnostics:", {k.rsplit("/", 1)[1]: [x["message"].split("\n")[0] for x in v]
                             for k, v in c.diags.items()})
    c.stop()
