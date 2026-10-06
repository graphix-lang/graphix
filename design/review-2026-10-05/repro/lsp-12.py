#!/usr/bin/env python3
# lsp-12: workspace/didChangeWatchedFiles is ignored; the project graph
# follows disk only on a didSave.
#
# The VS Code client watches **/*.gx (ide/editors/vscode/src/extension.ts:22)
# and sends workspace/didChangeWatchedFiles, which handle_notification
# (graphix-lsp/src/server.rs:221) drops. Only didSave rescans (saved()).
# workspace/symbol searches the active document's project, so it shows
# which project net.gx is in.
#
# net.gx is open; main.gx is written on disk to `mod net;` (a git checkout,
# a generator) and the client reports it with didChangeWatchedFiles.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-12.py <graphix>
#
# expected: after the watched-file change net.gx is in main.gx's project:
# workspace/symbol answers [net, x, y].
#
# observed (HEAD c722befe, debug build):
#   1. net.gx standalone:                 ['y']
#   2. main.gx changed on disk + watched: ['y']           (no rescan)
#   3. after a didSave of any document:   ['net', 'x', 'y']
import json, os, queue, subprocess, sys, tempfile, threading, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"


def uri(path):
    return "file://" + urllib.parse.quote(path, safe="/-._~")


class Client:
    def __init__(self, files):
        self.dir = tempfile.mkdtemp(prefix="lsp-12-")
        for f, t in files.items():
            open(os.path.join(self.dir, f), "w").write(t)
        env = dict(os.environ, XDG_CACHE_HOME=self.dir)
        self.p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
        self.q, self.id = queue.Queue(), 0
        threading.Thread(target=self.reader, daemon=True).start()
        self.request("initialize", {"processId": None, "rootUri": None, "capabilities": {},
                                    "workspaceFolders": [{"uri": uri(self.dir), "name": "w"}]})
        self.notify("initialized", {})

    def reader(self):
        while line := self.p.stdout.readline():
            if line.lower().startswith(b"content-length:"):
                n = int(line.split(b":")[1])
                while self.p.stdout.readline() not in (b"\r\n", b"\n", b""):
                    pass
                self.q.put(json.loads(self.p.stdout.read(n)))
        self.q.put(None)

    def send(self, msg):
        body = json.dumps(dict(msg, jsonrpc="2.0")).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.p.stdin.flush()

    def notify(self, method, params):
        self.send({"method": method, "params": params})

    def request(self, method, params):
        self.id += 1
        self.send({"id": self.id, "method": method, "params": params})
        while (m := self.q.get(timeout=100)) is not None:
            if m.get("id") == self.id and "method" not in m:
                return m.get("result")
        raise SystemExit(f"server died during {method}")

    def uri(self, name):
        return uri(os.path.join(self.dir, name))

    def symbols(self):
        return sorted(s["name"] for s in self.request("workspace/symbol", {"query": ""}) or [])

    def done(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


net = "let y = 2;\ny\n"
c = Client({"main.gx": "let x = 1;\nx\n", "net.gx": net})
c.notify("textDocument/didOpen", {"textDocument": {
    "uri": c.uri("net.gx"), "languageId": "graphix", "version": 1, "text": net}})
print("1. net.gx standalone:                ", c.symbols())
open(os.path.join(c.dir, "main.gx"), "w").write("let x = 1;\nmod net;\nnet::y\n")
c.notify("workspace/didChangeWatchedFiles",
         {"changes": [{"uri": c.uri("main.gx"), "type": 2}]})
print("2. main.gx changed on disk + watched:", c.symbols())
c.notify("textDocument/didSave", {"textDocument": {"uri": c.uri("net.gx")}})
print("3. after a didSave of any document:  ", c.symbols())
c.done()
