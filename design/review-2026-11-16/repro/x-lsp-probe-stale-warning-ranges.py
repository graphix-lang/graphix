# Warnings kept from the last successful check are republished, with the
# current document version, at the positions of the old text.
#
# Run: timeout -s KILL 120 python3 design/review-2026-11-16/repro/x-lsp-probe-stale-warning-ranges.py
#
# v1 "use array::len;\nlet x = 1;\nx\n" -> warning 0:11-0:14 `len` (right).
# v2 inserts two lines above, the first unparseable.
# Expected: the warning moves with its text (2:11-2:14) or is not
#   published as v2's.
# Observed (HEAD e1be2fbf2): published as version 2 at 0:11-0:14, which in
#   v2 is past the end of `let a = (;` (in general: whatever text is there).
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
        params = {"processId": None, "capabilities": {}}
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
    f = d + "/main.gx"
    v1 = "use array::len;\nlet x = 1;\nx\n"
    v2 = "let a = (;\nlet b = 2;\n" + v1
    open(f, "w").write(v1)
    c = Client(d)
    c.open(f, v1)
    for version, text in ((1, v1), (2, v2)):
        if version == 2:
            c.notify("textDocument/didChange", {"textDocument": {"uri": uri(f), "version": 2},
                                                 "contentChanges": [{"text": v2}]})
        c.request("workspace/symbol", {"query": "\u0000"})
        lines = text.split("\n")
        for dd in c.diags.get(uri(f), []):
            s, e = dd["range"]["start"], dd["range"]["end"]
            print(f"v{version}", "warning" if dd["severity"] == 2 else "error",
                  f"{s['line']}:{s['character']}-{e['line']}:{e['character']}",
                  repr(lines[s["line"]][s["character"]:e["character"]]), dd["message"].split("\n")[0])
    c.stop()
