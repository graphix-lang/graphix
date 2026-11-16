# The package root's own `mod <package>` wrapper, which nobody wrote, is
# answered as a written reference at line 1, column 1 of mod.gx.
#
# Run: timeout -s KILL 120 python3 design/review-2026-11-16/repro/x-lsp-probe-package-phantom-reference.py
#
# A crate graphix-package-foo with src/graphix/mod.gx:
#   mod sub;
#   let f = |x: i64| -> i64 sub::g(x) * 2;
#   let k = package::sub::g(1);
# References on `package` (line 2, column 9).
# Expected: one location, 2:8-2:15 (`package`).
# Observed (HEAD e1be2fbf2): also 0:0-0:3 in mod.gx, which underlines `mod`
#   (the length of the package name `foo`, at the wrapper's default position).
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
    crate = d + "/graphix-package-foo"
    os.makedirs(crate + "/src/graphix")
    open(crate + "/Cargo.toml", "w").write('[package]\nname = "graphix-package-foo"\nversion = "0.1.0"\n')
    mg = crate + "/src/graphix/mod.gx"
    text = "mod sub;\nlet f = |x: i64| -> i64 sub::g(x) * 2;\nlet k = package::sub::g(1);\n"
    open(mg, "w").write(text)
    open(crate + "/src/graphix/mod.gxi", "w").write("val f: fn(x: i64) -> i64;\nval k: i64;\nmod sub;\n")
    open(crate + "/src/graphix/sub.gx", "w").write("let g = |x: i64| -> i64 x + 1;\n")
    c = Client(d)
    c.open(mg, text)
    refs = c.request("textDocument/references", {"textDocument": {"uri": uri(mg)},
                     "position": {"line": 2, "character": 9}, "context": {"includeDeclaration": True}})
    lines = text.split("\n")
    for l in refs or []:
        s, e = l["range"]["start"], l["range"]["end"]
        print(f"{s['line']}:{s['character']}-{e['line']}:{e['character']}",
              repr(lines[s["line"]][s["character"]:e["character"]]))
    c.stop()
