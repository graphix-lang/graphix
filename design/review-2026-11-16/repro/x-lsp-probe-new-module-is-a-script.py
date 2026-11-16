# A module file the disk scan does not place in a project is checked as a
# script of its own, though the open root that `mod`s it checks it fine.
#
# Run: timeout -s KILL 120 python3 design/review-2026-11-16/repro/x-lsp-probe-new-module-is-a-script.py
#      (GRAPHIX=/path/to/graphix to pick the binary; default the review's debug build)
#
# Case 1: a workspace folder, `a.gx` on disk declares `mod b;`, and `b.gx`
#   is a new buffer not yet saved.
# Case 2: no workspace folder (single-file mode), both files on disk.
# Expected: no diagnostic on b.gx, and hovering `v` in b.gx shows `v: i64`
#   (the root a.gx's check reads b.gx's buffer and accepts it).
# Observed (HEAD e1be2fbf2), both cases:
#   b.gx: [0:8] `super` goes above the package root
#   hover v in b.gx: None
#   (and after saving b.gx in case 1 both are right).
# Case 3: a workspace folder, a.gx and b.gx on disk and open, unrelated;
#   a.gx's buffer gains `mod b;` (unsaved), then b.gx's buffer changes so
#   that a.gx's `b::v + k` no longer checks.
# Expected: a.gx is checked again: an error on a.gx, hover b::v a string.
# Observed: a.gx is never checked again: no diagnostic, hover `b::v: i64`.
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


def report(c, b):
    c.request("workspace/symbol", {"query": "\u0000"})
    ds = [(f"{d['range']['start']['line']}:{d['range']['start']['character']}", d["message"])
          for d in c.diags.get(uri(b), [])]
    print("  b.gx diagnostics:", ds)
    print("  hover v in b.gx:", c.hover(b, 0, 4))


A = "let k = 5;\nmod b;\nb::v\n"
B = "let v = super::k + 1;\n"
with tempfile.TemporaryDirectory() as d:
    a, b = d + "/a.gx", d + "/b.gx"
    open(a, "w").write(A)
    print("case 1: workspace folder, b.gx unsaved")
    c = Client(d)
    c.open(a, A)
    c.open(b, B)
    report(c, b)
    c.stop()
    open(b, "w").write(B)
    print("case 2: no workspace folder, both on disk")
    c = Client(None)
    c.open(a, A)
    c.open(b, B)
    report(c, b)
    c.stop()
    print("case 3: a.gx's unsaved buffer gains `mod b;`, then b.gx's buffer changes")
    open(a, "w").write("let k = 5;\nk\n")
    open(b, "w").write("let v = 1;\n")
    c = Client(d)
    c.open(a, "let k = 5;\nk\n")
    c.open(b, "let v = 1;\n")
    for path, text in ((a, "let k = 5;\nmod b;\nb::v + k\n"), (b, "let v = \"s\";\n")):
        c.notify("textDocument/didChange", {"textDocument": {"uri": uri(path), "version": 2},
                                             "contentChanges": [{"text": text}]})
        c.request("workspace/symbol", {"query": "\u0000"})
    print("  a.gx diagnostics:", [d_["message"].split("\n")[0] for d_ in c.diags.get(uri(a), [])])
    print("  hover b::v in a.gx:", c.hover(a, 2, 3))
    c.stop()
