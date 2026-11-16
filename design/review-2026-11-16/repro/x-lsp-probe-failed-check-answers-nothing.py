# One error anywhere in a project, standing since the project was opened,
# leaves every file of the project without hover, definition, references
# or types: a failed check keeps nothing, and queries answer only from a
# successful one.
#
# Run: timeout -s KILL 120 python3 design/review-2026-11-16/repro/x-lsp-probe-failed-check-answers-nothing.py
#
# main.gx:  mod util; let a = util::bump(1); let b: string = a; b
#           (the error is on its third line)
# util.gx:  let bump = |x: i64| -> i64 x + 1;
#           let twice = |x: i64| -> i64 bump(bump(x));   (no error)
# Expected: queries in util.gx (and above the error in main.gx) answer from
#   what the check compiled before it failed: hover bump `fn(x: i64) -> i64`.
# Observed (HEAD e1be2fbf2): hover/definition None everywhere, symbols carry
#   no types.
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
    m, u = d + "/main.gx", d + "/util.gx"
    open(m, "w").write("mod util;\nlet a = util::bump(1);\nlet b: string = a;\nb\n")
    open(u, "w").write("let bump = |x: i64| -> i64 x + 1;\nlet twice = |x: i64| -> i64 bump(bump(x));\n")
    c = Client(d)
    c.open(m, open(m).read())
    c.open(u, open(u).read())
    c.request("workspace/symbol", {"query": "\u0000"})
    print("diagnostics:", {k.rsplit("/", 1)[1]: [x["message"] for x in v] for k, v in c.diags.items()})
    print("hover bump in util.gx:", c.hover(u, 1, 30))
    at = {"textDocument": {"uri": uri(u)}, "position": {"line": 1, "character": 30}}
    print("definition bump in util.gx:", c.request("textDocument/definition", at))
    print("hover a in main.gx (above the error):", c.hover(m, 1, 4))
    syms = c.request("textDocument/documentSymbol", {"textDocument": {"uri": uri(u)}})
    print("util.gx symbols:", [(s["name"], s.get("detail")) for s in syms or []])
    c.stop()
