# A type an interface declares and the typedef implementing it are not one
# symbol: in a module the implementation's `type Counter = ..` answers
# nothing, and in a script the interface's `type Counter;` and every type
# its vals name answer nothing.
#
# Run: timeout -s KILL 120 python3 design/review-2026-11-16/repro/x-lsp-probe-interface-type-unlinked.py
#
# Expected: hover/definition/references answer on every `Counter`, and the
#   references list both declarations, as they do for a `val` and its `let`.
# Observed (HEAD e1be2fbf2):
#   module ctr.gx `type |Counter = Abstract<i64>`: hover None, definition None,
#     references []; references from ctr.gxi omit ctr.gx 0:5.
#   script app.gxi `type |Counter;` and `-> |Counter`: hover None, definition
#     None, references []; references from app.gx omit every app.gxi site.
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


IMPL = "type Counter = Abstract<i64>;\nlet make = |n: i64| -> Counter Counter(n);\nlet get = |c: Counter| -> i64 c.0;\n"
SIG = "/// a counter\ntype Counter;\nval make: fn(n: i64) -> Counter;\nval get: fn(c: Counter) -> i64;\n"


def ask(c, label, path, line, ch):
    at = {"textDocument": {"uri": uri(path)}, "position": {"line": line, "character": ch}}
    print(label, "hover:", c.hover(path, line, ch))
    d = c.request("textDocument/definition", at)
    print(label, "definition:", d and (d["uri"].rsplit("/", 1)[1], d["range"]["start"]))
    r = c.request("textDocument/references", dict(at, context={"includeDeclaration": True}))
    print(label, "references:", sorted((l["uri"].rsplit("/", 1)[1], l["range"]["start"]["line"],
                                        l["range"]["start"]["character"]) for l in r or []))


with tempfile.TemporaryDirectory() as d:
    open(d + "/main.gx", "w").write("mod ctr;\nlet c: ctr::Counter = ctr::make(3);\nctr::get(c)\n")
    open(d + "/ctr.gx", "w").write(IMPL)
    open(d + "/ctr.gxi", "w").write(SIG)
    os.makedirs(d + "/s")
    open(d + "/s/app.gx", "w").write(IMPL + "get(make(3))\n")
    open(d + "/s/app.gxi", "w").write(SIG)
    c = Client(d)
    for f in ("main.gx", "ctr.gx", "ctr.gxi", "s/app.gx", "s/app.gxi"):
        c.open(d + "/" + f, open(d + "/" + f).read())
    print("module:")
    ask(c, "  ctr.gx  type |Counter =", d + "/ctr.gx", 0, 5)
    ask(c, "  ctr.gxi type |Counter;", d + "/ctr.gxi", 1, 5)
    print("script:")
    ask(c, "  app.gx  type |Counter =", d + "/s/app.gx", 0, 5)
    ask(c, "  app.gxi type |Counter;", d + "/s/app.gxi", 1, 5)
    ask(c, "  app.gxi -> |Counter", d + "/s/app.gxi", 2, 24)
    c.stop()
