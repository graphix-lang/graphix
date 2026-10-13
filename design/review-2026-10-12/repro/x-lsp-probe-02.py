"""x-lsp-probe-02: A script's .gxi is invisible to hover, definition and references.

usage: timeout -s KILL 170 python3 -I design/review-2026-10-12/repro/x-lsp-probe-02.py <path/to/graphix>

Drives `graphix lsp` over stdio with two projects of the same shape:
  module: main.gx `mod util; util::helper(3)`, util.gx + util.gxi
  script: script.gx + script.gxi (the script's own interface)
and a third, the script with a mismatching .gxi, to show the server does
pair the script with its .gxi (the check refuses the mismatch).

expected: the script's .gxi `val` answers hover (type + `doubles` doc) and
definition (-> script.gx 0:4), the script's hover carries the doc, and the
references from script.gx include script.gxi 1:4, as the module's do.
observed (HEAD 7324207f): script hover/definition on the val are None, the
script.gx hover has no doc, references list only script.gx sites; the module
control answers all of them; the mismatch is reported.
"""

import json, os, select, subprocess, sys, tempfile

BIN = sys.argv[1]


def uri(p):
    return "file://" + p


class Client:
    def __init__(self, root):
        self.p = subprocess.Popen([BIN, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        self.buf, self.id, self.diags = b"", 0, {}
        self.request("initialize", {"processId": None, "capabilities": {},
                                    "workspaceFolders": [{"uri": uri(root), "name": "w"}]})
        self.notify("initialized", {})

    def send(self, obj):
        body = json.dumps(obj).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.p.stdin.flush()

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def read(self):
        while True:
            if b"\r\n\r\n" in self.buf:
                head, rest = self.buf.split(b"\r\n\r\n", 1)
                n = int([l for l in head.split(b"\r\n")
                         if l.lower().startswith(b"content-length")][0].split(b":")[1])
                if len(rest) >= n:
                    self.buf = rest[n:]
                    return json.loads(rest[:n])
            r, _, _ = select.select([self.p.stdout], [], [], 120)
            if not r:
                raise SystemExit("timeout")
            self.buf += os.read(self.p.stdout.fileno(), 65536)

    def request(self, method, params):
        self.id += 1
        self.send({"jsonrpc": "2.0", "id": self.id, "method": method, "params": params})
        while True:
            m = self.read()
            if m.get("id") == self.id:
                return m.get("result")
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"].rsplit("/", 1)[-1]] = \
                    [d["message"][:80] for d in m["params"]["diagnostics"]]

    def open(self, path):
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(path), "languageId": "graphix", "version": 1,
            "text": open(path).read()}})

    def at(self, method, path, line, ch, **extra):
        p = {"textDocument": {"uri": uri(path)}, "position": {"line": line, "character": ch}}
        p.update(extra)
        return self.request(method, p)

    def close(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


def project(files):
    root = tempfile.mkdtemp()
    for name, text in files.items():
        with open(os.path.join(root, name), "w") as f:
            f.write(text)
    return root


IMPL = "let helper = |x: i64| x * 2;\n"
SIG = "/// doubles\nval helper: fn(x: i64) -> i64;\n"


def hover(c, f, l, ch):
    h = c.at("textDocument/hover", f, l, ch)
    return h and h["contents"]["value"].replace("\n", " ")


def show(c, label, gx, gxi, use_line, use_ch):
    c.request("workspace/symbol", {"query": "zzqq"})
    print("== %s   diagnostics: %s" % (label, c.diags))
    print("  hover .gxi val      :", hover(c, gxi, 1, 5))
    d = c.at("textDocument/definition", gxi, 1, 5)
    print("  definition .gxi val :", d and (d["uri"].rsplit("/", 1)[-1], d["range"]["start"]))
    print("  hover .gx use       :", hover(c, gx, use_line, use_ch))
    refs = c.at("textDocument/references", gx, 0, 5, context={"includeDeclaration": True}) or []
    print("  references from .gx :",
          [(r["uri"].rsplit("/", 1)[-1], r["range"]["start"]["line"],
            r["range"]["start"]["character"]) for r in refs])


root = project({"main.gx": "mod util;\nutil::helper(3)\n",
                "util.gx": IMPL + "helper(1)\n", "util.gxi": SIG})
c = Client(root)
c.open(root + "/main.gx"); c.open(root + "/util.gx"); c.open(root + "/util.gxi")
show(c, "module control", root + "/util.gx", root + "/util.gxi", 1, 2)
c.close()

root = project({"script.gx": IMPL + "helper(3)\n", "script.gxi": SIG})
c = Client(root)
c.open(root + "/script.gx"); c.open(root + "/script.gxi")
show(c, "script", root + "/script.gx", root + "/script.gxi", 1, 2)
c.close()

root = project({"script.gx": IMPL + "helper(3)\n",
                "script.gxi": "val helper: fn(x: string) -> i64;\n"})
c = Client(root)
c.open(root + "/script.gx"); c.open(root + "/script.gxi")
c.request("workspace/symbol", {"query": "zzqq"})
print("== script with a mismatching .gxi   diagnostics:", c.diags)
c.close()
