"""x-lsp-probe: protocol-level repros for `graphix lsp` over stdio.

usage: python3 x-lsp-probe.py <path/to/graphix> <case> [args]

cases (each prints what it saw and the expected value):
  coalesce            40 queued didChange notifications cost 40 checks
                      (each one publishes), not one: lsp-server's stdio
                      channel is bounded(0), so `receiver.is_empty()` is
                      always true (graphix-lsp/src/server.rs:110).
  open-many <dir>     didOpen every .gx/.gxi under <dir> (a project, e.g.
                      ../netidx/graphix-package-netidx-admin) then hover:
                      one check per open before the first answer.
  script-gxi          a script's .gxi: hover/definition on its `val` answer
                      nothing, references from the script omit it.
  same-line-def       go-to-definition on a reference that shares a line with
                      its declaration jumps to the .gxi `val`.
  completion-scope    completion offers top-level names declared after the
                      cursor (not in scope there).
  lone-gxi            a .gxi with no .gx beside it gets no diagnostics,
                      not even a parse error.
  memory <dir> <file> RSS of the server over 200 checks of <file> in <dir>.
"""

import json, os, queue, select, subprocess, sys, tempfile, threading, time, urllib.parse

BIN = sys.argv[1]
CASE = sys.argv[2]


def uri(p):
    return "file://" + urllib.parse.quote(p)


class Client:
    def __init__(self, root):
        self.p = subprocess.Popen([BIN, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        self.buf, self.id, self.diags, self.npub = b"", 0, {}, 0
        self.wq = queue.Queue()

        def writer():
            while True:
                b = self.wq.get()
                self.p.stdin.write(b)
                self.p.stdin.flush()
        threading.Thread(target=writer, daemon=True).start()
        folders = [{"uri": uri(root), "name": "w"}] if root else None
        self.request("initialize", {"processId": None, "workspaceFolders": folders,
                                    "capabilities": {}})
        self.notify("initialized", {})

    def send(self, obj):
        body = json.dumps(obj).encode()
        self.wq.put(b"Content-Length: %d\r\n\r\n" % len(body) + body)

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def read(self, timeout):
        end = time.time() + timeout
        while True:
            if b"\r\n\r\n" in self.buf:
                head, rest = self.buf.split(b"\r\n\r\n", 1)
                n = int([l for l in head.split(b"\r\n")
                         if l.lower().startswith(b"content-length")][0].split(b":")[1])
                if len(rest) >= n:
                    self.buf = rest[n:]
                    return json.loads(rest[:n])
            r, _, _ = select.select([self.p.stdout], [], [], max(0, end - time.time()))
            if not r:
                return None
            self.buf += os.read(self.p.stdout.fileno(), 65536)

    def request(self, method, params, timeout=150):
        self.id += 1
        self.send({"jsonrpc": "2.0", "id": self.id, "method": method, "params": params})
        while True:
            m = self.read(timeout)
            if m is None:
                return {"TIMEOUT": True}
            if m.get("id") == self.id:
                return m
            if m.get("method") == "textDocument/publishDiagnostics":
                self.npub += 1
                self.diags[m["params"]["uri"]] = m["params"]["diagnostics"]

    def open(self, path, text):
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(path), "languageId": "graphix", "version": 1, "text": text}})

    def change(self, path, text, version):
        self.notify("textDocument/didChange", {
            "textDocument": {"uri": uri(path), "version": version},
            "contentChanges": [{"text": text}]})

    def at(self, method, path, line, ch, **extra):
        p = {"textDocument": {"uri": uri(path)}, "position": {"line": line, "character": ch}}
        p.update(extra)
        return self.request(method, p).get("result")

    def barrier(self):
        return self.request("workspace/symbol", {"query": "zzqq"})

    def close(self):
        self.request("shutdown", None, timeout=20)
        self.notify("exit", None)
        try:
            self.p.wait(timeout=20)
        except subprocess.TimeoutExpired:
            self.p.kill()


def project(files):
    root = tempfile.mkdtemp()
    for name, text in files.items():
        with open(os.path.join(root, name), "w") as f:
            f.write(text)
    return root


if CASE == "coalesce":
    root = project({"main.gx": "let x = 1;\nx\n"})
    c = Client(root)
    m = root + "/main.gx"
    c.open(m, "let x = 1;\nx\n")
    c.barrier()
    before = c.npub
    for v in range(2, 42):
        c.change(m, "let x = 1;\nx + %s\n" % ("q" if v % 2 else "1"), v)
    c.barrier()
    print("publishes for 40 queued edits:", c.npub - before)
    print("expected: 1 (the burst checked once, at its end); observed above")
    c.close()

elif CASE == "open-many":
    root = os.path.abspath(sys.argv[3])
    c = Client(root)
    c.barrier()
    fs = sorted(os.path.join(d, f) for d, _, n in os.walk(root) for f in n
                if f.endswith((".gx", ".gxi")) and "/target/" not in d)
    t0 = time.time()
    for f in fs:
        c.open(f, open(f).read())
    c.at("textDocument/hover", fs[0], 0, 0)
    print(len(fs), "opens; first answer after %.1fs; %d publishes" % (time.time() - t0, c.npub))
    c.close()

elif CASE == "script-gxi":
    root = project({"script.gx": "let helper = |x: i64| x * 2;\nhelper(3)\n",
                    "script.gxi": "/// doubles\nval helper: fn(x: i64) -> i64;\n"})
    c = Client(root)
    s, si = root + "/script.gx", root + "/script.gxi"
    c.open(s, open(s).read())
    c.open(si, open(si).read())
    c.barrier()
    print("diagnostics:", {k.rsplit('/', 1)[-1]: v for k, v in c.diags.items()})
    print("hover script.gxi `helper`:", c.at("textDocument/hover", si, 1, 5))
    print("definition script.gxi `helper`:", c.at("textDocument/definition", si, 1, 5))
    print("hover script.gx `helper` (no `doubles` doc):", c.at("textDocument/hover", s, 1, 2))
    refs = c.at("textDocument/references", s, 0, 5, context={"includeDeclaration": True})
    print("references from script.gx:", [(r["uri"].rsplit('/', 1)[-1], r["range"]["start"]) for r in refs or []])
    print("expected: the val answers as a module's .gxi val does, and is among the references")
    c.close()

elif CASE == "same-line-def":
    util = "let bump = |n: i64| -> i64 n + 1; let twice = |n: i64| -> i64 bump(bump(n));\n"
    root = project({"main.gx": "mod util;\nutil::twice(1)\n", "util.gx": util,
                    "util.gxi": "val bump: fn(n: i64) -> i64;\nval twice: fn(n: i64) -> i64;\n"})
    c = Client(root)
    u = root + "/util.gx"
    c.open(u, util)
    c.barrier()
    d = c.at("textDocument/definition", u, 0, util.index("bump(bump") + 1)
    print("definition of the reference `bump(` ->", d["uri"].rsplit('/', 1)[-1], d["range"]["start"])
    print("expected: util.gx 0:4 (the declaration), as when the reference is on its own line")
    c.close()

elif CASE == "completion-scope":
    text = "let f = |a: i64| a + 1;\nlet g = |b: i64| b + 1;\nlet later = 1;\ng(later)\n"
    root = project({"main.gx": text})
    c = Client(root)
    m = root + "/main.gx"
    c.open(m, text)
    c.barrier()
    items = c.at("textDocument/completion", m, 0, len("let f = |a: i64| a + ")) or []
    names = [i["label"] for i in items if i["label"] in ("a", "b", "f", "g", "later")]
    print("completion inside f's body offers:", names)
    print("expected: ['a'] (f, g and later are not in scope in f's non-rec body)")
    c.close()

elif CASE == "lone-gxi":
    root = project({"api.gxi": "val x: i64 = ;\n"})
    c = Client(root)
    c.open(root + "/api.gxi", "val x: i64 = ;\n")
    c.barrier()
    print("diagnostics:", c.diags)
    print("expected: the parse error at 0:11")
    c.close()

elif CASE == "memory":
    root, f = os.path.abspath(sys.argv[3]), os.path.abspath(sys.argv[4])
    c = Client(root)
    text = open(f).read()
    c.open(f, text)
    c.barrier()

    def rss():
        for l in open("/proc/%d/status" % c.p.pid):
            if l.startswith("VmRSS"):
                return int(l.split()[1])
    for i in range(1, 201):
        c.change(f, text + "\n" * (i % 3), i + 1)
        if i % 40 == 0:
            c.barrier()
            print(i, "checks, RSS kB", rss())
    c.close()
