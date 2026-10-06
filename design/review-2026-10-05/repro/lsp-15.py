#!/usr/bin/env python3
# lsp-15: a bare top-level `mod foo dynamic { .. }` links an unrelated
# foo.gx into the script's project, and foo.gx's own errors are never shown.
#
# walk_expr_for_mods (graphix-lsp/src/workspace.rs:128) records a
# ModuleKind::Dynamic name like a `mod foo;`, so bfs_from_root resolves it
# to foo.gx beside the script. foo.gx then is no root: it is checked only
# as part of main.gx, whose check never loads it (a dynamic module's body
# is its `source` at run time). `graphix --check foo.gx` fails; the LSP
# shows nothing. The documented `let status = mod foo dynamic { .. }` form
# is a Bind at top level, which the walk does not see, so it is unaffected.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-15.py <graphix>
#
# expected: foo.gx carries its type error in all three cases.
#
# observed (HEAD c722befe, debug build):
#   bare `mod foo dynamic` in main.gx: foo.gx []
#   `let status = mod foo dynamic`:    foo.gx ['ERROR type mismatch string does not contain i64']
#   no main.gx:                        foo.gx ['ERROR type mismatch string does not contain i64']
#
# The walk also reads only top-level statements, though a `mod util;` inside
# a block checks: util.gx is then a root of its own, and an edit to it that
# breaks main.gx re-checks nothing (both files open, util::bump made to
# return a string).
#
# expected: main.gx reports the arithmetic error in both layouts.
#
# observed:
#   top-level `mod util;`: main.gx ['ERROR cannot compute ...: string + i64 ...']
#   `mod util;` in a block: main.gx []
import json, os, queue, subprocess, sys, tempfile, threading, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"
SEV = {1: "ERROR", 2: "WARNING"}


def uri(path):
    return "file://" + urllib.parse.quote(path, safe="/-._~")


class Client:
    def __init__(self, files):
        self.dir = tempfile.mkdtemp(prefix="lsp-15-")
        for f, t in files.items():
            open(os.path.join(self.dir, f), "w").write(t)
        env = dict(os.environ, XDG_CACHE_HOME=self.dir)
        self.p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
        self.q, self.id, self.standing = queue.Queue(), 0, {}
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
            if m.get("method") == "textDocument/publishDiagnostics":
                f = m["params"]["uri"].rsplit("/", 1)[-1]
                self.standing[f] = [f"{SEV[x['severity']]} {x['message'].splitlines()[0]}"
                                    for x in m["params"]["diagnostics"]]
            if m.get("id") == self.id and "method" not in m:
                return m.get("result")
        raise SystemExit(f"server died during {method}")

    def done(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


dynamic = ('mod foo dynamic {\n    sandbox whitelist [core];\n    sig {\n        val x: i64\n'
           '    };\n    source "let x = 1"\n};\n')
foo = "let y: string = 1;\ny\n"
for label, main in [("bare `mod foo dynamic` in main.gx", dynamic + "foo::x\n"),
                    ("`let status = mod foo dynamic`:   ", "let status = " + dynamic + "foo::x\n"),
                    ("no main.gx:                       ", None)]:
    files = {"foo.gx": foo}
    if main:
        files["main.gx"] = main
    c = Client(files)
    c.notify("textDocument/didOpen", {"textDocument": {
        "uri": uri(os.path.join(c.dir, "foo.gx")), "languageId": "graphix",
        "version": 1, "text": foo}})
    c.request("workspace/symbol", {"query": "zzz"})
    print(label, "foo.gx", c.standing.get("foo.gx", []))
    c.done()


util = "let bump = |n: i64| -> i64 n + 1\n"
for label, main in [("top-level `mod util;`: ", "mod util;\nlet r = util::bump(1) + 1;\nr\n"),
                    ("`mod util;` in a block:", "let r = {\n    mod util;\n    util::bump(1) + 1\n};\nr\n")]:
    c = Client({"main.gx": main, "util.gx": util})
    for name, text in [("main.gx", main), ("util.gx", util)]:
        c.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(os.path.join(c.dir, name)), "languageId": "graphix",
            "version": 1, "text": text}})
    c.request("workspace/symbol", {"query": "zzz"})
    c.notify("textDocument/didChange", {
        "textDocument": {"uri": uri(os.path.join(c.dir, "util.gx")), "version": 2},
        "contentChanges": [{"text": "let bump = |n: i64| -> string \"no\"\n"}]})
    c.request("workspace/symbol", {"query": "zzz"})
    print(label, "main.gx", c.standing.get("main.gx", []))
    c.done()
