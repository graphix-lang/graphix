#!/usr/bin/env python3
# lsp-09: a shebang script is accepted by the runtime and `--check` but
# not by the formatter, the LSP's project scan or its document symbols.
#
# Only RootFile::load (graphix-types/src/expr/resolver.rs:537-540) strips
# a leading `#!` line. workspace::extract_mod_decls
# (graphix-lsp/src/workspace.rs:106), symbols::declared
# (graphix-lsp/src/symbols.rs:53) and format::format_source (`graphix fmt`
# and textDocument/formatting) parse the raw text and fail at the `#`. The
# scan records no `mod` edges for the script, so its modules become
# project roots of their own: they are checked alone, and an edit to one
# never re-checks the script.
#
# Two projects, identical except for main.gx's first line:
#   main.gx: [#!/usr/bin/env graphix]  let cfg = 1; mod util; let top = util::g; top
#   util.gx: use super::cfg; let g = cfg + 1;
# Steps: fmt --stdout main.gx; --check main.gx after breaking util.gx
# (`cfg + "s"`); then over `graphix lsp`: open main.gx, documentSymbol,
# formatting, open util.gx, break util.gx.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-09.py <graphix>
#
# expected: the shebang project behaves like the control: fmt formats it,
# main.gx has symbols, util.gx is checked under main.gx (no error when
# opened, the i64 + string error once broken, as `--check` reports).
#
# observed (HEAD c722befe, debug build):
#   == shebang
#   fmt: exit 1: Unexpected `#`
#   --check (util broken): exit 1: 5: cannot compute i64 + string: arithmetic is fn('a: Number,
#   lsp open main     {}
#   lsp symbols main  None
#   lsp format main   None
#   lsp open util     {'util.gx': ['`super` goes above the package root']}
#   lsp break util    {'util.gx': ['`super` goes above the package root']}
#   == control
#   fmt: exit 0
#   --check (util broken): exit 1: 5: cannot compute i64 + string: arithmetic is fn('a: Number,
#   lsp open main     {}
#   lsp symbols main  ['cfg', 'util', 'top']
#   lsp format main   0 edit(s)
#   lsp open util     {}
#   lsp break util    {'util.gx': ["cannot compute i64 + string: arithmetic is fn('a: Number, 'a"]}
import json, os, queue, subprocess, sys, tempfile, threading, time, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"
BODY = "let cfg = 1;\nmod util;\nlet top = util::g;\ntop\n"
UTIL = "use super::cfg;\nlet g = cfg + 1;\n"
BROKEN = 'use super::cfg;\nlet g = cfg + "s";\n'


def uri(path):
    return "file://" + urllib.parse.quote(path, safe="/-._~")


class Client:
    def __init__(self, root):
        self.p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        self.q, self.diags, self.id = queue.Queue(), {}, 0
        threading.Thread(target=self.reader, daemon=True).start()
        self.request("initialize", {"processId": None, "rootUri": None,
                                    "capabilities": {},
                                    "workspaceFolders": [{"uri": uri(root), "name": "w"}]})
        self.notify("initialized", {})

    def reader(self):
        f = self.p.stdout
        while True:
            line = f.readline()
            if not line:
                return self.q.put(None)
            if line.lower().startswith(b"content-length:"):
                n = int(line.split(b":")[1])
                while f.readline() not in (b"\r\n", b"\n", b""):
                    pass
                self.q.put(json.loads(f.read(n)))

    def send(self, msg):
        b = json.dumps(dict(msg, jsonrpc="2.0")).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
        self.p.stdin.flush()

    def notify(self, method, params):
        self.send({"method": method, "params": params})

    def request(self, method, params):
        self.id += 1
        self.send({"id": self.id, "method": method, "params": params})
        while True:
            m = self.q.get(timeout=150)
            if m is None:
                raise EOFError(method)
            if m.get("method") == "textDocument/publishDiagnostics":
                self.diags[m["params"]["uri"]] = m["params"]["diagnostics"]
            if m.get("id") == self.id and "method" not in m:
                return m.get("result")

    def sync(self):
        self.request("workspace/symbol", {"query": "\u0000"})

    def open(self, u, text):
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": u, "languageId": "graphix", "version": 1, "text": text}})
        self.sync()

    def change(self, u, text):
        self.notify("textDocument/didChange", {"textDocument": {"uri": u, "version": 2},
                                               "contentChanges": [{"text": text}]})
        self.sync()

    def stands(self):
        return {k.rsplit("/", 1)[-1]: [d["message"][:60] for d in v]
                for k, v in self.diags.items() if v}

    def shutdown(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


def first_error(out):
    for l in out.splitlines():
        if "Unexpected" in l or "cannot compute" in l:
            return l.strip()[:60]
    return out.strip().splitlines()[-1][:60] if out.strip() else ""


def project(name, main):
    root = tempfile.mkdtemp(prefix=f"lsp-09-{name}-")
    for f, t in (("main.gx", main), ("util.gx", UTIL)):
        open(os.path.join(root, f), "w").write(t)
    return root


def run(name, main):
    print("==", name)
    root = project(name, main)
    mf = os.path.join(root, "main.gx")
    r = subprocess.run([GX, "fmt", "--stdout", mf], capture_output=True, text=True)
    print(f"fmt: exit {r.returncode}" + (f": {first_error(r.stderr)}" if r.returncode else ""))
    broken = project(name + "-broken", main)
    open(os.path.join(broken, "util.gx"), "w").write(BROKEN)
    r = subprocess.run([GX, "--check", "--no-cache", os.path.join(broken, "main.gx")],
                       capture_output=True, text=True)
    print(f"--check (util broken): exit {r.returncode}: {first_error(r.stderr + r.stdout)}")
    c = Client(root)
    um, uu = uri(mf), uri(os.path.join(root, "util.gx"))
    c.open(um, main)
    print("lsp open main    ", c.stands())
    syms = c.request("textDocument/documentSymbol", {"textDocument": {"uri": um}})
    print("lsp symbols main ", syms if syms is None else [s["name"] for s in syms])
    fmt = c.request("textDocument/formatting", {"textDocument": {"uri": um},
                                                "options": {"tabSize": 4, "insertSpaces": True}})
    print("lsp format main  ", fmt if fmt is None else f"{len(fmt)} edit(s)")
    c.open(uu, UTIL)
    print("lsp open util    ", c.stands())
    c.change(uu, BROKEN)
    print("lsp break util   ", c.stands())
    c.shutdown()


run("shebang", "#!/usr/bin/env graphix\n" + BODY)
run("control", BODY)
