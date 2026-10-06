#!/usr/bin/env python3
# lsp-05: a .gxi outside every project is checked as a program and gets a
# bogus parse error.
#
# `roots_of` (graphix-lsp/src/state.rs:114) makes any open file no project
# contains its own root, a `.gxi` included, and `check` hands that root to
# `typecheck_project`, which loads it through `RootFile::load`: an
# interface parsed as a program. `detect_package_scope` (workspace.rs:229)
# tests only the `mod` stem, so a package's `mod.gxi` takes the same path.
# A .gxi is outside every project whenever the client names no workspace
# (single-file mode), and, in a workspace, from its creation until a save
# rescans the disk. Once a save puts it into its project the error never
# goes away (the former-root leak, lsp-04).
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-05.py <graphix>
#
# expected: no diagnostics on any .gxi below (every file is valid:
#   `graphix --check main.gx` exits 0), and hover in api.gxi answers.
# observed (HEAD c722befe, debug build):
#   graphix --check main.gx: exit 0
#   1 no workspace, api.gxi open        api.gxi: ['Parse error at line: 1, column: 1 ..
#       note: .. `///` is a doc comment, legal only in a .gxi interface file; ..']
#     hover on api.gxi `double`: None
#   2 no workspace, api.gx open         api.gx: []   (a .gx root pairs its .gxi)
#   3 no workspace, package mod.gxi     mod.gxi: ['Parse error at line: 2, column: 5 ..']
#   4 workspace, new api.gxi unsaved    api.gxi: ['Parse error at line: 1, column: 1 ..']
#   5 .. api.gxi saved, main edited     api.gxi: ['Parse error at line: 1, column: 1 ..']
#   6 .. api.gxi closed                 api.gxi: ['Parse error at line: 1, column: 1 ..']
import atexit, json, os, queue, shutil, subprocess, sys, tempfile, threading, urllib.parse

graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
tmp = os.path.realpath(tempfile.mkdtemp(prefix="lsp-05."))
atexit.register(shutil.rmtree, tmp, True)
env = dict(os.environ, XDG_CACHE_HOME=os.path.join(tmp, "cache"))
MAIN = "mod api;\napi::double(2)\n"
API_GX = "let double = |n: i64| -> i64 n * 2;\n"
API_GXI = "/// twice n\nval double: fn(n: i64) -> i64;\n"
PKG_GX = "let flip = |d: Dir| -> Dir select d { `Up => `Down, `Down => `Up };\n"
PKG_GXI = "type Dir = [`Up, `Down];\nval flip: fn(d: Dir) -> Dir;\n"


def write(path, text):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    open(path, "w").write(text)


def uri(path):
    return "file://" + urllib.parse.quote(path)


class Server:
    def __init__(self, folder):
        self.proc = subprocess.Popen([graphix, "lsp"], env=env, stdin=subprocess.PIPE,
                                     stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        atexit.register(self.proc.kill)
        self.inbox, self.standing, self.next_id = queue.Queue(), {}, 0
        threading.Thread(target=self.reader, daemon=True).start()
        folders = None if folder is None else [{"uri": uri(folder), "name": "w"}]
        self.request("initialize", {"processId": None, "rootUri": None,
                                    "capabilities": {}, "workspaceFolders": folders})
        self.notify("initialized", {})

    def reader(self):
        f = self.proc.stdout
        while True:
            line = f.readline()
            if not line:
                self.inbox.put(None)
                return
            if line.lower().startswith(b"content-length:"):
                n = int(line.split(b":")[1])
                while f.readline() not in (b"\r\n", b"\n", b""):
                    pass
                self.inbox.put(json.loads(f.read(n)))

    def send(self, msg):
        b = json.dumps(dict(msg, jsonrpc="2.0")).encode()
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
        self.proc.stdin.flush()

    def notify(self, method, params):
        self.send({"method": method, "params": params})

    def request(self, method, params):
        self.next_id += 1
        rid = self.next_id
        self.send({"id": rid, "method": method, "params": params})
        while True:
            m = self.inbox.get(timeout=150)
            if m is None:
                sys.exit("the server died")
            if m.get("method") == "textDocument/publishDiagnostics":
                p = m["params"]
                name = p["uri"].rsplit("/", 1)[-1]
                self.standing[name] = [d["message"] for d in p["diagnostics"]]
            if m.get("id") == rid and "method" not in m:
                return m.get("result")

    def open(self, path, text):
        self.notify("textDocument/didOpen", {"textDocument": {
            "uri": uri(path), "languageId": "graphix", "version": 1, "text": text}})

    def show(self, step, name):
        # the server checks what is dirty before it answers a request
        self.request("workspace/symbol", {"query": "\u0000"})
        msgs = self.standing.get(name, [])
        short = [m.splitlines()[0] + "".join(" .. " + l.strip() for l in m.splitlines()
                                             if "note" in l) for m in msgs]
        print(f"{step:<36} {name}: {short}")

    def stop(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.proc.wait(timeout=20)


# a valid program: main.gx uses module api, implemented by api.gx, declared by api.gxi
a = os.path.join(tmp, "a")
write(os.path.join(a, "main.gx"), MAIN)
write(os.path.join(a, "api.gx"), API_GX)
write(os.path.join(a, "api.gxi"), API_GXI)
check = subprocess.run([graphix, "--check", os.path.join(a, "main.gx")], env=env,
                       capture_output=True, text=True, timeout=120)
print("graphix --check main.gx: exit", check.returncode, check.stderr.strip()[-200:])

s = Server(None)
s.open(os.path.join(a, "api.gxi"), API_GXI)
s.show("1 no workspace, api.gxi open", "api.gxi")
h = s.request("textDocument/hover", {"textDocument": {"uri": uri(os.path.join(a, "api.gxi"))},
                                     "position": {"line": 1, "character": 5}})
print("  hover on api.gxi `double`:", h)
s.stop()

s = Server(None)
s.open(os.path.join(a, "api.gx"), API_GX)
s.show("2 no workspace, api.gx open", "api.gx")
s.stop()

crate = os.path.join(tmp, "graphix-package-demo")
write(os.path.join(crate, "Cargo.toml"), "[package]\nname = \"graphix-package-demo\"\n")
write(os.path.join(crate, "src", "graphix", "mod.gx"), PKG_GX)
write(os.path.join(crate, "src", "graphix", "mod.gxi"), PKG_GXI)
s = Server(None)
s.open(os.path.join(crate, "src", "graphix", "mod.gxi"), PKG_GXI)
s.show("3 no workspace, package mod.gxi", "mod.gxi")
s.stop()

# a workspace where the interface is written after the server started
b = os.path.join(tmp, "b")
write(os.path.join(b, "main.gx"), MAIN)
write(os.path.join(b, "api.gx"), API_GX)
s = Server(b)
s.open(os.path.join(b, "main.gx"), MAIN)
s.open(os.path.join(b, "api.gxi"), API_GXI)
s.show("4 workspace, new api.gxi unsaved", "api.gxi")
write(os.path.join(b, "api.gxi"), API_GXI)
s.notify("textDocument/didSave", {"textDocument": {"uri": uri(os.path.join(b, "api.gxi"))}})
s.notify("textDocument/didChange", {
    "textDocument": {"uri": uri(os.path.join(b, "main.gx")), "version": 2},
    "contentChanges": [{"text": "mod api;\napi::double(3)\n"}]})
s.show("5 .. api.gxi saved, main edited", "api.gxi")
s.notify("textDocument/didClose", {"textDocument": {"uri": uri(os.path.join(b, "api.gxi"))}})
s.show("6 .. api.gxi closed", "api.gxi")
s.stop()
